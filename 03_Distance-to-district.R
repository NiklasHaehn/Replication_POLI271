# Unload ollamar if loaded from a prior script in the same session
if ("package:ollamar" %in% search()) detach("package:ollamar", unload = TRUE)

library(tidyverse)
library(haven)
library(sf)
library(geosphere)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ─── Paths ────────────────────────────────────────────────────────────────────

path_input     <- "data/fmt/Dataset_WithDeathplaces.dta"
path_output    <- "data/fmt/Dataset_WithDeathDistance.dta"
path_shp_cache <- "data/raw/jefflewis_shapefiles"   # downloaded once, reused

if (!dir.exists(path_shp_cache)) dir.create(path_shp_cache, recursive = TRUE)

# ─── 1. Load data ─────────────────────────────────────────────────────────────

full <- read_dta(path_input)
cat(sprintf("Total rows: %d | Rows with death coords: %d\n",
            nrow(full), sum(!is.na(full$death_lat))))

# ─── 2. Shapefile loaders ─────────────────────────────────────────────────────

# --- Jeff Lewis (congresses 1–114): download from cdmaps.polisci.ucla.edu ---
# Files are extracted flat into the congress directory (no subfolder).
# Download happens only once per congress; subsequent calls reuse the cache.

load_jefflewis <- function(congress_num) {

  congress_dir <- file.path(path_shp_cache, sprintf("c%03d", congress_num))

  # Check cache first — look for any existing .shp file
  existing <- list.files(congress_dir, pattern = "[.]shp$",
                         recursive = TRUE, full.names = TRUE)

  if (length(existing) == 0) {
    url      <- sprintf("https://cdmaps.polisci.ucla.edu/shp/districts%03d.zip",
                        congress_num)
    zip_dest <- file.path(path_shp_cache,
                          sprintf("districts%03d.zip", congress_num))

    message(sprintf("Congress %d: downloading %s ...", congress_num, url))
    ok <- tryCatch({
      download.file(url, zip_dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message(sprintf("Congress %d: download failed — %s", congress_num, e$message))
      FALSE
    })

    if (!ok || !file.exists(zip_dest)) return(NULL)

    # Guard against a 404 HTML page masquerading as a zip
    if (file.size(zip_dest) < 10000) {
      message(sprintf("Congress %d: downloaded file suspiciously small (%d bytes) — skipping",
                      congress_num, file.size(zip_dest)))
      unlink(zip_dest)
      return(NULL)
    }

    dir.create(congress_dir, showWarnings = FALSE)
    tryCatch(
      unzip(zip_dest, exdir = congress_dir),
      error = function(e) message(sprintf("Congress %d: unzip failed — %s", congress_num, e$message))
    )
    unlink(zip_dest)   # remove zip — shp files remain in cache

    existing <- list.files(congress_dir, pattern = "[.]shp$",
                           recursive = TRUE, full.names = TRUE)
  }

  if (length(existing) == 0) {
    message(sprintf("Congress %d: no .shp file found after extraction", congress_num))
    return(NULL)
  }

  message(sprintf("Congress %d: loading %s", congress_num, basename(existing[1])))
  shp <- tryCatch(
    st_read(existing[1], quiet = TRUE) |>
      st_transform(crs = 4326) |>
      st_make_valid(),          # repair any unclosed rings / invalid geometries
    error = function(e) {
      message(sprintf("Congress %d: st_read failed — %s", congress_num, e$message))
      NULL
    }
  )
  if (is.null(shp)) return(NULL)
  list(shp = shp, source = "jefflewis")
}

# --- tigris (congresses 113–116): post-2010 redistricting ---
congress_to_tigris_year <- c(
  "113" = 2014, "114" = 2016, "115" = 2018, "116" = 2020
)

load_tigris_shp <- function(congress_num) {
  yr  <- congress_to_tigris_year[[as.character(congress_num)]]
  shp <- tryCatch(
    congressional_districts(year = yr, cb = TRUE) |> st_transform(crs = 4326),
    error = function(e) {
      message(sprintf("tigris failed for congress %d: %s", congress_num, e$message))
      NULL
    }
  )
  if (is.null(shp)) return(NULL)
  list(shp = shp, source = "tigris")
}

load_shapefile <- function(congress_num) {
  if (as.character(congress_num) %in% names(congress_to_tigris_year)) {
    return(load_tigris_shp(congress_num))
  }
  load_jefflewis(congress_num)
}

# ─── 3. Helpers ───────────────────────────────────────────────────────────────

# Shared: extract the sf rows for one legislator's represented district
get_district_shp <- function(shp, source, state_abbrev, district) {
  district_int <- as.integer(district)

  if (source == "jefflewis") {
    state_full <- state.name[match(state_abbrev, state.abb)]
    if (is.na(state_full)) return(NULL)
    state_rows <- shp[shp$STATENAME == state_full, ]
    if (nrow(state_rows) == 0) return(NULL)
    if (district_int == 0) return(state_rows)                       # at-large: whole state
    d <- state_rows[as.integer(state_rows$DISTRICT) == district_int, ]
    if (nrow(d) == 0)                                                # fallback: -1 / 0 codes
      d <- state_rows[as.integer(state_rows$DISTRICT) %in% c(0L, -1L), ]
    d

  } else {
    data(fips_codes, package = "tigris", envir = environment())
    fips   <- sprintf("%02d",
               as.integer(unique(fips_codes$state_code[fips_codes$state == state_abbrev])[1]))
    cd_col <- grep("^CD[0-9]+FP$", names(shp), value = TRUE)[1]
    if (is.na(cd_col)) return(NULL)
    shp[shp$STATEFP == fips & shp[[cd_col]] == sprintf("%02d", district_int), ]
  }
}

# Minimum distance (miles) from a point to the district boundary
dist_to_district <- function(death_lon, death_lat, shp, source, state_abbrev, district) {
  d <- get_district_shp(shp, source, state_abbrev, district)
  if (is.null(d) || nrow(d) == 0) return(NA_real_)
  coords <- st_coordinates(d)
  if (nrow(coords) == 0) return(NA_real_)
  min(distGeo(cbind(death_lon, death_lat), coords[, c("X", "Y")]) / 1609.344,
      na.rm = TRUE)
}

# Binary: 1 if the point falls inside the district polygon (point-in-polygon)
# Analogous to the original `binary` variable for birth place.
in_district <- function(death_lon, death_lat, shp, source, state_abbrev, district) {
  d <- get_district_shp(shp, source, state_abbrev, district)
  if (is.null(d) || nrow(d) == 0) return(NA_integer_)
  tryCatch({
    pt     <- st_sfc(st_point(c(death_lon, death_lat)), crs = 4326)
    result <- st_within(pt, st_union(d), sparse = FALSE)
    as.integer(result[1L, 1L])
  }, error = function(e) NA_integer_)
}

# ─── 4. Main loop ─────────────────────────────────────────────────────────────

has_coords <- full |> filter(!is.na(death_lat), !is.na(death_lon))
no_coords  <- full |> filter( is.na(death_lat) |  is.na(death_lon))

congresses <- sort(unique(has_coords$congress))
results    <- vector("list", length(congresses))

for (idx in seq_along(congresses)) {
  k   <- congresses[idx]
  sub <- has_coords |> filter(congress == k)
  cat(sprintf("Congress %d — %d observations\n", k, nrow(sub)))

  shp_obj <- load_shapefile(k)

  if (is.null(shp_obj)) {
    sub$death_dist_miles  <- NA_real_
    sub$death_logged_dist <- NA_real_
    sub$death_binary      <- NA_integer_
    results[[idx]] <- sub
    next
  }

  args <- list(shp = shp_obj$shp, source = shp_obj$source)

  sub$death_dist_miles <- mapply(dist_to_district,
    death_lon = sub$death_lon, death_lat = sub$death_lat,
    state_abbrev = sub$st_name, district = sub$cd,
    MoreArgs = args)

  sub$death_binary <- mapply(in_district,
    death_lon = sub$death_lon, death_lat = sub$death_lat,
    state_abbrev = sub$st_name, district = sub$cd,
    MoreArgs = args)

  sub$death_logged_dist <- log(sub$death_dist_miles + 1)
  results[[idx]] <- sub
}

# ─── 5. Recombine & save ──────────────────────────────────────────────────────

no_coords <- no_coords |>
  mutate(death_dist_miles  = NA_real_,
         death_logged_dist = NA_real_,
         death_binary      = NA_integer_)

output <- bind_rows(bind_rows(results), no_coords) |>
  arrange(congress, icpsr) |>
  # both_binary: 1 if legislator both died AND was born in their district
  mutate(both_binary = if_else(binary == 1L & death_binary == 1L, 1L, 0L))

cat(sprintf("\nDeath-to-district distance non-NA : %d / %d (%.1f%%)\n",
  sum(!is.na(output$death_dist_miles)),  nrow(output),
  100 * mean(!is.na(output$death_dist_miles))))
cat(sprintf("death_binary = 1 (died in district): %d\n",
  sum(output$death_binary == 1L, na.rm = TRUE)))
cat(sprintf("both_binary  = 1 (born & died in district): %d\n",
  sum(output$both_binary  == 1L, na.rm = TRUE)))

output |>
  filter(!is.na(deathplace), deathplace != "") |>
  summarise(share_with_dist = sum(!is.na(death_dist_miles)) / n())

write_dta(output, path_output)
cat("Saved:", path_output, "\n")
