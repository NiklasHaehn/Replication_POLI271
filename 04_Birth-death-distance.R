library(tidyverse)
library(haven)
library(ggmap)
library(geosphere)

# ─── Paths ────────────────────────────────────────────────────────────────────

# Input: output of 03_Distance-to-district.R
path_input        <- "data/fmt/Dataset_WithDeathDistance.dta"
path_output       <- "data/fmt/Dataset_Final.dta"
path_birthgeo_cache <- "data/fmt/birthplace_geocache.csv"

# ─── 1. Load data ─────────────────────────────────────────────────────────────

full <- read_dta(path_input)
cat(sprintf("Total rows: %d\n", nrow(full)))

# ─── 2. Geocode birth places (hometown_final) ─────────────────────────────────

register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))

# Load existing cache or start fresh
if (file.exists(path_birthgeo_cache)) {
  cache <- read_csv(path_birthgeo_cache, show_col_types = FALSE)
  cat(sprintf("Loaded geocache with %d entries.\n", nrow(cache)))
} else {
  cache <- tibble(hometown_final = character(), birth_lon = numeric(), birth_lat = numeric())
}

# Geocode only what isn't cached yet
unique_hometowns <- full |>
  filter(!is.na(hometown_final)) |>
  pull(hometown_final) |>
  unique()

to_geocode <- setdiff(unique_hometowns, cache$hometown_final)

if (length(to_geocode) > 0) {
  cat(sprintf("Geocoding %d new birth places via Google Maps ...\n", length(to_geocode)))
  new_geo <- geocode(to_geocode, output = "latlona", source = "google") |>
    bind_cols(tibble(hometown_final = to_geocode)) |>
    select(hometown_final, birth_lon = lon, birth_lat = lat)
  cache <- bind_rows(cache, new_geo)
  write_csv(cache, path_birthgeo_cache)
  cat(sprintf("Cache updated: %d total entries saved.\n", nrow(cache)))
} else {
  cat("All birth places already in cache — no API calls made.\n")
}

# ─── 3. Join birth coordinates & compute birth-to-death distance ──────────────

full <- left_join(full, cache, by = "hometown_final") |>
  mutate(
    birth_death_dist_miles  = if_else(
      !is.na(birth_lat) & !is.na(birth_lon) & !is.na(death_lat) & !is.na(death_lon),
      distGeo(cbind(birth_lon, birth_lat), cbind(death_lon, death_lat)) / 1609.344,
      NA_real_
    ),
    birth_death_logged_dist = log(birth_death_dist_miles + 1)
  )

cat(sprintf(
  "Birth-to-death distance non-NA: %d / %d (%.1f%%)\n",
  sum(!is.na(full$birth_death_dist_miles)),
  nrow(full),
  100 * mean(!is.na(full$birth_death_dist_miles))
))

# ─── 4. Save ──────────────────────────────────────────────────────────────────

write_dta(full, path_output)
cat("Saved:", path_output, "\n")
