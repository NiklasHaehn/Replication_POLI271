library(tidyverse)
library(ggmap)
library(haven)

# ─── Paths ───────────────────────────────────────────────────────────────────

path_deathplaces  <- "data/fmt/MP_deathplaces_NH.csv"
path_main_dataset <- "Replication_from_dataverse/Dataset_ForMainAnalysis.dta"
path_output       <- "data/fmt/Dataset_WithDeathplaces.dta"

# ─── 1. Load & clean death places ────────────────────────────────────────────

deathplaces <- read_csv(path_deathplaces, show_col_types = FALSE) |>
  rename(bioguide_id = id_bioguide,
         deathplace  = Death_Place_llama3) |>
  # Keep only rows where the LLM successfully extracted a place
  filter(!is.na(deathplace),
         is.na(Death_Place_error) | Death_Place_error == "NA") |>
  select(bioguide_id, deathplace)

# ─── 2. Geocode unique death place strings (same logic as DataCreation_01) ───

# API key is stored in ~/.Rprofile as GOOGLE_MAPS_API_KEY
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))

unique_places <- unique(deathplaces$deathplace)

latlong <- geocode(unique_places, output = "latlona", source = "google")
latlong <- bind_cols(
  tibble(deathplace = unique_places),
  latlong |> select(lon, lat)
)

# ─── 3. Join coordinates back to death places ─────────────────────────────────

deathplaces <- left_join(deathplaces, latlong, by = "deathplace") |>
  rename(death_lon = lon,
         death_lat = lat)

# ─── 4. Load main analysis dataset ───────────────────────────────────────────

main <- read_dta(path_main_dataset)

# ─── 5. Merge death places into main dataset ──────────────────────────────────

# Dataset_ForMainAnalysis has one row per legislator-congress; death place is
# a legislator-level attribute, so we join on bioguide_id alone.
main <- left_join(main, deathplaces, by = "bioguide_id")

cat(sprintf(
  "Rows in main dataset : %d\n",
  nrow(main)
))
cat(sprintf(
  "Rows with death place : %d (%.1f%%)\n",
  sum(!is.na(main$deathplace)),
  100 * mean(!is.na(main$deathplace))
))

# ─── 6. Save ─────────────────────────────────────────────────────────────────

write_dta(main, path_output)
cat("Saved:", path_output, "\n")
