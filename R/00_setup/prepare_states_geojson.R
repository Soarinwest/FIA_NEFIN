# prepare_states_geojson.R
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tigris)     # install.packages("tigris")
})
options(tigris_use_cache = TRUE)

# 1) Download cartographic states (generalized) and project to EPSG:5070
st <- states(cb = TRUE, year = 2023, class = "sf") |>
  st_transform(5070) |>
  st_make_valid()

# 2) Keep only what we need; filter to NE states
ne_abbr <- c("VT","NH","ME","MA","CT","RI","NY")
st_ne <- st |>
  filter(STUSPS %in% ne_abbr) |>
  transmute(
    STATEFP,                 # e.g., "50"
    STATECD = as.integer(STATEFP),  # e.g., 50
    STUSPS,
    NAME,
    geometry
  )

# 3) Save for the pipeline
dir.create("data/boundaries", recursive = TRUE, showWarnings = FALSE)
st_write(st_ne, "data/boundaries/states_5070.geojson", delete_dsn = TRUE, quiet = TRUE)