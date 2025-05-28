data_dir <- file.path(getwd(), "data")

require(sf)
require(dplyr)

nyc_zip <- st_read("data/ZIP_CODE_040114/ZIP_CODE_040114.shp")

str(nyc_zip)

st_crs(nyc_zip)

mapview(nyc_zip)

nys_health <- read_csv("data/NYS_Health_Facility.csv", show_col_types = FALSE, lazy = FALSE)

str(nys_health)

# Drop the rows without coordinates
missing_coords <- is.na(nys_health$`Facility Longitude`) | is.na(nys_health$`Facility Latitude`)
sum(missing_coords)

nys_health <- nys_health[!missing_coords, ]

# Drop the rows where coordinates are 0 
nys_health <- nys_health %>%
  filter(!(nys_health$`Facility Longitude` == 0 & nys_health$`Facility Latitude` == 0))

# Swap Longtitude and Latitude for the row with false coordinates 
nys_health <- nys_health %>%
  mutate(
    temp_lon = `Facility Longitude`,
    `Facility Longitude` = ifelse(`Facility ID` == 5764, `Facility Latitude`, `Facility Longitude`),
    `Facility Latitude` = ifelse(`Facility ID` == 5764, temp_lon, `Facility Latitude`)
  ) %>%
  select(-temp_lon)

nys_health_sf <- st_as_sf(nys_health, coords = c("Facility Longitude", "Facility Latitude"))

str(nys_health_sf)

st_crs(nys_health_sf) <- 4326

st_crs(nys_health_sf)

mapview(nys_health_sf)

nys_retail_food <- read_csv("data/nys_retail_food_store_xy.csv", show_col_types = FALSE, lazy = FALSE) 

str(nys_retail_food)

missing_coords <- is.na(nys_retail_food$`X`) | is.na(nys_retail_food$`Y`)
sum(missing_coords)

nys_retail_food <- nys_retail_food[!missing_coords, ]

nys_retail_food_sf <- st_as_sf(nys_retail_food, coords = c("X", "Y"))

st_crs(nys_retail_food_sf)

st_crs(nys_retail_food_sf) <- 4326

mapview(nys_retail_food_sf)


save(nyc_zip, nys_health_sf, nys_retail_food_sf, file = "nyc.RData")

st_write(nyc_zip, "nyc.gpkg", layer = "nyc_zip", append = FALSE)
st_write(nys_health_sf, "nyc.gpkg", layer = "nys_health_sf", append = TRUE)
st_write(nys_retail_food_sf, "nyc.gpkg", layer = "nys_retail_food_sf", append = TRUE)
