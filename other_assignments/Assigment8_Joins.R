# Load required packages
require(tidyverse)
require(sf) 
require(mapview) 
require(magrittr)

# Task 1
nyc_zip <- st_read("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/ZIP_CODE_040114/ZIP_CODE_040114.shp")
covid_data <- readr::read_csv("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/R-Spatial_II_Lab/tests-by-zcta_2021_04_23.csv", lazy = FALSE)
nyc_merged <- base::merge(nyc_zip, covid_data, by.x = "ZIPCODE", by.y = "MODIFIED_ZCTA")
names(nyc_merged)
mapview(nyc_merged, zcol="COVID_DEATH_COUNT")

# Task 2
nyc_food <- st_read("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/R-Spatial_II_Lab/nycFoodStore.shp")
nyc_food <- st_transform(nyc_food, st_crs(nyc_zip))

food_counts <- nyc_food %>% 
  dplyr::filter(stringr::str_detect(Estbl_T, '[AJD]')) %>%
  sf::st_join(nyc_zip, ., join = st_contains) %>%
  group_by(Zip_Cod) %>%
  summarise(FoodStoreNum = n()) %>%
  mutate(Zip_Cod = as.character(Zip_Cod)) 

nyc_merged <- nyc_merged %>%
  left_join(st_drop_geometry(food_counts), by = c("ZIPCODE" = "Zip_Cod"))

names(nyc_merged)
plot(nyc_merged["FoodStoreNum"], breaks = "jenks", main = "Number of Food Stores")

# Task 3
nys_health <- read_csv("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/NYS_Health_Facility.csv", 
                       show_col_types = FALSE, lazy = FALSE)

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
st_crs(nys_health_sf) <- 4326

healthfacilities_count <- nyc_merged %>% 
  mutate(ziparea = st_area(geometry)) %>% 
  st_transform(4326) %>%
  st_join(nys_health_sf) %>%
  group_by(`Facility Zip Code`) %>% 
  summarize(healthfacilities_NUM = n(),
            ziparea = max(ziparea),
            rate = healthfacilities_NUM/ziparea * 1e6) 

mapview(healthfacilities_count, zcol='healthfacilities_NUM', legend=FALSE)

nyc_merged <- nyc_merged %>%
  left_join(
    healthfacilities_count %>% 
      st_drop_geometry(), 
    by = c("ZIPCODE" = "Facility Zip Code")
  )

# Task 4
nycCensus <- sf::st_read('/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/R-Spatial_II_Lab/2010 Census Tracts/geo_export_1dc7b645-647b-4806-b9a0-7b79660f120a.shp', stringsAsFactors = FALSE)
str(nycCensus)

nycCensus %<>% dplyr::mutate(cntyFIPS = case_when(
  boro_name == 'Bronx' ~ '005',
  boro_name == 'Brooklyn' ~ '047',
  boro_name == 'Manhattan' ~ '061',
  boro_name == 'Queens' ~ '081',
  boro_name == 'Staten Island' ~ '085'),
  tractFIPS = paste(cntyFIPS, ct2010, sep='')
)

acsData <- readLines("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/R-spatial/data/R-Spatial_II_Lab/ACSDP5Y2018.DP05_data_with_overlays_2020-04-22T132935.csv") %>%
  magrittr::extract(-2) %>% 
  textConnection() %>%
  read.csv(header=TRUE, quote= "\"") %>%
  dplyr::select(GEO_ID, 
                totPop = DP05_0001E, elderlyPop = DP05_0024E, # >= 65
                malePop = DP05_0002E, femalePop = DP05_0003E,  
                whitePop = DP05_0037E, blackPop = DP05_0038E,
                asianPop = DP05_0067E, hispanicPop = DP05_0071E,
                adultPop = DP05_0021E, citizenAdult = DP05_0087E) %>%
  dplyr::mutate(censusCode = stringr::str_sub(GEO_ID, -9,-1))

popData <- merge(nycCensus, acsData, by.x ='tractFIPS', by.y = 'censusCode')

# verify the data
sum(popData$totPop)
st_crs(popData)
popNYC <- sf::st_transform(popData, st_crs(nyc_merged)) 
popData %>% magrittr::extract('elderlyPop') %>% plot(breaks = 'jenks')
names(popNYC)

# Task 5
covidPopZipNYC <- sf::st_join(nyc_merged, 
                              popNYC %>% sf::st_centroid(),
                              join = st_contains) %>% 
  group_by(ZIPCODE, PO_NAME, POPULATION, COUNTY,NEIGHBORHOOD_NAME, COVID_CASE_COUNT, TOTAL_COVID_TESTS, COVID_DEATH_COUNT, 
           FoodStoreNum, healthfacilities_NUM
  ) %>% 
  summarise(totPop = sum(totPop),
            adultPop = sum(adultPop),
            malePctg = sum(malePop)/totPop*100,
            femalePop = sum(femalePop)/totPop*100,
            elderlyPop = sum(elderlyPop),
            asianPop = sum(asianPop),
            blackPop = sum(blackPop),
            hispanicPop = sum(hispanicPop),
            whitePop = sum(whitePop)) 

covidPopZipNYC %>% head()
sum(covidPopZipNYC$totPop, na.rm = T)
covidPopZipNYC %>% dplyr::filter(is.na(totPop))
plot(covidPopZipNYC["COVID_CASE_COUNT"], breaks='jenks')
plot(covidPopZipNYC["blackPop"], breaks='jenks')
names(covidPopZipNYC)