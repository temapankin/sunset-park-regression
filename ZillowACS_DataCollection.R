
# Set the directory 
setwd("/Users/artyom/Documents/study/hunter/25S-GTECH385_DataAnalysisViz/sunset-park-regression")
getwd()


# List of required packages
packages <- c(
  "tidyverse",   # Data wrangling & cleaning
  "plm",         # Fixed effects and DiD regression
  "fixest",      # Econometric modeling
  "sf",          # Spatial data
  "tidycensus",  # U.S. Census
  "leaflet",     # Web Maps
  "tmap",        # Maps
  "janitor",     # Cleaning messy data tables
  "lubridate",   # Data Cleaning
  "zoo",         # Time-series data
  "dplyr",       # DataFrames
  "ggplot2",     # Vizualization
  "Hmisc"        # Vizualization
)

# Install missing packages
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# Read raw CSV data
zillow_raw <- read.csv("Zip_zori_uc_sfrcondomfr_sm_month.csv")

str(zillow_raw)

colnames(zillow_raw)

# Pivot table 
zillow_long <- zillow_raw %>%
  pivot_longer(
    cols = matches("^X\\d{4}\\.\\d{2}\\.\\d{2}$"),
    names_to = "date",
    values_to = "rent_zori"
  ) %>%
  mutate( # create requried columns 
    date = ymd(str_remove(date, "^X")),  
    zip_code = as.character(RegionName),
    treatment = if_else(zip_code %in% c("11220", "11232"), 1, 0),
    post = if_else(date >= ymd("2024-03-01"), 1, 0),
    treatmentpost = treatment*post
  ) %>% 
  filter(
    zip_code %in% c("11220", "11232", "11237", "11221"), # Filter to only have treatment and control neighborhoods
    date >= ymd("2018-01-01") #Filter for the period of interest
  )

# Select required columns
zillow_clean <- zillow_long %>%
  select(zip_code, date, rent_zori, treatment, post, treatmentpost)

# Check for the missing values
sum(is.na(zillow_clean$rent_zori))

zillow_clean <- zillow_clean %>%
  filter(date > as.Date("2020-03-31"))

# Check for the missing values
sum(is.na(zillow_clean$rent_zori))

# Interpolate missing values 

# zillow_clean <- zillow_clean %>%
#   mutate(date = as.Date(date)) %>%
#   arrange(zip_code, date) %>%
#   group_by(zip_code) %>%
#   mutate(
#     rent_zori = na.approx(rent_zori, x = as.numeric(date), na.rm = FALSE)
#   ) %>%
#   ungroup()

# sum(is.na(zillow_clean$rent_zori))

# Uploading the ACS data using API
census_api_key("4a886444c4703abdc2217207ea3acbca8fc9976e",  install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

zips <- c("11220", "11232", "11237", "11221")

vars <- c(
  median_income = "B19013_001",    # Median household income
  renters = "B25003_003",          # Renter-occupied units
  housing_units = "B25003_001",    # Total housing units
  total_pop = "B01003_001",        # Total population
  white = "B02001_002",            # White Pop
  black = "B02001_003",            # Black Pop
  asian = "B02001_005",            # Asian Pop
  hispanic = "B03003_003"          # Hispanic Pop
)

# Get data
acs_data <- get_acs(
  geography = "zip code tabulation area",
  variables = vars,
  cache = TRUE,
  year = 2022,
  survey = "acs5",
  output = "wide",
  cache_table = TRUE
) %>%
  filter(GEOID %in% zips)

# Calculate variables
acs_clean <- acs_data %>%
  mutate(
    zip_code = GEOID, 
    median_income = median_incomeE,
    pct_renters = rentersE / housing_unitsE,
    total_pop = total_popE,
    pct_white = whiteE / total_popE,
    pct_black = blackE / total_popE,
    pct_asian = asianE / total_popE,
    pct_hispanic = hispanicE / total_popE
  ) %>%
  select(zip_code, median_income,pct_renters,total_pop, pct_white, pct_black, pct_asian, pct_hispanic)

# Join to the rent data to create a panel df 
panel_data <- zillow_clean %>%
  left_join(acs_clean, by = "zip_code")

str(panel_data)


# Filter only numeric columns
numeric_data <- panel_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot
ggplot(numeric_data, aes(x = value)) +
  geom_histogram(fill =rgb(0, 0.5, 1, alpha = 0.5), bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("Histograms of numeric variables")