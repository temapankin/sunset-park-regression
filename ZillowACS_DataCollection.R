
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
  "Hmisc",        # Vizualization
  "blscrapeR"    # Inflation data 
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
    zip_code %in% c("11220", "11232", "11237", "11221", "11211", "11222"), # Filter to only have treatment and control neighborhoods
    date >= ymd("2018-01-01") #Filter for the period of interest
  )

# Select required columns
zillow_clean <- zillow_long %>%
  dplyr::select(zip_code, date, rent_zori, treatment, post, treatmentpost)

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
census_api_key("API_KEY",  install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

zips <- c("11220", "11232", # Sunset Park
          "11237", "11221", # Bushwick 
          "11211", "11222" # Greenpoint
        )

# Defining variables
vars <- c(
  median_income = "B19013_001",       # Median household income
  renters = "B25003_003",             # Renter-occupied units
  housing_units = "B25003_001",       # Total housing units
  total_pop = "B01003_001",           # Total population
  white = "B02001_002",               # White population
  black = "B02001_003",               # Black population
  asian = "B02001_005",               # Asian population
  hispanic = "B03003_003",            # Hispanic population
  vacant_units = "B25002_003",        # Vacant housing units
  total_units = "B25002_001",         # Total housing units (again, for vacancy calc)
  median_year_built = "B25035_001",   # Median year structure built
  unemployed = "B23025_005",          # Unemployed civilians
  labor_force = "B23025_003"          # Civilian labor force
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
    pct_hispanic = hispanicE / total_popE,
    vacancy_rate = vacant_unitsE / total_unitsE, 
    median_year_built = median_year_builtE,
    unemployment_rate = unemployedE / labor_forceE
  ) %>%
  dplyr::select(zip_code, median_income,pct_renters,total_pop, pct_white, pct_black, 
          pct_asian, pct_hispanic, vacancy_rate, median_year_built, unemployment_rate)

# Join to the rent data to create a panel df 
panel_data <- zillow_clean %>%
  left_join(acs_clean, by = "zip_code")

str(panel_data)

# Get CPI data using API
cpi_series <- bls_api("CUUR0000SA0", startyear = 2019, endyear = 2025, registrationKey = "API_KEY")
names(cpi_series)

cpi_clean <- cpi_series %>%
  filter(grepl("M\\d{2}", period)) %>%  
  mutate(
    month = as.integer(sub("M", "", period)), 
    date = as.Date(paste(year, month, "01", sep = "-")),
    date = ceiling_date(date, "month") - days(1),
    cpi = as.numeric(value)
  ) %>%
  dplyr::select(date, cpi) %>%
  arrange(date)

panel_data <- panel_data %>%
  left_join(cpi_clean, by = "date") %>%
  mutate(
    rent_real = rent_zori / cpi * 100)

# Filter only numeric columns
numeric_data <- panel_data %>%
  dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot
ggplot(numeric_data, aes(x = value)) +
  geom_histogram(fill =rgb(0, 0.5, 1, alpha = 0.5), bins = 30) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme(
    panel.grid = element_blank()
  ) +
  ggtitle("Histograms of numeric variables")

ggplot(panel_data, aes(x = as.Date(date), y = rent_zori, color = factor(zip_code))) +
  geom_line() +
  labs(
    title = "Rent Trends by ZIP Code",
    x = "Date",
    y = "Rent in $ (ZORI)",
    color = "ZIP Code"
  )

# Export data
saveRDS(panel_data, file = "panel_data.rds")
write.csv(panel_data, "panel_data.csv")

