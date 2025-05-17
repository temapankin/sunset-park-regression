
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
  "gridExtra",   # Vizualization
  "fixest",       # Regression
  "modelsummary"  # Model Output
)

# Load all packages
lapply(packages, library, character.only = TRUE)

# Read Data
panel_data <- readRDS(file = "panel_data.rds")

# Distribution of Rent plot
ggplot(panel_data, aes(x = rent_real)) +
  geom_histogram(fill = "steelblue", color = "white") +
  labs(title = "Distribution of Rent (ZORI)", x = "Rent", y = "Count") +
  theme_minimal()

# Rent distribution by ZIP
panel_data %>%
  ggplot(aes(x = rent_real, fill = zip_code)) +
  geom_histogram(binwidth = 100, position = "identity", alpha = 0.5) +
  facet_wrap(~zip_code, scales = "free_y") +
  labs(title = "Rent Distribution by ZIP Code")


# Log of independent variable
panel_data <- panel_data %>% mutate(log_rent = log(rent_real))

# Distribution of log rent plot
ggplot(panel_data, aes(x = log_rent)) +
  geom_histogram(fill = "sienna2", color = "white") +
  labs(title = "Distribution of Rent (ZORI)", x = "Rent (log)", y = "Count") +
  theme_minimal()

# Create scatter plots to compare dependent variables to independent
panel_data_long <- panel_data %>%
  dplyr::select(log_rent, median_income, pct_renters, pct_white, pct_black,
     pct_asian, pct_hispanic, vacancy_rate, median_year_built, unemployment_rate) %>%
  pivot_longer(
    cols = -log_rent,
    names_to = "variable",
    values_to = "value"
  )

ggplot(panel_data_long, aes(x = value, y = log_rent)) +
  geom_point(alpha = 0.4, color = 'sienna2') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Log Rent vs Independent Variables",
    x = "Value",
    y = "Log Rent (ZORI)"
  )


# Check Parallel Trends 
ggplot(panel_data, aes(x = date, y = log_rent, color = as.factor(treatment))) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = as.Date("2024-02-01"), linetype = "dashed") +
  labs(color = "Treatment")

# Check the distribution 
qqnorm(log(panel_data$log_rent))
qqline(log(panel_data$log_rent))

# Check Variance and Correlation between variables 
corrmatrix <- cor(panel_data[, c("pct_white", "pct_black", "pct_asian", "pct_hispanic", "pct_renters", "unemployment_rate", "vacancy_rate", "median_year_built")], use = "complete.obs")

# Export for report
write.csv(round(corrmatrix, 2), "correlation_matrix.csv")

# Scale data
panel_data <- panel_data %>%
  mutate(across(c(median_income, pct_white, pct_black, pct_asian, pct_hispanic, 
                  unemployment_rate, vacancy_rate, median_year_built), scale))


# Run the regression model and check the summary
model <- lm(log_rent ~ median_income + pct_white + pct_white + pct_hispanic +
                  unemployment_rate + median_year_built + post + treatment + treatmentpost,
                  data = panel_data)
summary(model)

# Export to Word doc
modelsummary(model, output = "regression.docx")


# Explore the model
plot(model)

# Histogram of residuals
hist(resid(model), breaks = 30, main = "Histogram of Residuals")

# Q-Q plot
qqnorm(resid(model)); qqline(resid(model))


# Run the regression model with fixed effects for zip
model_clustered <- feols(log_rent ~ post + treatmentpost + median_income +
  pct_white + pct_hispanic + unemployment_rate + median_year_built |
  zip_code,
  cluster = ~zip_code,
  data = panel_data)
summary(model_clustered)

# Breusch-Pagan test
library(lmtest)
bptest(model)

# Check second model
coefplot(model_clustered)
etable(model_clustered) 