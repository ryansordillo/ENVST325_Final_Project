#Ryan Sordillo
#ENVST325 Final Project
#4/28/2026

# ── Libraries ────────────────────────────────────────────────────────────────
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(caret)
library(olsrr)
library(rsoi)
library(randomForest)
library(patchwork)

# ── ENSO / ONI Data ───────────────────────────────────────────────────────────
# Download Oceanic Nino Index directly from NOAA via rsoi package
oni_raw <- download_oni()
# Parse year and month from Date column and create binary El Nino / La Nina indicators
oni_clean <- oni_raw %>%
  mutate(
    year  = as.integer(format(Date, "%Y")),
    month = as.integer(format(Date, "%m"))
  ) %>%
  select(year, month, ONI, phase) %>%
  mutate(
    el_nino = ifelse(phase == "Warm Phase/El Nino", 1, 0),
    la_nina = ifelse(phase == "Cool Phase/La Nina", 1, 0)
  )

# ── CAL FIRE Data ─────────────────────────────────────────────────────────────
# Load California historic fire perimeters dataset from CAL FIRE FRAP
# Source: https://data.ca.gov/dataset/california-fire-perimeters-all
fire <- read_csv("ENVST325 Final Project/fire_data/California_Historic_Fire_Perimeters_3836453159319713276.csv")

# ── Data Cleaning ─────────────────────────────────────────────────────────────
# Rename columns to remove spaces, parse dates, and filter to reliable records
fire_clean <- fire %>%
  rename(
    acres      = `GIS Calculated Acres`,
    alarm_date = `Alarm Date`,
    cont_date  = `Containment Date`,
    cause      = Cause,
    agency     = Agency,
    year       = Year,
    unit_id    = `Unit ID`
  ) %>%
  mutate(
    alarm_date = mdy_hms(alarm_date),
    cont_date  = mdy_hms(cont_date)
  ) %>%
  filter(
    !is.na(alarm_date),  # remove fires with no ignition date (5,396 rows)
    !is.na(year),        # remove fires with no year (77 rows)
    !is.na(agency),      # remove fires with no agency (49 rows)
    acres > 0,           # remove zero-acre records
    year >= 1950         # restrict to modern records with reliable attributes
  )

# Restrict to California fires only — important since climate data is CA-specific
fire_clean <- fire_clean %>%
  filter(State == "CA")
  
# ── Feature Engineering ───────────────────────────────────────────────────────

# Binary ignition cause indicators
# Cause code 1 = Lightning, 7 = Arson, 11 = Power Line
fire_clean <- fire_clean %>%
  mutate(
    lightning  = ifelse(cause == 1,  1, 0),
    arson      = ifelse(cause == 7,  1, 0),
    power_line = ifelse(cause == 11, 1, 0)
  )

# Temporal features and fire duration
fire_clean <- fire_clean %>%
  mutate(
    log_acres     = log(acres),   # log transform to address right skew in response variable
    month         = month(alarm_date),
    season        = ifelse(month %in% c(12,1,2), "Winter",
                           ifelse(month %in% c(3,4,5),  "Spring",
                                  ifelse(month %in% c(6,7,8),  "Summer", "Fall"))),
    duration_days = as.numeric(difftime(cont_date, alarm_date, units = "days"))
  )

# Agency and fire season binary indicators
# Federal agencies: USFS, BLM, NPS, FWS, BIA, DOD
# Fire season defined as May-October (peak fire risk months in California)
fire_clean <- fire_clean %>%
  mutate(
    federal     = ifelse(agency %in% c("USF","BLM","NPS","FWS","BIA","DOD"), 1, 0),
    fire_season = ifelse(month %in% c(5,6,7,8,9,10), 1, 0)
  )

# Join ENSO indicators by year and month
fire_clean <- fire_clean %>%
  left_join(oni_clean, by = c("year", "month"))

# Remove fires < 1 acre so all log_acres values are positive
sum(fire_clean$acres < 1)
fire_clean <- fire_clean %>% filter(acres >= 1)

# ── Climate Data ──────────────────────────────────────────────────────────────
# Load statewide monthly climate data from NOAA Climate at a Glance
# Source: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/

# Function to parse NOAA CSV files which have 3 header rows to skip
clean_noaa <- function(filepath, value_name) {
  read_csv(filepath, skip = 3, col_names = c("date", value_name, "anomaly")) %>%
    filter(date != "Date") %>%            # remove stray column header row
    select(date, all_of(value_name)) %>%
    mutate(
      year         = as.integer(substr(date, 1, 4)),
      month        = as.integer(substr(date, 5, 6)),
      !!value_name := as.numeric(.data[[value_name]])  # convert from character to numeric
    ) %>%
    select(year, month, all_of(value_name))
}

temp   <- clean_noaa("ENVST325 Final Project/climate_data/cali_temp_data.csv",   "avg_temp")
precip <- clean_noaa("ENVST325 Final Project/climate_data/cali_precip_data.csv", "avg_precip")
pdsi   <- clean_noaa("ENVST325 Final Project/climate_data/cali_PDSI.csv",        "pdsi")

# Create lagged climate variables to capture antecedent conditions
# Fires respond not just to current month climate but to preceding months of drought/heat
temp_lag <- temp %>%
  arrange(year, month) %>%
  mutate(
    avg_temp_lag1 = lag(avg_temp, 1),  # temperature 1 month prior
    avg_temp_lag3 = lag(avg_temp, 3)   # temperature 3 months prior
  )

precip_lag <- precip %>%
  arrange(year, month) %>%
  mutate(
    avg_precip_lag1 = lag(avg_precip, 1),  # precipitation 1 month prior
    avg_precip_lag3 = lag(avg_precip, 3)   # precipitation 3 months prior
  )

pdsi_lag <- pdsi %>%
  arrange(year, month) %>%
  mutate(
    pdsi_lag1 = lag(pdsi, 1),  # drought index 1 month prior
    pdsi_lag3 = lag(pdsi, 3),  # drought index 3 months prior
    pdsi_lag6 = lag(pdsi, 6)   # drought index 6 months prior
  )

# Join all climate datasets to fire_clean by year and month
# Drop any previously joined climate columns first to avoid duplicates
fire_clean <- fire_clean %>%
  select(-any_of(c("avg_temp","avg_precip","pdsi",
                   "avg_temp_lag1","avg_temp_lag3",
                   "avg_precip_lag1","avg_precip_lag3",
                   "pdsi_lag1","pdsi_lag3","pdsi_lag6"))) %>%
  left_join(temp_lag,   by = c("year", "month")) %>%
  left_join(precip_lag, by = c("year", "month")) %>%
  left_join(pdsi_lag,   by = c("year", "month"))

# ── Final Data Checks ─────────────────────────────────────────────────────────

# Check duration NAs — too many to use as predictor (5801 missing)
sum(is.na(fire_clean$duration_days))

# Check agency distribution
table(fire_clean$agency)

# Remove the single OTH agency record — too few observations to model
fire_clean <- fire_clean %>%
  filter(agency != "OTH") %>%
  mutate(agency = factor(agency))

# Remove 2025 fires — NOAA climate data not yet available for 2025
fire_clean <- fire_clean %>% filter(!is.na(avg_temp))

# ── Train / Test Split ────────────────────────────────────────────────────────
# 80/20 stratified split using caret::createDataPartition
set.seed(123)
train_index <- createDataPartition(fire_clean$log_acres, p = 0.8, list = FALSE)
train <- fire_clean[train_index, ]
test  <- fire_clean[-train_index, ]

# ── Linear Regression Baseline ────────────────────────────────────────────────
# OLS model using ignition type, land ownership, temperature, and drought predictors
# All predictors are binary or continuous — no factor variables for interpretability
lm_model <- lm(log_acres ~ year + federal + lightning + arson +
                 avg_temp + avg_temp_lag3 + pdsi_lag1,
               data = train)
summary(lm_model)

# ── OLS Assumption Checks ─────────────────────────────────────────────────────

# Standardized residuals and fitted values
res.fire <- rstandard(lm_model)
fit.fire <- fitted.values(lm_model)

# Q-Q plot to assess normality of residuals
qqnorm(res.fire, pch = 19, col = "grey50", main = "Q-Q Plot: Wildfire Model")
qqline(res.fire)

# Shapiro-Wilk test for normality (limited to 4,999 obs due to test constraints)
shapiro.test(res.fire[1:4999])

# Residuals vs fitted plot to assess homoscedasticity and linearity
plot(fit.fire, res.fire, pch = 19, col = "grey50",
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Residuals vs Fitted: Wildfire Model")
abline(h = 0)

# Variance Inflation Factor — check for multicollinearity among predictors
ols_vif_tol(lm_model)

# Forward stepwise AIC selection to confirm variable inclusion is justified
fire.step <- ols_step_forward_aic(lm_model)
fire.step
plot(fire.step)
summary(fire.step$model)

# Linear model test set performance
lm_pred_train <- predict(lm_model, newdata = test)
mse_test      <- mean((test$log_acres - lm_pred_train)^2)
mse_test

# ── Random Forest Model ───────────────────────────────────────────────────────
# Random Forest can capture nonlinear relationships and variable interactions
# that OLS cannot, making it better suited to the complex climate-fire relationship

# Select variables for RF — includes all climate variables and ENSO indicators
rf_vars <- c(
  "log_acres",
  "year",
  "federal",
  "lightning",
  "arson",
  "avg_temp",
  "avg_temp_lag1",
  "avg_temp_lag3",
  "pdsi_lag1",
  "el_nino",
  "la_nina",
  "avg_precip",
  "avg_precip_lag1",
  "avg_precip_lag3"
)

# Subset and remove NAs for RF dataset
rf_data <- fire_clean %>%
  select(all_of(rf_vars)) %>%
  na.omit()

# 80/20 train/test split for RF (separate from LM split for consistency)
set.seed(123)
train_index <- createDataPartition(rf_data$log_acres, p = 0.8, list = FALSE)
train_rf    <- rf_data[train_index, ]
test_rf     <- rf_data[-train_index, ]

# Fit Random Forest with 500 trees and permutation-based variable importance
rf_model <- randomForest(
  log_acres ~ .,
  data      = train_rf,
  ntree     = 500,
  importance = TRUE
)

# Predict on test set
rf_pred <- predict(rf_model, newdata = test_rf)
rf_mse  <- mean((test_rf$log_acres - rf_pred)^2)
rf_mse

# Default variable importance plot
varImpPlot(rf_model)

# ── Model Comparison ──────────────────────────────────────────────────────────
# Evaluate both models on the same RF test set for a fair comparison
lm_pred <- predict(lm_model, newdata = test_rf)
lm_mse  <- mean((test_rf$log_acres - lm_pred)^2)
lm_r2   <- cor(test_rf$log_acres, lm_pred)^2
rf_r2   <- cor(test_rf$log_acres, rf_pred)^2

cat("=== Model Comparison ===\n")
cat("Linear Regression MSE:", round(lm_mse, 3), "\n")
cat("Linear Regression R²: ", round(lm_r2, 3), "\n")
cat("Random Forest MSE:    ", round(rf_mse, 3), "\n")
cat("Random Forest R²:     ", round(rf_r2, 3), "\n")

# ── Figures for Factsheet ─────────────────────────────────────────────────────
## Figure 1 — Random Forest Feature Importance
# Variables ranked by % increase in MSE when permuted
# Higher = more important for prediction
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(variable = recode(variable,
                           "federal"         = "Federal Land",
                           "year"            = "Year",
                           "lightning"       = "Lightning Ignition",
                           "avg_precip_lag3" = "Precipitation (3-mo lag)",
                           "pdsi_lag1"       = "Drought Index (1-mo lag)",
                           "avg_temp_lag3"   = "Temperature (3-mo lag)",
                           "avg_precip"      = "Precipitation",
                           "avg_temp_lag1"   = "Temperature (1-mo lag)",
                           "avg_precip_lag1" = "Precipitation (1-mo lag)",
                           "avg_temp"        = "Temperature",
                           "la_nina"         = "La Nina",
                           "el_nino"         = "El Nino",
                           "arson"           = "Arson Ignition"
  ))

ggplot(importance_df, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title    = "Random Forest Feature Importance",
       subtitle = "Variables ranked by % increase in MSE when permuted",
       x = NULL,
       y = "% Increase in MSE") +
  theme_minimal(base_size = 13)


## Figure 2 — Actual vs Predicted: LM and RF side by side
pred_df <- data.frame(
  actual  = test_rf$log_acres,
  lm_pred = lm_pred,
  rf_pred = rf_pred
)

# Random Forest plot with fitted trend equation
lm_fit_rf <- lm(rf_pred ~ actual, data = pred_df)
coefs_rf  <- round(coef(lm_fit_rf), 3)
r2_rf     <- round(summary(lm_fit_rf)$r.squared, 3)
eq_rf     <- paste0("y = ", coefs_rf[2], "x + ", coefs_rf[1], "  |  R² = ", r2_rf)

plot_rf <- ggplot(pred_df, aes(x = actual, y = rf_pred)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkblue", linewidth = 0.8, se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate("text", x = min(pred_df$actual) + 0.5,
           y = max(pred_df$rf_pred) - 0.3,
           label = eq_rf, hjust = 0, size = 4.5, color = "darkblue") +
  labs(title    = "Random Forest",
       subtitle = paste("R² =", round(rf_r2, 3), " | MSE =", round(rf_mse, 3)),
       x = "Actual Log Acres", y = "Predicted Log Acres") +
  theme_minimal(base_size = 13)

# Linear regression plot with fitted trend equation
lm_fit_lm <- lm(lm_pred ~ actual, data = pred_df)
coefs_lm  <- round(coef(lm_fit_lm), 3)
r2_lm     <- round(summary(lm_fit_lm)$r.squared, 3)
eq_lm     <- paste0("y = ", coefs_lm[2], "x + ", coefs_lm[1], "  |  R² = ", r2_lm)

plot_lm <- ggplot(pred_df, aes(x = actual, y = lm_pred)) +
  geom_point(alpha = 0.3, color = "tomato") +
  geom_smooth(method = "lm", color = "darkgreen", linewidth = 0.8, se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "royalblue", linetype = "dashed") +
  annotate("text", x = min(pred_df$actual) + 0.5,
           y = max(pred_df$lm_pred) - 0.3,
           label = eq_lm, hjust = 0, size = 4.5, color = "darkgreen") +
  labs(title    = "Linear Regression",
       subtitle = paste("R² =", round(lm_r2, 3), " | MSE =", round(lm_mse, 3)),
       x = "Actual Log Acres", y = "Predicted Log Acres") +
  theme_minimal(base_size = 13)

# Combined side-by-side comparison plot
plot_lm <- plot_lm + 
  coord_cartesian(xlim = c(0, 12), ylim = c(3, 7))

plot_rf <- plot_rf + 
  coord_cartesian(xlim = c(0, 12), ylim = c(2, 9))

plot_lm + plot_rf +
  plot_annotation(
    title    = "Model Comparison: Actual vs Predicted Log Acres Burned",
    subtitle = "Dashed line = perfect prediction  |  Colored line = fitted trend",
    theme    = theme(
      plot.title    = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

## Figure 3 — Time Series: Total Acres Burned Per Year
# Shows the long-term trend in California wildfire severity from 1950-2025
fire_clean %>%
  group_by(year) %>%
  summarise(total_acres = sum(acres)) %>%
  ggplot(aes(x = year, y = total_acres)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Total Acres Burned Per Year in California (1950-2025)",
       subtitle = "Dashed line shows long-term linear trend",
       x = "Year",
       y = "Total Acres Burned") +
  theme_minimal(base_size = 13)

## Figure 4 — Distribution: Raw vs Log-Transformed Acres
# Demonstrates justification for log transformation of response variable
par(mfrow = c(1,2))
hist(fire_clean$acres,
     breaks = 50, col = "orange",
     main = "Raw Acres Burned",
     xlab = "Acres")
hist(fire_clean$log_acres,
     breaks = 50, col = "steelblue",
     main = "Log Acres Burned",
     xlab = "Log Acres")
par(mfrow = c(1,1))



