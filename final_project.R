#Ryan Sordillo
#ENVST325 Final Project
#4/28/2026

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(caret)
library(olsrr)
library(rsoi)

oni_raw <- download_oni()
head(oni_raw)

fire <- read_csv("ENVST325 Final Project/fire_data/California_Historic_Fire_Perimeters_3836453159319713276.csv")

##Data Cleaning##
oni_clean <- oni_raw %>%
  mutate(
    year  = as.integer(format(Date, "%Y")),
    month = as.integer(format(Date, "%m"))
  ) %>%
  select(year, month, ONI, phase) %>%
  mutate(
    el_nino = ifelse(phase ==  "Warm Phase/El Nino", 1, 0),
    la_nina = ifelse(phase == "Cool Phase/La Nina", 1, 0)
  )

fire_clean <- fire %>%
  rename(
    acres        = `GIS Calculated Acres`,
    alarm_date   = `Alarm Date`,
    cont_date    = `Containment Date`,
    cause        = Cause,
    agency       = Agency,
    year         = Year,
    unit_id      = `Unit ID`
  ) %>%
  mutate(
    alarm_date = mdy_hms(alarm_date),
    cont_date  = mdy_hms(cont_date)
  ) %>%
  filter(
    !is.na(alarm_date),   # drops 5,396 rows
    !is.na(year),         # drops 77 rows
    !is.na(agency),       # drops 49 rows
    acres > 0,            # drops zeros
    year >= 1950          # improves data completeness
  )

fire_clean <- fire_clean %>%
  filter(State == "CA")
  
#Feature Engineering

fire_clean <- fire_clean %>%
  mutate(
    # Ignition type binaries
    lightning    = ifelse(cause == 1, 1, 0),
    arson        = ifelse(cause == 7, 1, 0),
    power_line   = ifelse(cause == 11, 1, 0)
  )

fire_clean <- fire_clean %>%
  mutate(
    log_acres = log(acres),
    month = month(alarm_date),
    season = ifelse(month %in% c(12,1,2), "Winter",
             ifelse(month %in% c(3,4,5), "Spring",
             ifelse(month %in% c(6,7,8), "Summer", "Fall"))),
    duration_days = as.numeric(difftime(cont_date, alarm_date, units = 'days'))
  )

fire_clean <- fire_clean %>%
  mutate(
    federal = ifelse(agency %in% c("USF", "BLM", "NPS", "FWS", "BIA", "DOD"), 
                     1, 0),
    fire_season = ifelse(month %in% c(5,6,7,8,9,10),1,0)
  )

fire_clean <- fire_clean %>%
  left_join(oni_clean, by = c("year", "month"))

#Remove fires with less than 1 acres burned so log values are positive
sum(fire_clean$acres < 1)
fire_clean <- fire_clean %>% filter(acres >= 1)

###Other Dataset wrangling
temp <- read_csv("ENVST325 Final Project/climate_data/cali_temp_data.csv")
precip <- read_csv("ENVST325 Final Project/climate_data/cali_precip_data.csv")
pdsi <- read_csv("ENVST325 Final Project/climate_data/cali_PDSI.csv")

# Function to clean NOAA climate files
clean_noaa <- function(filepath, value_name) {
  read_csv(filepath, skip = 3, col_names = c("date", value_name, "anomaly")) %>%
    filter(date != "Date") %>%          # remove stray header row
    select(date, all_of(value_name)) %>%
    mutate(
      year         = as.integer(substr(date, 1, 4)),
      month        = as.integer(substr(date, 5, 6)),
      !!value_name := as.numeric(.data[[value_name]])  # convert to numeric
    ) %>%
    select(year, month, all_of(value_name))
}

temp   <- clean_noaa("ENVST325 Final Project/climate_data/cali_temp_data.csv",   "avg_temp")
precip <- clean_noaa("ENVST325 Final Project/climate_data/cali_precip_data.csv", "avg_precip")
pdsi   <- clean_noaa("ENVST325 Final Project/climate_data/cali_PDSI.csv",   "pdsi")


# Create lagged climate variables in the NOAA data before joining
temp_lag <- temp %>%
  arrange(year, month) %>%
  mutate(
    avg_temp_lag1 = lag(avg_temp, 1),   # 1 month lag
    avg_temp_lag3 = lag(avg_temp, 3)    # 3 month lag
  )

precip_lag <- precip %>%
  arrange(year, month) %>%
  mutate(
    avg_precip_lag1 = lag(avg_precip, 1),
    avg_precip_lag3 = lag(avg_precip, 3)
  )

pdsi_lag <- pdsi %>%
  arrange(year, month) %>%
  mutate(
    pdsi_lag1 = lag(pdsi, 1),
    pdsi_lag3 = lag(pdsi, 3),
    pdsi_lag6 = lag(pdsi, 6)
  )

#Join different datasets into fire_clean
fire_clean <- fire_clean %>%
  select(-any_of(c("avg_temp", "avg_precip", "pdsi",
                   "avg_temp_lag1", "avg_temp_lag3",
                   "avg_precip_lag1", "avg_precip_lag3",
                   "pdsi_lag1", "pdsi_lag3", "pdsi_lag6"))) %>%
  left_join(temp_lag,   by = c("year", "month")) %>%
  left_join(precip_lag, by = c("year", "month")) %>%
  left_join(pdsi_lag,   by = c("year", "month"))

#Data Visualization

#Distribution of acres vs log_acres
hist(fire_clean$acres, main = "Acres Burned (raw)", 
     xlab = "Acres", col = "orange", breaks = 50)
hist(fire_clean$log_acres, main = "Acres Burned (log)", 
     xlab = "Log Acres", col = "steelblue", breaks = 50)

fires_per_year <- fire_clean %>%
  group_by(year) %>%
  summarise(n_fires = n(),
            total_acres = sum(acres),
            mean_acres = mean(acres))

#Fires per year over time
ggplot(fires_per_year, aes(x = year, y = total_acres)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(title = "Total Acres Burned Per Year in California (1950-2025)",
       x = "Year", y = "Total Acres Burned") +
  theme_minimal()


sum(is.na(fire_clean$duration_days))
#5830
table(fire_clean$agency)
#Remove OTH
fire_clean <- fire_clean %>%
  filter(agency != "OTH") %>%
  mutate(
    agency = factor(agency)
  )

#Remove 2025 data which are na
fire_clean <- fire_clean %>% filter(!is.na(avg_temp))

# 80/20 train/test split
train_index <- createDataPartition(fire_clean$log_acres, p = 0.8, list = FALSE)
train <- fire_clean[train_index, ]
test  <- fire_clean[-train_index, ]

lm_model <- lm(log_acres ~ year + federal + lightning + arson + avg_temp + avg_temp_lag3 +
                 pdsi_lag1, data = train)


summary(lm_model)


##Assumption Check

# Standardized residuals and fitted values
res.fire <- rstandard(lm_model)
fit.fire <- fitted.values(lm_model)

# 1. Q-Q Plot (Normality of Residuals)
qqnorm(res.fire, pch = 19, col = "grey50", main = "Q-Q Plot: Wildfire Model")
qqline(res.fire)


#Shapiro Wilks Normality Test
shapiro.test(res.fire[1:4999])

plot(fit.fire, res.fire, pch = 19, col = "grey50",
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Residuals vs Fitted: Wildfire Model")
abline(h = 0)


ols_vif_tol(lm_model)

fire.step <- ols_step_forward_aic(lm_model)
fire.step
plot(fire.step)
summary(fire.step$model)

#Testing results for linear model

test_pred <- predict(lm_model, newdata = test)
mse_test <- mean((test$log_acres - test_pred)^2)
mse_test


#Random Forest
library(randomForest)
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

rf_data <- fire_clean %>%
  select(all_of(rf_vars)) %>%
  na.omit()

set.seed(123)
train_index <- createDataPartition(rf_data$log_acres, p = 0.8, list = FALSE)
train_rf <- rf_data[train_index, ]
test_rf  <- rf_data[-train_index, ]

rf_model <- randomForest(
  log_acres ~ .,
  data = train_rf,
  ntree = 500,
  importance = TRUE
)

rf_pred <- predict(rf_model, newdata = test_rf)

rf_mse <- mean((test_rf$log_acres - rf_pred)^2)

rf_mse

varImpPlot(rf_model)

# Get comparable metrics for both models
lm_pred <- predict(lm_model, newdata = test_rf)
lm_mse  <- mean((test_rf$log_acres - lm_pred)^2)
lm_r2   <- cor(test_rf$log_acres, lm_pred)^2
rf_r2   <- cor(test_rf$log_acres, rf_pred)^2

cat("=== Model Comparison ===\n")
cat("Linear Regression MSE:", round(lm_mse, 3), "\n")
cat("Linear Regression R²: ", round(lm_r2, 3), "\n")
cat("Random Forest MSE:    ", round(rf_mse, 3), "\n")
cat("Random Forest R²:     ", round(rf_r2, 3), "\n")



####Plots for Factsheet
## Figure 1 - Feature Importance for random forest##
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(variable = recode(variable,
                           "federal"= "Federal Land",
                           "year"= "Year",
                           "lightning"= "Lightning Ignition",
                           "avg_precip_lag3"= "Precipitation (3-mo lag)",
                           "pdsi_lag1"= "Drought Index (1-mo lag)",
                           "avg_temp_lag3"= "Temperature (3-mo lag)",
                           "avg_precip" = "Precipitation",
                           "avg_temp_lag1"= "Temperature (1-mo lag)",
                           "avg_precip_lag1"= "Precipitation (1-mo lag)",
                           "avg_temp" = "Temperature",
                           "la_nina"= "La Nina",
                           "el_nino"= "El Nino",
                           "arson"          = "Arson Ignition"
  ))

ggplot(importance_df, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Random Forest Feature Importance",
       subtitle = "Variables ranked by % increase in MSE",
       x = NULL,
       y = "% Increase in MSE") +
  theme_minimal(base_size = 13)


## Figure 2 - Actual vs Predicted ##
library(ggpmisc)
pred_df <- data.frame(
  actual    = test_rf$log_acres,
  lm_pred   = lm_pred,
  rf_pred   = rf_pred
)

library(ggpmisc)

#Random Forest
lm_fit_rf  <- lm(rf_pred ~ actual, data = pred_df)
coefs_rf   <- round(coef(lm_fit_rf), 3)
r2_rf      <- round(summary(lm_fit_rf)$r.squared, 3)
eq_rf      <- paste0("y = ", coefs_rf[2], "x + ", coefs_rf[1], "  |  R² = ", r2_rf)

plot_rf <- ggplot(pred_df, aes(x = actual, y = rf_pred)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkblue", linewidth = 0.8, se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  annotate("text", x = min(pred_df$actual) + 0.5,
           y = max(pred_df$rf_pred) - 0.3,
           label = eq_rf, hjust = 0, size = 4.5, color = "darkblue") +
  labs(title = "Random Forest: Actual vs Predicted Log Acres",
       subtitle = "Red dashed = perfect prediction  |  Blue = fitted trend",
       x = "Actual Log Acres", y = "Predicted Log Acres") +
  theme_minimal(base_size = 13)

#linear Model
lm_fit_lm  <- lm(lm_pred ~ actual, data = pred_df)
coefs_lm   <- round(coef(lm_fit_lm), 3)
r2_lm      <- round(summary(lm_fit_lm)$r.squared, 3)
eq_lm      <- paste0("y = ", coefs_lm[2], "x + ", coefs_lm[1], "  |  R² = ", r2_lm)

plot_lm <- ggplot(pred_df, aes(x = actual, y = lm_pred)) +
  geom_point(alpha = 0.3, color = "tomato") +
  geom_smooth(method = "lm", color = "darkgreen", linewidth = 0.8, se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "royalblue", linetype = "dashed") +
  annotate("text", x = min(pred_df$actual) + 0.5,
           y = max(pred_df$lm_pred) - 0.3,
           label = eq_lm, hjust = 0, size = 4.5, color = "darkgreen") +
  labs(title = "Linear Regression: Actual vs Predicted Log Acres",
       subtitle = "Blue dashed = perfect prediction  |  Dark green = fitted trend",
       x = "Actual Log Acres", y = "Predicted Log Acres") +
  theme_minimal(base_size = 13)

## Figure 3 - Time Series of Total Acres Burned ##
fire_clean %>%
  group_by(year) %>%
  summarise(total_acres = sum(acres)) %>%
  ggplot(aes(x = year, y = total_acres)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "black", linetype = "dashed") +
  labs(title = "Total Acres Burned Per Year in California (1950-2025)",
       x = "Year",
       y = "Total Acres Burned") +
  theme_minimal()

## Figure 4 - Distribution raw vs log ##
par(mfrow = c(1,2))
hist(fire_clean$acres, breaks = 50, col = "orange",
     main = "Raw Acres Burned",
     xlab = "Acres")
hist(fire_clean$log_acres, breaks = 50, col = "steelblue",
     main = "Log Acres Burned",
     xlab = "Log Acres")
par(mfrow = c(1,1))


