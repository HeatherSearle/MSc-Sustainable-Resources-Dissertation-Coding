library(dplyr)
library(ggplot2)

load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/UK_lobster_landings.RData") 
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_SST_data/UK_average_monthly_temperature.RData")
# monthly_lobster_landings need this loaded from the standardising script and average monthly temp as well - should probably save

combined_standardised_data_for_regression <- inner_join(
  average_monthly_temperature,
  monthly_lobster_landings,
  by = c("date" = "month_start_date") # Join by 'date' from temp and 'month_start_date' from lobster
)

str(combined_standardised_data_for_regression_SSTL)

# testing for a linear relationship ---------------------------------------

cor.test(combined_standardised_data_for_regression_SSTL$standardised_avg_temp,
         combined_standardised_data_for_regression_SSTL$standardised_landings)
# 0 = weak linear relationship and 1/-1 = strong linear relationship - we got 0.58 so there is a relationship maybe masked by seasonality 

# simple lm regression temperature and landings ------------------------------

SSTL.lm <- lm(standardised_landings ~ standardised_avg_temp,
                  data = combined_standardised_data_for_regression_SSTL)

summary(SSTL.lm)

# not suprising but there is no clear linear relationship at this level between just these two variables but we do get some good insights

# Plotting the data -------------------------------------------------------
# average temperature (historical 2014-2024)
ggplot(average_monthly_temperature, aes(date, avg_temp)) +
  geom_line(, colour = "darkred") +
  theme_minimal()

# monthly lobster landings value (2014-2024)
ggplot(monthly_lobster_value, aes(month_start_date, total_value_000s)) +
  geom_line(, colour = "darkgreen") +
  theme_minimal()

# average temperature (scenario 1-3)
ggplot(scenario_1_filtered, aes(date, avg_temp)) +
  geom_line(, colour = "darkblue") +
  theme_minimal()

ggplot(scenario_2_filtered, aes(date, avg_temp)) +
  geom_line(, colour = "darkblue") +
  theme_minimal()

ggplot(scenario_3_filtered, aes(date, avg_temp)) +
  geom_line(, colour = "darkblue") +
  theme_minimal()

# Accounting for monthly variance and time lags between landings --------

# need a month column as this will be a new categorical variable in the model

combined_standardised_data_for_regression_SSTL$month <- format(combined_standardised_data_for_regression_SSTL$date, "%m")

combined_standardised_data_for_regression_SSTL$month_factor <- as.factor(combined_standardised_data_for_regression_SSTL$month)

str(combined_standardised_data_for_regression_SSTL)

SSTL.seasonal.lm <- lm(standardised_landings ~ standardised_avg_temp + month_factor,
                           data = combined_standardised_data_for_regression_SSTL)

summary(SSTL.seasonal.lm )
#r.squared = 0.9673519 - this is great! means much of the variance is explained by the model

# K-fold cross validation -------------------------------------------------
# checking if the model is decent 
ctrl <- trainControl(method = "cv", number = 10)
model <- train(standardised_value ~ standardised_avg_temp + month_factor,
               data = combined_standardised_data_for_regression_SSTV,
               method = "lm", trControl = ctrl)

print(model)

# this is already starting to look like a better fit model now onto see if a time lag helps
# Khan et al talk about lags for american lobsters 1-3 years and 8 year patterns but other studies also say larval development takes around 3 weeks

combined_standardised_data_for_regression <- combined_standardised_data_for_regression |> 
  arrange(date) |>  
  mutate(
    standardised_avg_temp_lag1 = lag(standardised_avg_temp, n = 1),  # Temperature from 1 month ago
    standardised_avg_temp_lag2 = lag(standardised_avg_temp, n = 12)   # Temperature from 12 months ago
  )

templand.seasonal.lag2.lm <- lm(standardised_landings ~ standardised_avg_temp_lag2 + month_factor,
                                data = combined_standardised_data_for_regression)
summary(templand.seasonal.lag2.lm)

templand.seasonal.lag2.lm <- lm(standardised_landings ~ standardised_avg_temp_lag2 + month_factor,
                                data = combined_standardised_data_for_regression)

plot_lagged_regression <- ggplot(combined_standardised_data_for_regression,
                                 aes(x = standardised_avg_temp_lag2,
                                     y = standardised_landings, color = month_factor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + # Separate lines for each month
  labs(
    title = "Standardised Landings vs. Standardised Temp (12-month lag) by Month",
    x = "Standardised Avg. Monthly Temperature (12-month lag)",
    y = "Standardised Monthly Lobster Landings",
    color = "Month"
  ) +
  theme_minimal()

print(plot_lagged_regression)


combined_standardised_data_for_regression$predicted_landings_lag <- predict(templand.seasonal.lag2.lm,
                                                                            newdata = combined_standardised_data_for_regression)

plot_predicted_vs_actual_lagged <- ggplot(combined_standardised_data_for_regression,
                                          aes(x = predicted_landings_lag,
                                              y = standardised_landings)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed") +
  labs(
    title = "Actual vs. Predicted Landings (Lagged Temp + Seasonality)",
    x = "Predicted Standardised Landings",
    y = "Actual Standardised Landings"
  ) +
  theme_minimal()

print(plot_predicted_vs_actual_lagged)

# at first look this doesn't seem to improve the model much so I think just accounting for seasonality has a big impact. 


# Scenario 1 (landings) predictions --------------------------------------------------

scenario_1_sst_data$predicted_landings <- predict(templand.seasonal.lm, newdata = scenario_1_sst_data)

summary(scenario_1_sst_data$predicted_landings)
head(scenario_1_sst_data$predicted_landings)

library(ggplot2)

ggplot(scenario_1_sst_data, aes(x = date, y = predicted_landings)) +
  geom_line(color = "darkblue", size = 1) +
  labs(
    title = "Predicted Lobster Landings under Scenario 1",
    x = "Date",
    y = "Predicted Landings (standardised units)"
  ) +
  theme_minimal()


# regression temperature and value (SSTV) ----------------------------------------
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/monthly_standardised_lobster_value.RData")
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_SST_data/UK_average_standardised_monthly_temperature.RData")
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_1_sst_data.RData")

combined_standardised_data_for_regression_SSTV <- inner_join(
  average_monthly_temperature,
  monthly_lobster_value,
  by = c("date" = "month_start_date") # Join by 'date' from temp and 'month_start_date' from lobster
)

str(combined_standardised_data_for_regression_SSTV)

# testing for a linear relationship 

cor.test(combined_standardised_data_for_regression_SSTV$standardised_avg_temp,
         combined_standardised_data_for_regression_SSTV$standardised_value)
# 0 = weak linear relationship and 1/-1 = strong linear relationship - we got 0.59 so there is a relationship maybe masked by seasonality 

SSTV.lm <- lm(standardised_value ~ standardised_avg_temp,
                  data = combined_standardised_data_for_regression_SSTV)
summary(SSTV.lm)
# the lm here isn't actually that bad but we can definitely improve 

# Accounting for seasonality
combined_standardised_data_for_regression_SSTV$month <- format(combined_standardised_data_for_regression_SSTV$date, "%m")

combined_standardised_data_for_regression_SSTV$month_factor <- as.factor(combined_standardised_data_for_regression_SSTV$month)

str(combined_standardised_data_for_regression_SSTV)

SSTV.seasonal.lm <- lm(standardised_value ~ standardised_avg_temp + month_factor,
                           data = combined_standardised_data_for_regression_SSTV)
summary(SSTV.seasonal.lm )
# R squared value = 0.84-5 so explained a great deal of the variance but there is
# also room for other factors to be contributing to value 


# Scenario 1 (value) predictions ------------------------------------------
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_1_filtered_standardised.RData")
scenario_1_filtered$predicted_value <- predict(SSTV.seasonal.lm, newdata = scenario_1_filtered)

ggplot(scenario_1_filtered, aes(x = date, y = predicted_value)) +
  geom_line(color = "darkred", linewidth = 1) +
  labs(
    title = "Predicted Lobster Value under Scenario 1 (2025–2045)",
    x = "Date",
    y = "Predicted value (standardised units)"
  ) +
  theme_minimal()

# inspecting the predicted data
# unscaling - these are slightly different from the original values need to check why
mean_val <- mean(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)
sd_val <- sd(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)

scenario_1_filtered <- scenario_1_filtered |>
  mutate(
    predicted_value_unscaled = predicted_value * sd_val + mean_val
  )

original_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, total_value_000s) |>
  mutate(source = "Observed")

predicted_values <- scenario_1_filtered |>
  select(date, total_value_000s = predicted_value_unscaled) |>
  mutate(source = "Predicted")

combined_plot_data <- bind_rows(original_values, predicted_values)

ggplot(combined_plot_data, aes(x = date, y = total_value_000s, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_smooth() +
  labs(
    title = "Observed vs Predicted Lobster Value",
    x = "Date",
    y = "Lobster Value (£000s)",
    color = "Data Source"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Adding Fuel data into the model ---------------------------------------------
combined_standardised_data_for_regression_SSTV$year <- format(as.Date(combined_standardised_data_for_regression_SSTV$date), "%Y")
average_fuel_costs_data$Year <- as.character(average_fuel_costs_data$Year)
names(average_fuel_costs_data)[names(average_fuel_costs_data) == "Year"] <- "year"

SSTV_data_with_fuel <- merge(combined_standardised_data_for_regression_SSTV,
                             average_fuel_costs_data,
                                 by = "year")

# standardising the fuel data within the data frame 
SSTV_data_with_fuel$standardised_fuel_cost <- scale(SSTV_data_with_fuel$average_fuel_costs)


SSTVF.seasonal.lm <- lm(standardised_value ~ standardised_avg_temp + month_factor + standardised_fuel_cost,
                        data = SSTV_data_with_fuel)
summary(SSTVF.seasonal.lm)
# very slightly improved R squared value from adjusted 0.8415 to adjusted 0.8426