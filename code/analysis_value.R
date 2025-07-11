load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/monthly_standardised_lobster_value.RData")
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_SST_data/UK_average_standardised_monthly_temperature.RData")

library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)


# Validating the relationship between catch and value ---------------------

catch_value.lm <- lm(value_000s ~ live_weight_tonnes,
                  data = UK_lobster_landings)

summary(catch_value.lm) #R-squared:  0.8984 nice ;)

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


# Seasonal Regression Model  ----------------------------------------------
# Join by 'date' from temp and 'month_start_date' from lobster
combined_standardised_data_for_regression_SSTV <- inner_join(
  average_monthly_temperature,
  monthly_lobster_value,
  by = c("date" = "month_start_date"))

str(combined_standardised_data_for_regression_SSTV)

combined_standardised_data_for_regression_SSTV$month <- format(combined_standardised_data_for_regression_SSTV$date,"%m")
combined_standardised_data_for_regression_SSTV$month_factor <- as.factor(combined_standardised_data_for_regression_SSTV$month)

str(combined_standardised_data_for_regression_SSTV)

SSTV.seasonal.lm <- lm(standardised_value ~ standardised_avg_temp + month_factor,
                       data = combined_standardised_data_for_regression_SSTV)

summary(SSTV.seasonal.lm )
# Adjusted R-squared:0.8415 so explains a great deal of the variance


# K-fold cross validation -------------------------------------------------
# checking if the model is decent 
ctrl <- trainControl(method = "cv", number = 10)
model <- train(standardised_value ~ standardised_avg_temp + month_factor,
               data = combined_standardised_data_for_regression_SSTV,
               method = "lm", trControl = ctrl)

print(model)


# Scenario 1 --------------------------------------------------
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_1_filtered_standardised.RData")

# predicting value based on the scenario 1 temperatures 
scenario_1_filtered$predicted_value <- predict(SSTV.seasonal.lm, newdata = scenario_1_filtered)

ggplot(scenario_1_filtered, aes(x = date, y = predicted_value)) +
  geom_line(, colour = "red") +
  theme_minimal()

original_standard_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, standardised_value) |>
  mutate(source = "Observed")

predicted_standard_values <- scenario_1_filtered |>
  select(date, predicted_value) |>
  mutate(source = "Predicted") |> 
  rename(standardised_value = predicted_value)

S1_standardised_plot_data <- bind_rows(original_standard_values, predicted_standard_values)

ggplot(combined_plot_data, aes(x = date, y = standardised_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Un-standardising value 
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
  select(date, predicted_value_unscaled) |>
  mutate(source = "Predicted") |> 
  rename(total_value_000s = predicted_value_unscaled)

S1_data <- bind_rows(original_values, predicted_values)

ggplot(S1_data, aes(x = date, y = total_value_000s, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Investigating the results 
S1_data$year <- year(S1_data$date)

obs_data <- subset(S1_data, year >= 2014 & year <= 2024)
S1_2025_2035 <- subset(S1_data, year >= 2025 & year <= 2035)
S1_2035_2045 <- subset(S1_data, year > 2035 & year <= 2045)

# Totals
total_obs <- sum(obs_data$total_value_000s)
total_S1_2025_2035 <- sum(S1_2025_2035$total_value_000s)
total_S1_2035_2045 <- sum(S1_2035_2045$total_value_000s)

# Averages
avg_obs <- mean(obs_data$total_value_000s)
avg_S1_2025_2035 <- mean(S1_2025_2035$total_value_000s)
avg_S1_2035_2045 <- mean(S1_2035_2045$total_value_000s)

# Print
cat("Total (Observed 2014–2024):", total_obs, "\n")
cat("Total (S1 Predicted 2025–2035):", total_S1_2025_2035, "\n")
cat("Total (S1 Predicted 2035–2045):", total_S1_2035_2045, "\n\n")

cat("Average (Observed 2014–2024):", avg_obs, "\n")
cat("Average (S1 Predicted 2025–2035):", avg_S1_2025_2035, "\n")
cat("Average (S1 Predicted 2035–2045):", avg_S1_2035_2045, "\n")


# Scenario 2 --------------------------------------------------------------
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_2_filtered_standardised.RData")

# predicting value based on the scenario 1 temperatures 
scenario_2_filtered$predicted_value <- predict(SSTV.seasonal.lm, newdata = scenario_2_filtered)

ggplot(scenario_2_filtered, aes(x = date, y = predicted_value)) +
  geom_line(, colour = "red") +
  theme_minimal()

original_standard_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, standardised_value) |>
  mutate(source = "Observed")

predicted_standard_values <- scenario_2_filtered |>
  select(date, predicted_value) |>
  mutate(source = "Predicted") |> 
  rename(standardised_value = predicted_value)

S2_standardised_plot_data <- bind_rows(original_standard_values, predicted_standard_values)

ggplot(S2_standardised_plot_data, aes(x = date, y = standardised_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Un-standardising value 
mean_val <- mean(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)
sd_val <- sd(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)

scenario_2_filtered <- scenario_2_filtered |>
  mutate(
    predicted_value_unscaled = predicted_value * sd_val + mean_val
  )

original_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, total_value_000s) |>
  mutate(source = "Observed")

predicted_values <- scenario_2_filtered |>
  select(date, predicted_value_unscaled) |>
  mutate(source = "Predicted") |> 
  rename(total_value_000s = predicted_value_unscaled)

S2_data <- bind_rows(original_values, predicted_values)

ggplot(S2_data, aes(x = date, y = total_value_000s, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Investigating the results 
S2_data$year <- year(S2_data$date)

obs_data <- subset(S2_data, year >= 2014 & year <= 2024)
S2_2025_2035 <- subset(S2_data, year >= 2025 & year <= 2035)
S2_2035_2045 <- subset(S2_data, year > 2035 & year <= 2045)

# Totals
total_obs <- sum(obs_data$total_value_000s)
total_S2_2025_2035 <- sum(S2_2025_2035$total_value_000s)
total_S2_2035_2045 <- sum(S2_2035_2045$total_value_000s)

# Averages
avg_obs <- mean(obs_data$total_value_000s)
avg_S2_2025_2035 <- mean(S2_2025_2035$total_value_000s)
avg_S2_2035_2045 <- mean(S2_2035_2045$total_value_000s)

# Print
cat("Total (Observed 2014–2024):", total_obs, "\n")
cat("Total (S2 Predicted 2025–2035):", total_S2_2025_2035, "\n")
cat("Total (S2 Predicted 2035–2045):", total_S2_2035_2045, "\n\n")

cat("Average (Observed 2014–2024):", avg_obs, "\n")
cat("Average (S2 Predicted 2025–2035):", avg_S2_2025_2035, "\n")
cat("Average (S2 Predicted 2035–2045):", avg_S2_2035_2045, "\n")


# Scenario 3 --------------------------------------------------------------

load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_3_filtered_standardised.RData")

# predicting value based on the scenario 1 temperatures 
scenario_3_filtered$predicted_value <- predict(SSTV.seasonal.lm, newdata = scenario_3_filtered)

ggplot(scenario_3_filtered, aes(x = date, y = predicted_value)) +
  geom_line(, colour = "red") +
  theme_minimal()

original_standard_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, standardised_value) |>
  mutate(source = "Observed")

predicted_standard_values <- scenario_3_filtered |>
  select(date, predicted_value) |>
  mutate(source = "Predicted") |> 
  rename(standardised_value = predicted_value)

S3_standardised_plot_data <- bind_rows(original_standard_values, predicted_standard_values)

ggplot(S3_standardised_plot_data, aes(x = date, y = standardised_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Un-standardising value 
mean_val <- mean(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)
sd_val <- sd(combined_standardised_data_for_regression_SSTV$total_value_000s, na.rm = TRUE)

scenario_3_filtered <- scenario_3_filtered |>
  mutate(
    predicted_value_unscaled = predicted_value * sd_val + mean_val
  )

original_values <- combined_standardised_data_for_regression_SSTV |>
  select(date, total_value_000s) |>
  mutate(source = "Observed")

predicted_values <- scenario_3_filtered |>
  select(date, predicted_value_unscaled) |>
  mutate(source = "Predicted") |> 
  rename(total_value_000s = predicted_value_unscaled)

S3_data <- bind_rows(original_values, predicted_values)

ggplot(S3_data, aes(x = date, y = total_value_000s, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "pink"))

# Investigating the results 
S3_data$year <- year(S3_data$date)

obs_data <- subset(S3_data, year >= 2014 & year <= 2024)
S3_2025_2035 <- subset(S3_data, year >= 2025 & year <= 2035)
S3_2035_2045 <- subset(S3_data, year > 2035 & year <= 2045)

# Totals
total_obs <- sum(obs_data$total_value_000s)
total_S3_2025_2035 <- sum(S3_2025_2035$total_value_000s)
total_S3_2035_2045 <- sum(S3_2035_2045$total_value_000s)

# Averages
avg_obs <- mean(obs_data$total_value_000s)
avg_S3_2025_2035 <- mean(S3_2025_2035$total_value_000s)
avg_S3_2035_2045 <- mean(S3_2035_2045$total_value_000s)

# Print
cat("Total (Observed 2014–2024):", total_obs, "\n")
cat("Total (S3 Predicted 2025–2035):", total_S3_2025_2035, "\n")
cat("Total (S3 Predicted 2035–2045):", total_S3_2035_2045, "\n\n")

cat("Average (Observed 2014–2024):", avg_obs, "\n")
cat("Average (S3 Predicted 2025–2035):", avg_S3_2025_2035, "\n")
cat("Average (S3 Predicted 2035–2045):", avg_S3_2035_2045, "\n")
