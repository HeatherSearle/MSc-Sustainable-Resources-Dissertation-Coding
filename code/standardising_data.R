library(dplyr)
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_SST_data/UK_temperature_data.RData")


# temperature data - monthly average and standardisation ------------------

average_monthly_temperature <- all_temperature_data |> 
  group_by(date) |>  
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE), .groups = "drop" 
  )

head(average_monthly_temperature)
tail(average_monthly_temperature)
str(average_monthly_temperature)
summary(average_monthly_temperature$avg_temp)

# standardising using Z-score
# Calculate the mean and standard deviation of the entire average monthly temperature series
mean_avg_temp <- mean(average_monthly_temperature$avg_temp, na.rm = TRUE)
sd_avg_temp <- sd(average_monthly_temperature$avg_temp, na.rm = TRUE)

# Apply Z-score standardization
average_monthly_temperature <- average_monthly_temperature |> 
  mutate(
    standardised_avg_temp = (avg_temp - mean_avg_temp) / sd_avg_temp
  )

# Inspect the result, particularly the new standardised_avg_temp column
head(average_monthly_temperature)
tail(average_monthly_temperature)
summary(average_monthly_temperature$standardised_avg_temp) # Mean should be very close to 0, SD very close to 1

save(
  average_monthly_temperature, 
  file = "data/tidy_SST_data/UK_average_standardised_monthly_temperature.RData"
)

# Lobster landings monthly totals and standardisation ---------------------
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/UK_lobster_landings.RData")

UK_lobster_landings <- UK_lobster_landings |>
  mutate(
    month_start_date = as.Date(format(date, "%Y-%m-01"))
  )

monthly_lobster_landings <- UK_lobster_landings |> 
  group_by(month_start_date) |> 
  summarise(
    total_live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE) # Summing and handling NAs
  ) |> 
  ungroup() 

# standardising 

# Calculate the mean and standard deviation of the entire monthly lobster landings series
mean_landings <- mean(monthly_lobster_landings$total_live_weight_tonnes, na.rm = TRUE)
sd_landings <- sd(monthly_lobster_landings$total_live_weight_tonnes, na.rm = TRUE)

# Apply Z-score standardization
monthly_lobster_landings <- monthly_lobster_landings |> 
  mutate(
    standardised_landings = (total_live_weight_tonnes - mean_landings) / sd_landings
  )

# Inspect the result
head(monthly_lobster_landings)
tail(monthly_lobster_landings)
summary(monthly_lobster_landings$standardised_landings) # Mean should be close to 0, SD close to 1
sd_avg_landings <- sd(monthly_lobster_landings$standardised_landings, na.rm = TRUE)

save(
  monthly_lobster_landings, 
  file = "data/standardised_data/monthly_lobster_landings.RData"
)


# Standardising lobster value and monthly totals  --------------------------------------------
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/UK_lobster_landings.RData")

UK_lobster_landings <- UK_lobster_landings |>
  mutate(
    month_start_date = as.Date(format(date, "%Y-%m-01"))
  )

monthly_lobster_value <- UK_lobster_landings |> 
  group_by(month_start_date) |> 
  summarise(
    total_value_000s = sum(value_000s, na.rm = TRUE) # Summing and handling NAs
  ) |> 
  ungroup() 

# standardising 

# Calculate the mean and standard deviation of the entire monthly lobster landings series
mean_value <- mean(monthly_lobster_value$total_value_000s, na.rm = TRUE)
sd_value <- sd(monthly_lobster_value$total_value_000s, na.rm = TRUE)

# Apply Z-score standardization
monthly_lobster_value <- monthly_lobster_value |> 
  mutate(
    standardised_value = (total_value_000s - mean_value) / sd_value
  )

# Inspect the result
head(monthly_lobster_value)
tail(monthly_lobster_value)
summary(monthly_lobster_value$standardised_value) # Mean should be close to 0, SD close to 1
sd_avg_value <- sd(monthly_lobster_value$standardised_value, na.rm = TRUE)

save(
  monthly_lobster_value, 
  file = "data/tidy_landings_data/monthly_standardised_lobster_value.RData"
)

# Standardising SST scenario data -----------------------------------------
# we must reuse the same mean and sd scores from the training data to ensure continuity 
# so ensure we have run the standardising code for temp data above to get mean_avg_temp and sd_avg_temp 

# S1
library(lubridate)
scenario_1_sst_data <- read.csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv")

scenario_1_sst_data <- scenario_1_sst_data |>
  mutate(
    standardised_avg_temp = (Mean_UK_SST - mean_avg_temp) / sd_avg_temp
  )

scenario_1_sst_data <- scenario_1_sst_data |> 
  mutate(
    month_factor = factor(sprintf("%02d", month(Date)))
  )

save(
  scenario_1_sst_data, 
  file = "data/standardised_data/scenario_1_sst_data.RData"
)

# S2 
scenario_2_sst_data <- read.csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv")

scenario_2_sst_data <- scenario_2_sst_data |>
  mutate(
    standardised_avg_temp = (Mean_UK_SST - mean_avg_temp) / sd_avg_temp
  )

scenario_2_sst_data <- scenario_2_sst_data |> 
  mutate(
    month_factor = factor(sprintf("%02d", month(date)))
  )

save(
  scenario_2_sst_data, 
  file = "data/standardised_data/scenario_2_sst_data.RData")

# S3 
scenario_3_sst_data <- read.csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv")

scenario_3_sst_data <- scenario_3_sst_data |>
  mutate(
    standardised_avg_temp = (Mean_UK_SST - mean_avg_temp) / sd_avg_temp
  )

scenario_3_sst_data <- scenario_3_sst_data |> 
  mutate(
    month_factor = factor(sprintf("%02d", month(date)))
  )

save(
  scenario_3_sst_data, 
  file = "data/standardised_data/scenario_3_sst_data.RData")
