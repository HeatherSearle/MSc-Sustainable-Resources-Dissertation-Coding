# Load data ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)

load("data/tidy_landings_data/UK_lobster_landings.RData") 
 # load("data/tidy_SST_data/UK_average_monthly_temperature.RData")
load("data/tidy_SST_data/observed_average_monthly_temperature.RData")

# Check and combine data -----------------------------------------------------

head(UK_lobster_landings)
str(UK_lobster_landings)
summary(is.na(UK_lobster_landings)) # all NAs are in categories

head(observed_average_monthly_temperature)
summary(observed_average_monthly_temperature)
str(observed_average_monthly_temperature)
sum(is.na(observed_average_monthly_temperature))

# Categorical variables all look good
UK_lobster_landings |> 
  select(length_group, species_code, species_name, species_group) |> 
  distinct()

## Check monthly data ---------------------------------------------

# Check numerical variables monthly by date
monthly_data <- UK_lobster_landings |> 
  group_by(date, year, month) |> 
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), 
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE),
    value_000s = sum(value_000s, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  # Add in monthly data
  left_join(observed_average_monthly_temperature, by = "date") |> 
  # Added a total months variable to make it easier to check correlation
  mutate(
    total_months = as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) -
           (as.numeric(first(format(date, "%Y"))) * 12 + as.numeric(first(format(date, "%m"))))
  )

# Landed and live weight are very similar but look nice
ggplot(monthly_data, aes(x = date, y = live_weight_tonnes)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$live_weight_tonnes)))
ggplot(monthly_data, aes(x = date, y = landed_weight_tonnes)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$landed_weight_tonnes)))
# Live weight is normally bigger than landed weight but not be more than 0.6 tonnes any particular month
ggplot(monthly_data, aes(x = date, y = live_weight_tonnes-landed_weight_tonnes)) +
  geom_line()

# Value looks good
ggplot(monthly_data, aes(x = date, y = value_000s)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$value_000s)))

# Sea temperature has monthly variation but no major trend
ggplot(monthly_data, aes(x = date, y = avg_temp)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$avg_temp)))
# Noticed that last 6 months are missing!!!

# Can also check how monthly variation compares for different years
# Looks pretty similar for landings
ggplot(monthly_data, aes(x = month, y = live_weight_tonnes, group = year, col = year)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$live_weight_tonnes)))
# More difference for value - earlier years have noticeably lower value
# The difference is most noticeable in August where the value peaks
ggplot(monthly_data, aes(x = month, y = value_000s, group = year, col = year)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$value_000s)))
# Sea temperature
ggplot(monthly_data, aes(x = month, y = avg_temp, group = year, col = year)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_data$avg_temp)))

## Check annual ----------------------------------------------
# Check numerical variables by year
annual_data <- monthly_data |> 
  group_by(year) |> 
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), 
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE),
    value_000s = sum(value_000s, na.rm = TRUE),
    avg_temp = mean(avg_temp, na.rm = TRUE),
    .groups = "drop"
  )

# Landed and live weight are very similar. Look quite flat
ggplot(annual_data, aes(x = year, y = live_weight_tonnes)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_data$live_weight_tonnes)))
ggplot(annual_data, aes(x = year, y = landed_weight_tonnes)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_data$landed_weight_tonnes)))
# Live weight is normally bigger than landed weight but not be more than 2 tonnes any particular year
ggplot(annual_data, aes(x = year, y = live_weight_tonnes-landed_weight_tonnes)) +
  geom_line()

# Value looks good - it looks like its going up more than the landings
ggplot(annual_data, aes(x = year, y = value_000s)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_data$value_000s)))
# Could be due to inflation!!! Should remove inflation and check if value still goes up!

# Not much trend in avg temp
# It goes down in the final year but probably because peak months july + august are missing
ggplot(annual_data, aes(x = year, y = avg_temp)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_data$avg_temp)))

# Since they are basically the same, we will work with landed_weight

## Check correlations ------------------------------------------------------

# Check annual correlations (exclude live weight)
cor(annual_data[,-3])
corrplot(cor(annual_data[,-3]))
plot(cor(annual_data[,-3]))
# Basically no correlation between year and the weights which is expected from graphs
# And there is high correlation between year and value - expected from the graph
# Low correlation between weight and value which is unexpected
# What could be causing this?? Probably because weight is pretty static
# Not many trends coming from avg temp - slight negative correlation with year is potentially because of decrease in 2024

# Check monthly correlations (exclude date and live weight columns)
# Also need to filter out nas
cor(monthly_data[!is.na(monthly_data$avg_temp),-c(1, 4)])
corrplot(cor(monthly_data[!is.na(monthly_data$avg_temp),-c(1,4)]))
# Less correlation between year and value/weight which is expected since we have monthly variation
# Month has moderate correlation between weight and value which is expected since both peak in August then decrease
# Landed weight and value are very highly correlated because of the monthly trends
# Total months is only moderately correlated with value and not at all with landed weight which is again not surprising because of monthly variation
# Average temperature is correlated with other monthly vars like weight and value as expected


# Load SST Scenario Data --------------------------------------------------

# Read each file and add a scenario column
scenario_1 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP1_2.6")

scenario_2 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP2_4.5")

scenario_3 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP5_8.5")

# Check data
summary(scenario_1)

scenario_1[which.min(scenario_1$avg_temp), ]
scenario_1[which.max(scenario_1$avg_temp), ]

scenario_2[which.min(scenario_2$avg_temp), ]
scenario_2[which.max(scenario_2$avg_temp), ]

scenario_3[which.min(scenario_3$avg_temp), ]
scenario_3[which.max(scenario_3$avg_temp), ]

# Because of start and end dates we will filter to 2014-2047

# Combine all into long format and filter to start from 2014
monthly_sst_data <- bind_rows(scenario_1, scenario_2, scenario_3) |>
  mutate(year = as.numeric(format(date, "%Y"))) |> 
  filter(year %in% 2014:2047)

# Compare the scenarios
ggplot(monthly_sst_data, aes(x = date, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_sst_data$avg_temp)))

# Try aggregating to annual data to see difference more clearly
annual_sst_data <- monthly_sst_data |> 
  group_by(scenario, year) |> 
  summarise(avg_temp = mean(avg_temp, na.rm = TRUE), .groups = "drop") 

ggplot(annual_sst_data, aes(x = year, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_sst_data$avg_temp)))

# Now lets check against observed data
# First extract observed data and check it is in the right format
observed_sst_data <- monthly_data |> 
  select(date, year, avg_temp) |> 
  mutate(scenario = "Observed") |> 
  filter(!is.na(avg_temp)) |> 
  select(date, avg_temp, scenario, year)
summary(observed_sst_data)

# Combine and check monthly data
monthly_combined_sst_data <- bind_rows(monthly_sst_data, observed_sst_data) |> 
  filter(year %in% 2014:2024)
ggplot(monthly_combined_sst_data, aes(x = date, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_combined_sst_data$avg_temp)))
# Oscillations are out of sync

# Check annual data instead (note that observed data is missing half of 2024)
annual_combined_sst_data <- monthly_combined_sst_data |> 
  group_by(scenario, year) |> 
  summarise(avg_temp = mean(avg_temp, na.rm = TRUE), .groups = "drop") 
ggplot(annual_combined_sst_data, aes(x = year, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_combined_sst_data$avg_temp)))
# Observed data is significantly lower

# Lets quickly check about the oscillations
ggplot(monthly_combined_sst_data, aes(x = date, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(
    xlim = c(as.Date("2022-01-01"), as.Date("2022-12-31")),
    ylim = c(0, max(monthly_combined_sst_data$avg_temp)))
# The months are wrong in the scenario data

# Combine data ------------------------------------------------------------

combined_standardised_data_for_regression_SSTL <- inner_join(
  average_monthly_temperature,
  monthly_lobster_landings,
  by = c("date" = "month_start_date") # Join by 'date' from temp and 'month_start_date' from lobster
)

str(combined_standardised_data_for_regression_SSTL)

# Seasonal Regression Model  ----------------------------------------------
# Join by 'date' from temp and 'month_start_date' from lobster

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
model <- train(standardised_landings ~ standardised_avg_temp + month_factor,
               data = combined_standardised_data_for_regression_SSTL,
               method = "lm", trControl = ctrl)

print(model)


# Scenario 1 --------------------------------------------------------------
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_1_filtered_standardised.RData")

# predicting value based on the scenario 1 temperatures 
scenario_1_filtered$predicted_landings <- predict(SSTL.seasonal.lm, newdata = scenario_1_filtered)

ggplot(scenario_1_filtered, aes(x = date, y = predicted_landings)) +
  geom_line(, colour = "red") +
  theme_minimal()

original_standard_landings <- combined_standardised_data_for_regression_SSTL |>
  select(date, standardised_landings) |>
  mutate(source = "Observed")

predicted_standard_landings <- scenario_1_filtered |>
  select(date, predicted_landings) |>
  mutate(source = "Predicted") |> 
  rename(standardised_landings = predicted_landings)

S1_standardised_plot_data <- bind_rows(original_standard_landings, predicted_standard_landings)

ggplot(S1_standardised_plot_data, aes(x = date, y = standardised_landings, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))

# Un-standardising landings 
mean_land <- mean(combined_standardised_data_for_regression_SSTL$total_live_weight_tonnes, na.rm = TRUE)
sd_land <- sd(combined_standardised_data_for_regression_SSTL$total_live_weight_tonnes, na.rm = TRUE)

scenario_1_filtered <- scenario_1_filtered |>
  mutate(
    predicted_landings_unscaled = predicted_landings * sd_land + mean_land
  )

original_landings <- combined_standardised_data_for_regression_SSTL |>
  select(date, total_live_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings <- scenario_1_filtered |>
  select(date, predicted_landings_unscaled) |>
  mutate(source = "Predicted") |> 
  rename(total_live_weight_tonnes = predicted_landings_unscaled)

S1_data <- bind_rows(original_landings, predicted_landings)

ggplot(S1_data, aes(x = date, y = total_live_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))

# Investigating the results 
S1_data$year <- year(S1_data$date)

obs_data <- subset(S1_data, year >= 2014 & year <= 2024)
S1_2025_2035 <- subset(S1_data, year >= 2025 & year <= 2035)
S1_2035_2045 <- subset(S1_data, year > 2035 & year <= 2045)

# Totals
total_obs <- sum(obs_data$total_live_weight_tonnes)
total_S1_2025_2035 <- sum(S1_2025_2035$total_live_weight_tonnes)
total_S1_2035_2045 <- sum(S1_2035_2045$total_live_weight_tonnes)

# Averages
avg_obs <- mean(obs_data$total_live_weight_tonnes)
avg_S1_2025_2035 <- mean(S1_2025_2035$total_live_weight_tonnes)
avg_S1_2035_2045 <- mean(S1_2035_2045$total_live_weight_tonnes)

# Print
cat("Total (Observed 2014–2024):", total_obs, "\n")
cat("Total (S1 Predicted 2025–2035):", total_S1_2025_2035, "\n")
cat("Total (S1 Predicted 2035–2045):", total_S1_2035_2045, "\n\n")

cat("Average (Observed 2014–2024):", avg_obs, "\n")
cat("Average (S1 Predicted 2025–2035):", avg_S1_2025_2035, "\n")
cat("Average (S1 Predicted 2035–2045):", avg_S1_2035_2045, "\n")

S1_data %>%
  group_by(year) %>%
  summarise(mean = mean(total_live_weight_tonnes, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line()
