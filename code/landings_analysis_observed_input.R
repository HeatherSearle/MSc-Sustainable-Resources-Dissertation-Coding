# Load data ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(caret)
library(dplyr)

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

# Since they are basically the same, we will work with live_weight

## Check correlations ------------------------------------------------------

# Check annual correlations (exclude landed weight)
cor(annual_data[,-3])
corrplot(cor(annual_data[,-3]))
# Exclude 2024 since we are missing 6 months of temp data
cor(annual_data[-11,-3])
corrplot(cor(annual_data[-11,-3]))
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

# Read each file and update scenario column (already has scenario but has sst_ at the beginning)
scenario_1 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP1_2.6",
         date = floor_date(date, unit = "month"))

scenario_2 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP2_4.5",
         date = floor_date(date, unit = "month"))

scenario_3 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP5_8.5",
         date = floor_date(date, unit = "month"))

# Check which months had the max/min temperature
summary(scenario_1)

scenario_1[which.min(scenario_1$avg_temp), ]
scenario_2[which.min(scenario_2$avg_temp), ]
scenario_3[which.min(scenario_3$avg_temp), ]


scenario_1[which.max(scenario_1$avg_temp), ]
scenario_2[which.max(scenario_2$avg_temp), ]
scenario_3[which.max(scenario_3$avg_temp), ]


# Combine all into long format
monthly_sst_data <- bind_rows(scenario_1, scenario_2, scenario_3) |>
  mutate(year = as.numeric(format(date, "%Y")))

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
  filter(year %in% 2015:2023)
ggplot(monthly_combined_sst_data, aes(x = date, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(monthly_combined_sst_data$avg_temp)))

# Check annual data instead (note that observed data is missing half of 2024)
annual_combined_sst_data <- monthly_combined_sst_data |> 
  group_by(scenario, year) |> 
  summarise(avg_temp = mean(avg_temp, na.rm = TRUE), .groups = "drop") 
ggplot(annual_combined_sst_data, aes(x = year, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(ylim = c(0, max(annual_combined_sst_data$avg_temp)))
# Observed data is slight lower

# Lets quickly check about the oscillations for a specific year
ggplot(monthly_combined_sst_data, aes(x = date, y = avg_temp, col = scenario)) +
  geom_line() +
  coord_cartesian(
    xlim = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
    ylim = c(0, max(monthly_combined_sst_data$avg_temp)))
# The months are wrong in the scenario data - not anymore!


# Seasonal Regression Model  ----------------------------------------------
## formatting the data
# trimming the last 12 months as temp dat only avialble for 6 months of 2024
monthly_data_trimmed <- monthly_data[1:(nrow(monthly_data) - 12), ]

# adding month factor
monthly_data_trimmed$month <- format(monthly_data_trimmed$date, "%m")
monthly_data_trimmed$month_factor <- as.factor(monthly_data_trimmed$month)

# standardising (use these variables for standardising scenario data)
#  mean and sd for landed_weight_tonnes - as we are commenting on LANDINGS
mean_landings <- mean(monthly_data_trimmed$landed_weight_tonnes, na.rm = TRUE)
sd_landings <- sd(monthly_data_trimmed$landed_weight_tonnes, na.rm = TRUE)

#  mean and sd for avg_temperature
mean_temp <- mean(monthly_data_trimmed$avg_temp, na.rm = TRUE)
sd_temp <- sd(monthly_data_trimmed$avg_temp, na.rm = TRUE)

# Add z-score standardised columns to trimmed dataset
monthly_data_trimmed$standardised_monthly_landings <- 
  (monthly_data_trimmed$landed_weight_tonnes - mean_landings) / sd_landings

monthly_data_trimmed$standardised_monthly_temp <- 
  (monthly_data_trimmed$avg_temp - mean_temp) / sd_temp

### running the model
SSTL.seasonal.lm <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
                       data = monthly_data_trimmed)

summary(SSTL.seasonal.lm )
# adjusted r.squared = 0.964 - this is great! means much of the variance is explained by the model

# K-fold cross validation -------------------------------------------------
# checking if the model is decent 
ctrl <- trainControl(method = "cv", number = 10)
model <- train(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
               data = monthly_data_trimmed,
               method = "lm", trControl = ctrl)

summary(model) # Rsquared = 0.968


# Scenario 1 --------------------------------------------------------------

## formatting data 
# standardising using observed data mean and sd for observed temp
scenario_1$standardised_monthly_temp <- (scenario_1$avg_temp - mean_temp) / sd_temp

# adding month factor
scenario_1$month <- format(scenario_1$date, "%m")
scenario_1$month_factor <- as.factor(scenario_1$month)

# trimming for 2024 July-2050 only 
scenario_1_trimmed <- scenario_1 |> 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))

# predicting landings based on the scenario 1 temperatures 
scenario_1_trimmed$predicted_landings <- predict(SSTL.seasonal.lm, newdata = scenario_1_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_1_trimmed <- scenario_1_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) |> 
  select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't need to unstandardise just use live_weight_tonnes and avg_temp

## plot monthly predictions-----------
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

# stt and monthly predictions 
ggplot(scenario_1_trimmed, aes(x = date)) +
  geom_line(aes(y = predicted_landings_real), color = "red", size = 1) +
  geom_line(aes(y = avg_temp), color = "blue", linetype = "dashed") +
  scale_y_continuous(
    name = "Predicted Landings",
    sec.axis = sec_axis(~ ., name = "Sea Surface Temperature (scaled)")
  ) +
  theme_minimal() +
  labs(title = "Monthly Predicted Landings and Sea Surface Temperature")

## predicted annual totals ----------
scenario_1_annual <- scenario_1_trimmed %>%
  mutate(year = year(date)) %>%
  group_by(scenario, year) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_1_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_1_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "blue") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## observed vs predicted ---------------------------------------------------

original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real <- scenario_1_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real)

S1_observed_predicted_data <- bind_rows(original_landings, predicted_landings_real)

ggplot(S1_observed_predicted_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))

# looking at annual observed vs predicted

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real_annual <- scenario_1_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings)

S1_observed_predicted_annual_data <- bind_rows(original_landings_annual, predicted_landings_real_annual)

ggplot(S1_observed_predicted_annual_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))


# Scenario 2 --------------------------------------------------------------

## formatting data 
# standardising using observed data mean and sd for observed temp
scenario_2$standardised_monthly_temp <- (scenario_2$avg_temp - mean_temp) / sd_temp

# adding month factor
scenario_2$month <- format(scenario_2$date, "%m")
scenario_2$month_factor <- as.factor(scenario_2$month)

# trimming for 2024 July-2050 only 
scenario_2_trimmed <- scenario_2 |> 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))

# predicting landings based on the scenario 1 temperatures 
scenario_2_trimmed$predicted_landings <- predict(SSTL.seasonal.lm, newdata = scenario_2_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_2_trimmed <- scenario_2_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) |> 
  select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't need to unstandardise just use live_weight_tonnes and avg_temp

## plot monthly predictions-----------
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

# stt and monthly predictions 
ggplot(scenario_2_trimmed, aes(x = date)) +
  geom_line(aes(y = predicted_landings_real), color = "red", size = 1) +
  geom_line(aes(y = avg_temp), color = "blue", linetype = "dashed") +
  scale_y_continuous(
    name = "Predicted Landings",
    sec.axis = sec_axis(~ ., name = "Sea Surface Temperature (scaled)")
  ) +
  theme_minimal() +
  labs(title = "Monthly Predicted Landings and Sea Surface Temperature")

## predicted annual totals ----------
scenario_2_annual <- scenario_2_trimmed %>%
  mutate(year = year(date)) %>%
  group_by(scenario, year) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_2_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_2_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "green") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## observed vs predicted ---------------------------------------------------

original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

S2_predicted_landings_real <- scenario_2_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real)

S2_observed_predicted_data <- bind_rows(original_landings, S2_predicted_landings_real)

ggplot(S2_observed_predicted_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen"))

# looking at annual observed vs predicted

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

S2_predicted_landings_real_annual <- scenario_2_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings)

S2_observed_predicted_annual_data <- bind_rows(original_landings_annual, S2_predicted_landings_real_annual)

ggplot(S2_observed_predicted_annual_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen"))


# scenario 3 --------------------------------------------------------------

## formatting data 
# standardising using observed data mean and sd for observed temp
scenario_3$standardised_monthly_temp <- (scenario_3$avg_temp - mean_temp) / sd_temp

# adding month factor
scenario_3$month <- format(scenario_3$date, "%m")
scenario_3$month_factor <- as.factor(scenario_3$month)

# trimming for 2024 July-2050 only 
scenario_3_trimmed <- scenario_3 |> 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))

# predicting landings based on the scenario 1 temperatures 
scenario_3_trimmed$predicted_landings <- predict(SSTL.seasonal.lm, newdata = scenario_3_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_3_trimmed <- scenario_3_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) |> 
  select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't need to unstandardise just use live_weight_tonnes and avg_temp

## plot monthly predictions-----------
ggplot(scenario_3_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# stt and monthly predictions 
ggplot(scenario_3_trimmed, aes(x = date)) +
  geom_line(aes(y = predicted_landings_real), color = "red", size = 1) +
  geom_line(aes(y = avg_temp), color = "blue", linetype = "dashed") +
  scale_y_continuous(
    name = "Predicted Landings",
    sec.axis = sec_axis(~ ., name = "Sea Surface Temperature (scaled)")
  ) +
  theme_minimal() +
  labs(title = "Monthly Predicted Landings and Sea Surface Temperature")

## predicted annual totals ----------
scenario_3_annual <- scenario_3_trimmed %>%
  mutate(year = year(date)) %>%
  group_by(scenario, year) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_3_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_3_annual, aes(x = year, y = total_landings)) +
  geom_line( colour = "red") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## observed vs predicted ---------------------------------------------------

original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

S3_predicted_landings_real <- scenario_3_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real)

S3_observed_predicted_data <- bind_rows(original_landings, S3_predicted_landings_real)

ggplot(S3_observed_predicted_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red"))

# looking at annual observed vs predicted

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

S3_predicted_landings_real_annual <- scenario_3_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings)

S3_observed_predicted_annual_data <- bind_rows(original_landings_annual, S3_predicted_landings_real_annual)

ggplot(S3_observed_predicted_annual_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red"))


# plotting all 3 scenarios ------------------------------------------------

# Add scenario labels before binding
S1 <- S1_observed_predicted_annual_data %>%
  mutate(scenario = "S1")

S2 <- S2_observed_predicted_annual_data %>%
  mutate(scenario = "S2")

S3 <- S3_observed_predicted_annual_data %>%
  mutate(scenario = "S3")

combined_scenario_data <- bind_rows(S1, S2, S3)

# plotting the scenarios next to each other 
ggplot(combined_scenario_data, aes(x = year, y = landed_weight_tonnes, color = scenario)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ scenario) +  # One panel per scenario
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("S1" = "blue", "S2" = "green", "S3" = "red")) +
  theme_minimal()

## fun chart sorry fan chart -----------


# trimming for predicted values only 
combined_scenario_data_trimmed <- combined_scenario_data |> 
  filter(source == "Predicted")

ggplot() +
  # Observed line
  geom_line(data = original_landings_annual, aes(x = year, y = landed_weight_tonnes),
            color = "black", linewidth = 1.2) +
  
  # Predicted lines for each scenario
  geom_line(data = combined_scenario_data_trimmed, aes(x = year, y = landed_weight_tonnes, color = scenario),
            linewidth = 1.2) +
  
  # Optional: smoother lines for predicted (if desired)
  # geom_smooth(data = predicted_data, aes(x = year, y = live_weight_tonnes, color = scenario),
  #             method = "loess", se = FALSE, size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c("S1" = "red", "S2" = "blue", "S3" = "green")) +
  theme_minimal() +
  labs(title = "Observed and Scenario-based Predicted Landings",
       x = "Year", y = "Live Weight (tonnes)", color = "Scenario") +
  scale_y_continuous(limits = c(0, NA))
