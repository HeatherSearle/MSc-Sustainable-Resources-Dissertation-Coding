# conducting value analysis including a year variable in the model 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(caret)
library(dplyr)
library(readxl)


# load data ---------------------------------------------------------------


load("data/tidy_landings_data/UK_lobster_landings.RData") 
load("data/tidy_SST_data/observed_average_monthly_temperature.RData")


## monthly data ------------------------------------------------------------


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



## annual data -------------------------------------------------------------

annual_data <- monthly_data |> 
  group_by(year) |> 
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), 
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE),
    value_000s = sum(value_000s, na.rm = TRUE),
    avg_temp = mean(avg_temp, na.rm = TRUE),
    .groups = "drop"
  )


## scenario data -----------------------------------------------------------

scenario_1 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP1_2.6",
         date = floor_date(date, unit = "month"),
         year = year(date))

scenario_2 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP2_4.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

scenario_3 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv") %>%
  mutate(date = as.Date(date),
         scenario = "SSP5_8.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

## adjusting for inflation - CPI index -------------------------------------
cpi_raw <- read_excel("data/cpi_uk_ons.xlsx", sheet = "filtered_data")

cpi_clean <- cpi_raw |> 
  mutate(date = parse_date_time(date, orders = "ym"),
         month = month(date))

monthly_data_cpi <- monthly_data |> 
  left_join(cpi_clean, by = "date")

base_cpi <- cpi_clean |> 
  filter(date == as.Date("2015-07-01")) |> 
  pull(cpi_index)

monthly_data_cpi <- monthly_data_cpi |> 
  mutate(
    real_value = value_000s * (base_cpi / cpi_index)
  )

ggplot((monthly_data_cpi), aes(x = date, y = real_value)) +
  geom_line(, colour = "red") +
  theme_minimal()

# filtering out the half year 2024
annual_data_cpi <- monthly_data_cpi |> 
  filter(year >= 2014, year <= 2023) |> 
  group_by(year) |> 
  summarise(
    annual_value_nominal = sum(value_000s, na.rm = TRUE),
    annual_value_real = sum(real_value, na.rm = TRUE),
    annual_landed_weight = sum(landed_weight_tonnes, na.rm = TRUE), .groups = "drop")


ggplot((annual_data_cpi), aes(x = year, y = annual_value_real)) +
  geom_line(colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

### price per tonne ------------------------------
annual_data_cpi <- annual_data_cpi |> 
  mutate(price_per_tonne = annual_value_real / annual_landed_weight)

ggplot(annual_data_cpi, aes(x = year, y = price_per_tonne)) +
  geom_line(colour = "red") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

annual_data_cpi[which.min(annual_data_cpi$price_per_tonne), ]
annual_data_cpi[which.max(annual_data_cpi$price_per_tonne), ]
mean(annual_data_cpi$price_per_tonne) # 12.12752 


# Regression Model  ----------------------------------------------


## formatting the data --------------------------------------------
# trimming the last 12 months as temp dat only avialble for 6 months of 2024
monthly_data_cpi_trimmed <- monthly_data_cpi[1:(nrow(monthly_data_cpi) - 12), ]

# adding month factor
monthly_data_cpi_trimmed$month <- format(monthly_data_cpi_trimmed$date, "%m")
monthly_data_cpi_trimmed$month_factor <- as.factor(monthly_data_cpi_trimmed$month)

# standardising (use these variables for standardising scenario data)
#  mean and sd for REAL VALUE
mean_value <- mean(monthly_data_cpi_trimmed$real_value, na.rm = TRUE)
sd_value <- sd(monthly_data_cpi_trimmed$real_value, na.rm = TRUE)

#  mean and sd for avg_temperature
mean_temp <- mean(monthly_data_cpi_trimmed$avg_temp, na.rm = TRUE)
sd_temp <- sd(monthly_data_cpi_trimmed$avg_temp, na.rm = TRUE)

# Add z-score standardised columns to trimmed dataset
monthly_data_cpi_trimmed$standardised_monthly_value <- 
  (monthly_data_cpi_trimmed$real_value - mean_value) / sd_value

monthly_data_cpi_trimmed$standardised_monthly_temp <- 
  (monthly_data_cpi_trimmed$avg_temp - mean_temp) / sd_temp

#standardising year NEW
# monthly_data_cpi_trimmed$standardised_year <- scale(monthly_data_cpi_trimmed$year)
# year_mean <- attr(scale(monthly_data_cpi_trimmed$year), "scaled:center")
# year_sd <- attr(scale(monthly_data_cpi_trimmed$year), "scaled:scale")


## run model ---------------------------------------------------------------

lobstervalue.lm <- lm(standardised_monthly_value ~ standardised_monthly_temp + 
                        month_factor,
                            data = monthly_data_cpi_trimmed)

summary(lobstervalue.lm )
# Adjusted R-squared:  0.8855 

anova(lobstervalue.lm)

## k-fold validation -------------------------------------------------------

ctrl2 <- trainControl(method = "cv", number = 10)
model2 <- train(standardised_monthly_value ~ standardised_monthly_temp + month_factor + year,
                data = monthly_data_cpi_trimmed,
                method = "lm", trControl = ctrl2)

summary(model2) # Rsquared = 0.90 


# scenario 1 --------------------------------------------------------------

## formatting data 
# standardising using observed data mean and sd for observed temp
scenario_1$standardised_monthly_temp <- (scenario_1$avg_temp - mean_temp) / sd_temp

# adding month factor
scenario_1$month <- format(scenario_1$date, "%m")
scenario_1$month_factor <- as.factor(scenario_1$month)

#adding a year column 
scenario_1 <- scenario_1 |> 
  mutate(year = year(date))

# #standardising year NEW
# scenario_1$standardised_year <- scale(scenario_1$year)

# trimming for 2024 July-2050 only 
scenario_1_trimmed <- scenario_1 |> 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))

# predicting value based on the scenario 1 temperatures 
scenario_1_trimmed$predicted_value <- predict(lobstervalue.lm, newdata = scenario_1_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_1_trimmed <- scenario_1_trimmed |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value) 
# select(-sd_temp, -n_cells, -se_temp)

#NEW
# scenario_1_trimmed$year <- (scenario_1_trimmed$standardised_year * year_sd) + year_mean

# monthly data also includes the unstandardised variables so don't 
# need to unstandardise just use real_value and avg_temp

##  monthly totals -----------
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_value_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_value_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## annual totals ----------
scenario_1_annual <- scenario_1_trimmed %>%
  mutate(year = year(date)) %>%
  group_by(scenario, year) %>%
  summarise(total_value = sum(predicted_value_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_1_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_1_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "blue") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

saveRDS(scenario_1_annual, "scenario_1_annual_value.rds")

## observed and predicted ---------------------------------------------------
last_observed <- monthly_data_cpi_trimmed %>%
  filter(date == max(date)) %>%
  select(date, real_value) %>%
  mutate(source = "Predicted")

#monthly
original_value <- monthly_data_cpi_trimmed |>
  select(date, real_value) |>
  mutate(source = "Observed")

predicted_value_real <- scenario_1_trimmed |>
  select(date, predicted_value_real) |>
  mutate(source = "Predicted") |> 
  rename(real_value = predicted_value_real) |> 
  bind_rows(last_observed)

S1_observed_predicted_data <- bind_rows(original_value, predicted_value_real)

ggplot(S1_observed_predicted_data, aes(x = date, y = real_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))

# annual
last_observed <- annual_data_cpi %>%
  filter(year == max(year)) %>%
  select(year, annual_value_real) %>%
  mutate(source = "Predicted")

original_value_annual <- annual_data_cpi |>
  select(year, annual_value_real) |>
  mutate(source = "Observed")

predicted_value_real_annual <- scenario_1_annual |>
  select(year, total_value) |>
  mutate(source = "Predicted") |> 
  rename(annual_value_real = total_value) |> 
  bind_rows(last_observed)

S1_observed_predicted_annual_data <- bind_rows(original_value_annual, predicted_value_real_annual)


## S1 plot -----------------------------------------------------------------


ggplot(S1_observed_predicted_annual_data, aes(x = year, y = annual_value_real, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2023, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue")) +
  labs(title = "Observed and Predicted Lobster Value (000s)",
       subtitle = "SSP1-2.6")

# scenario 2 --------------------------------------------------------------

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
scenario_2_trimmed$predicted_value <- predict(lobstervalue.lm, newdata = scenario_2_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_2_trimmed <- scenario_2_trimmed |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value)  
# select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't need to unstandardise just use live_weight_tonnes and avg_temp

## plot monthly predictions-----------
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_value_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_value_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

# stt and monthly predictions 
ggplot(scenario_2_trimmed, aes(x = date)) +
  geom_line(aes(y = predicted_value_real), color = "red", size = 1) +
  geom_line(aes(y = avg_temp), color = "blue", linetype = "dashed") +
  scale_y_continuous(
    name = "Predicted value",
    sec.axis = sec_axis(~ ., name = "Sea Surface Temperature (scaled)")
  ) +
  theme_minimal() +
  labs(title = "Monthly Predicted value and Sea Surface Temperature")

## predicted annual totals ----------
scenario_2_annual <- scenario_2_trimmed %>%
  mutate(year = year(date)) %>%
  group_by(scenario, year) %>%
  summarise(total_value = sum(predicted_value_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_2_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_2_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "green") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

saveRDS(scenario_2_annual, "scenario_2_annual_value.rds")

## observed vs predicted ---------------------------------------------------
# monthly

last_observed <- monthly_data_cpi_trimmed %>%
  filter(date == max(date)) %>%
  select(date, real_value) %>%
  mutate(source = "Predicted")

original_value <- monthly_data_cpi_trimmed |>
  select(date, real_value) |>
  mutate(source = "Observed")

S2_predicted_value_real <- scenario_2_trimmed |>
  select(date, predicted_value_real) |>
  mutate(source = "Predicted") |> 
  rename(real_value = predicted_value_real) |> 
  bind_rows(last_observed)

S2_observed_predicted_data <- bind_rows(original_value, S2_predicted_value_real)

ggplot(S2_observed_predicted_data, aes(x = date, y = real_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen"))

# looking at annual observed vs predicted
last_observed <- annual_data_cpi %>%
  filter(year == max(year)) %>%
  select(year, annual_value_real) %>%
  mutate(source = "Predicted")

original_value_annual <- annual_data_cpi |>
  select(year, annual_value_real) |>
  mutate(source = "Observed")

S2_predicted_value_real_annual <- scenario_2_annual |>
  select(year, total_value) |>
  mutate(source = "Predicted") |> 
  rename(annual_value_real = total_value) |> 
  bind_rows(last_observed)

S2_observed_predicted_annual_data <- bind_rows(original_value_annual, S2_predicted_value_real_annual)


## S2 plot -----------------------------------------------------------------


ggplot(S2_observed_predicted_annual_data, aes(x = year, y = annual_value_real, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2023, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen")) +
  labs(title = "Observed and Predicted Lobster Value (000s)",
       subtitle = "SSP2-4.5")

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
scenario_3_trimmed$predicted_value <- predict(lobstervalue.lm, newdata = scenario_3_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_3_trimmed <- scenario_3_trimmed |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value) 
# select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't need to unstandardise just use live_weight_tonnes and avg_temp

## plot monthly predictions-----------
ggplot(scenario_3_trimmed, aes(x = date, y = predicted_value_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# stt and monthly predictions 
ggplot(scenario_3_trimmed, aes(x = date)) +
  geom_line(aes(y = predicted_value_real), color = "red", size = 1) +
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
  summarise(total_value = sum(predicted_value_real, na.rm = TRUE), .groups = "drop")

# plot annual predicted totals
ggplot(scenario_3_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "red") +
  theme_minimal()

# annual plot scaled to 0 - to see the overall trend
ggplot(scenario_3_annual, aes(x = year, y = total_value)) +
  geom_line( colour = "red") +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

saveRDS(scenario_3_annual, "scenario_3_annual_value.rds")

## observed vs predicted ---------------------------------------------------

# monthly 
last_observed <- monthly_data_cpi_trimmed %>%
  filter(date == max(date)) %>%
  select(date, real_value) %>%
  mutate(source = "Predicted")

original_value <- monthly_data_cpi_trimmed |>
  select(date, real_value) |>
  mutate(source = "Observed")

S3_predicted_value_real <- scenario_3_trimmed |>
  select(date, predicted_value_real) |>
  mutate(source = "Predicted") |> 
  rename(real_value = predicted_value_real) |> 
  bind_rows(last_observed)

S3_observed_predicted_data <- bind_rows(original_value, S3_predicted_value_real)

ggplot(S3_observed_predicted_data, aes(x = date, y = real_value, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red"))

# looking at annual observed vs predicted
last_observed <- annual_data_cpi %>%
  filter(year == max(year)) %>%
  select(year, annual_value_real) %>%
  mutate(source = "Predicted")

original_value_annual <- annual_data_cpi |>
  select(year, annual_value_real) |>
  mutate(source = "Observed")

S3_predicted_value_real_annual <- scenario_3_annual |>
  select(year, total_value) |>
  mutate(source = "Predicted") |> 
  rename(annual_value_real = total_value) |> 
  bind_rows(last_observed)

S3_observed_predicted_annual_data <- bind_rows(original_value_annual, S3_predicted_value_real_annual)


## S3 plot -----------------------------------------------------------------


ggplot(S3_observed_predicted_annual_data, aes(x = year, y = annual_value_real, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2023, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red")) +
  labs(title = "Observed and Predicted Lobster Value (000s)",
                subtitle = "SSP5-8.5")

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
ggplot(combined_scenario_data, aes(x = year, y = annual_value_real, color = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "gray40", linewidth = 0.8) +
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
  geom_line(data = original_value_annual, aes(x = year, y = annual_value_real),
            color = "black", linewidth = 1.2) +
  
  # Predicted lines for each scenario
  geom_line(data = combined_scenario_data_trimmed, aes(x = year, y = annual_value_real, color = scenario),
            linewidth = 1.2) +
  
  # Optional: smoother lines for predicted (if desired)
  # geom_smooth(data = predicted_data, aes(x = year, y = live_weight_tonnes, color = scenario),
  #             method = "loess", se = FALSE, size = 0.5, linetype = "dashed") +
  
  scale_color_manual(values = c("S1" = "red", "S2" = "blue", "S3" = "green")) +
  theme_minimal() +
  labs(title = "Observed and Scenario-based Predicted value",
       x = "Year", y = "Value 000s", color = "Scenario") +
  scale_y_continuous(limits = c(0, NA))


# checking 2024 -----------------------------------------------------------

sum_2024_first_half_obs <- monthly_data_cpi %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_real_value = sum(real_value, na.rm = TRUE))
# 	actual = 14018.98

# predicting value based on the scenario 1 temperatures 
scenario_1$predicted_value <- predict(lobstervalue.lm, newdata = scenario_1)

scenario_1 <- scenario_1 |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value) 

sum_2024_first_half_S1 <- scenario_1 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_nominal_value = sum(predicted_value_real, na.rm = TRUE))
# 		14244.14 - with year
# 13343.41 - without year


#S2
scenario_2$predicted_value <- predict(lobstervalue.lm, newdata = scenario_2)

scenario_2 <- scenario_2 |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value) 

sum_2024_first_half_S2 <- scenario_2 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_nominal_value = sum(predicted_value_real, na.rm = TRUE))
# 			14975.79 - with year
# 14320.36 - without year

#S3
scenario_3$predicted_value <- predict(lobstervalue.lm, newdata = scenario_3)

scenario_3 <- scenario_3 |> 
  mutate(predicted_value_real = (predicted_value * sd_value) + mean_value) 

sum_2024_first_half_S3 <- scenario_3 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_nominal_value = sum(predicted_value_real, na.rm = TRUE))
# 				13955.73 - with year
# 12958.31 - without year

