# analysis for landings with model that takes into consideration year

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


# regression model --------------------------------------------------------

## formatting the data --------------------------------------------
# trimming the last 12 months as temp dat only avialble for 6 months of 2024
monthly_data_trimmed <- monthly_data[1:(nrow(monthly_data) - 12), ]

# adding month factor
monthly_data_trimmed$month <- format(monthly_data_trimmed$date, "%m")
monthly_data_trimmed$month_factor <- as.factor(monthly_data_trimmed$month)

# standardising (use these variables for standardising scenario data)
#  mean and sd for LANDINGS
mean_landings <- mean(monthly_data_trimmed$landed_weight_tonnes, na.rm = TRUE)
sd_landings <- sd(monthly_data_trimmed$landed_weight_tonnes, na.rm = TRUE)

#  mean and sd for avg_temperature
mean_temp <- mean(monthly_data_trimmed$avg_temp, na.rm = TRUE)
sd_temp <- sd(monthly_data_trimmed$avg_temp, na.rm = TRUE)

# mean and sd for year
# mean_year <- mean(monthly_data_trimmed$year, na.rm = TRUE)
# sd_year <- sd(monthly_data_trimmed$year, na.rm = TRUE)

# Add z-score standardised columns to trimmed dataset
monthly_data_trimmed$standardised_monthly_landings <- 
  (monthly_data_trimmed$landed_weight_tonnes - mean_landings) / sd_landings

monthly_data_trimmed$standardised_monthly_temp <- 
  (monthly_data_trimmed$avg_temp - mean_temp) / sd_temp

# monthly_data_trimmed$standardised_year <- 
#   (monthly_data_trimmed$year - mean_year) / sd_year

# #standardising year NEW
# monthly_data_trimmed$standardised_year <- scale(monthly_data_trimmed$year)
# year_mean <- attr(scale(monthly_data_trimmed$year), "scaled:center")
# year_sd <- attr(scale(monthly_data_trimmed$year), "scaled:scale")


## run model ---------------------------------------------------------------
# lobster.lm <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor + year,
#                          data = monthly_data_trimmed)

#lobsterlandings.lm <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor + standardised_year,
                #   data = monthly_data_trimmed)

lobsterlandings.lm <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
                         data = monthly_data_trimmed)


summary(lobsterlandings.lm )
# adjusted rsquared = 0.96

## k-fold validation -------------------------------------------------------

ctrl1 <- trainControl(method = "cv", number = 10)
model1 <- train(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
                data = monthly_data_trimmed,
                method = "lm", trControl = ctrl1)

summary(model1) # Rsquared = 0.97

## other model investigations -----------
anova(lobsterlandings.lm)

# scenario 1 --------------------------------------------------------------

## formatting data 
# standardising using observed data mean and sd for observed temp
scenario_1$standardised_monthly_temp <- (scenario_1$avg_temp - mean_temp) / sd_temp

#standardising year NEW
# scenario_1$standardised_year <- (scenario_1$year - mean_year) / sd_year

# adding month factor
scenario_1$month <- format(scenario_1$date, "%m")
scenario_1$month_factor <- as.factor(scenario_1$month)

# trimming for 2024 July-2050 only 
scenario_1_trimmed <- scenario_1 |> 
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))

# predicting landings based on the scenario 1 temperatures 
scenario_1_trimmed$predicted_landings <- predict(lobsterlandings.lm, newdata = scenario_1_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_1_trimmed <- scenario_1_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 

# #NEW
# scenario_1_trimmed$year <- (scenario_1_trimmed$standardised_year * year_sd) + year_mean

# select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't 
# need to unstandardise just use real_value and avg_temp

##  monthly totals -----------
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_1_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## annual totals ----------
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

saveRDS(scenario_1_annual, "scenario_1_annual_landings.rds")

## observed and predicted ---------------------------------------------------
last_observed <- monthly_data_trimmed %>%
  filter(date == max(date)) %>%
  select(date, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

#monthly
original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real <- scenario_1_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real) |> 
  bind_rows(last_observed)

S1_landings_data <- bind_rows(original_landings, predicted_landings_real)

ggplot(S1_landings_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue"))

# annual
last_observed <- annual_data %>%
  filter(year == max(year)) %>%
  select(year, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real_annual <- scenario_1_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings) |> 
  bind_rows(last_observed)

S1_annual_landings_data <- bind_rows(original_landings_annual, predicted_landings_real_annual) 


## S1 plot -----------------------------------------------------------------


ggplot(S1_annual_landings_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) + 
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(2000, NA)) +
  scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue")) +
  # scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed")) +
  theme_minimal() +
  # scale_color_manual(values = c("Observed" = "darkblue", "Predicted" = "lightblue")) +
  labs(title = "Observed and Predicted Lobster Landings (tonnes)",
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
scenario_2_trimmed$predicted_landings <- predict(lobsterlandings.lm, newdata = scenario_2_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_2_trimmed <- scenario_2_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 
# select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't 
# need to unstandardise just use real_value and avg_temp

##  monthly totals -----------
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_2_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## annual totals ----------
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

saveRDS(scenario_2_annual, "scenario_2_annual_landings.rds")

## observed and predicted ---------------------------------------------------
last_observed <- monthly_data_trimmed %>%
  filter(date == max(date)) %>%
  select(date, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

#monthly
original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real <- scenario_2_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real) |> 
  bind_rows(last_observed)

S2_landings_data <- bind_rows(original_landings, predicted_landings_real)

ggplot(S2_landings_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen"))

# annual
last_observed <- annual_data %>%
  filter(year == max(year)) %>%
  select(year, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real_annual <- scenario_2_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings) |> 
  bind_rows(last_observed)

S2_annual_landings_data <- bind_rows(original_landings_annual, predicted_landings_real_annual)


## S2 plot -----------------------------------------------------------------


ggplot(S2_annual_landings_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(2000, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkgreen", "Predicted" = "lightgreen")) +
  labs(title = "Observed and Predicted Lobster Landings (tonnes)",
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

# predicting landings based on the scenario 3 temperatures 
scenario_3_trimmed$predicted_landings <- predict(lobsterlandings.lm, newdata = scenario_3_trimmed)


## destandardising ---------------------------------------------------------
# also getting rid of extra unused variables 
scenario_3_trimmed <- scenario_3_trimmed |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 
# select(-sd_temp, -n_cells, -se_temp)

# monthly data also includes the unstandardised variables so don't 
# need to unstandardise just use real_value and avg_temp

##  monthly totals -----------
ggplot(scenario_3_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line(, colour = "red") +
  theme_minimal()

# monthly plot scaled to 0 - to see the overall trend
ggplot(scenario_3_trimmed, aes(x = date, y = predicted_landings_real)) +
  geom_line( colour = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

## annual totals ----------
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

saveRDS(scenario_3_annual, "scenario_3_annual_landings.rds")

## observed and predicted ---------------------------------------------------
last_observed <- monthly_data_trimmed %>%
  filter(date == max(date)) %>%
  select(date, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

#monthly
original_landings <- monthly_data_trimmed |>
  select(date, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real <- scenario_3_trimmed |>
  select(date, predicted_landings_real) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = predicted_landings_real) |> 
  bind_rows(last_observed)

S3_landings_data <- bind_rows(original_landings, predicted_landings_real)

ggplot(S3_landings_data, aes(x = date, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red"))

# annual
last_observed <- annual_data %>%
  filter(year == max(year)) %>%
  select(year, landed_weight_tonnes) %>%
  mutate(source = "Predicted")

original_landings_annual <- annual_data |>
  select(year, landed_weight_tonnes) |>
  mutate(source = "Observed")

predicted_landings_real_annual <- scenario_3_annual |>
  select(year, total_landings) |>
  mutate(source = "Predicted") |> 
  rename(landed_weight_tonnes = total_landings) |> 
  bind_rows(last_observed)

S3_annual_landings_data <- bind_rows(original_landings_annual, predicted_landings_real_annual)


## S3 plot -----------------------------------------------------------------


ggplot(S3_annual_landings_data, aes(x = year, y = landed_weight_tonnes, color = source)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_smooth(se = FALSE, color = "black", size = 0.5) +
  scale_y_continuous(limits = c(2000, NA)) +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "darkred", "Predicted" = "red")) +
  labs(title = "Observed and Predicted Lobster Landings (tonnes)",
       subtitle = "SSP5-8.5")

# 2024 landings ----------------------------------------------
sum_2024_first_half_obs_L <- monthly_data_cpi %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_real_landings = sum(landed_weight_tonnes, na.rm = TRUE))
# 	1075.227 - actual 

# predicting landings based on the scenario 1 temperatures 
scenario_1$predicted_landings <- predict(, newdata = scenario_1)

scenario_1 <- scenario_1 |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 

sum_2024_first_half_S1_L <- scenario_1 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE))
# 		1088.403

#S2
scenario_2$predicted_landings <- predict(realvalue.lm, newdata = scenario_2)

scenario_2 <- scenario_2 |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 

sum_2024_first_half_S2_L <- scenario_2 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE))
# 		1162.7

#S3
scenario_3$predicted_landings <- predict(realvalue.lm, newdata = scenario_3)

scenario_3 <- scenario_3 |> 
  mutate(predicted_landings_real = (predicted_landings * sd_landings) + mean_landings) 

sum_2024_first_half_S3_L <- scenario_3 %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-06-30")) %>%
  summarise(total_landings = sum(predicted_landings_real, na.rm = TRUE))
# 		1059.115