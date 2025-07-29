# Full Shellfish Landings and Value Analysis with Predictions and Summary Tables

# --- 0. Load Libraries ---------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)   # For year() and month() functions
library(tidyr)       # For expand_grid()
library(readr)       # For read_csv and write_csv
library(lmtest)      # For model testing
library(patchwork)   # For combining plots
library(caret)       # For machine learning utilities
library(purrr)       # For map_dfr and map functions
library(readxl)      # For read_excel (your CPI data source)
library(scales)      # For dollar_format in ggplot


# --- 1. Data Loading and Initial Preparation (Common to both Landings and Value) ----

# Load UK shellfish landings data
load("data/tidy_landings_data/UK_shellfish_landings.RData")
# Load observed average monthly temperature data
load("data/tidy_SST_data/observed_average_monthly_temperature.RData")

# Group by species_name and collect unique years for each (for informational purposes)
species_years <- UK_shellfish_landings %>%
  group_by(species_name) %>%
  summarise(years_present = paste(sort(unique(year)), collapse = ", "))

# Define common target species
target_species_common <- c("Crabs", "Norway Lobster", "European Lobster", "Scallops")

## Scenario data loading and initial processing ----------------
# SSP1-2.6 scenario data
scenario_1 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP1_2.6",
         date = floor_date(date, unit = "month"),
         year = year(date))

# SSP2-4.5 scenario data
scenario_2 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP2_4.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

# SSP5-8.5 scenario data
scenario_3 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP5_8.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

# Combine all scenario dataframes into a list for easy iteration
all_scenario_dfs <- list(scenario_1, scenario_2, scenario_3)


# --- 2. Landings Analysis (Tonnes) ------------------------------

message("\n--- Starting Landings Analysis (Tonnes) ---")

# Monthly data processing for Landings
monthly_data_landings <- UK_shellfish_landings |>
  group_by(date, year, month, species_name) |>
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE),
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE), # Target variable for this section
    value_000s = sum(value_000s, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(species_name %in% target_species_common) |>
  left_join(observed_average_monthly_temperature, by = "date") |>
  mutate(
    total_months = as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) -
      (as.numeric(first(format(date, "%Y"))) * 12 + as.numeric(first(format(date, "%m"))))
  )

monthly_data_landings$species_name <- as.factor(monthly_data_landings$species_name)

# Trim the last 12 months for training
monthly_data_trimmed_landings <- monthly_data_landings[1:(nrow(monthly_data_landings) - 12), ]
monthly_data_trimmed_landings$month <- format(monthly_data_trimmed_landings$date, "%m")
monthly_data_trimmed_landings$month_factor <- as.factor(monthly_data_trimmed_landings$month)

# Calculate SPECIES-SPECIFIC means and SDs for landings
species_landings_stats <- monthly_data_trimmed_landings %>%
  group_by(species_name) %>%
  summarise(
    mean_landings_species = mean(landed_weight_tonnes, na.rm = TRUE),
    sd_landings_species = sd(landed_weight_tonnes, na.rm = TRUE),
    .groups = "drop"
  )

monthly_data_trimmed_landings <- monthly_data_trimmed_landings %>%
  left_join(species_landings_stats, by = "species_name") %>%
  mutate(
    standardised_monthly_landings = (landed_weight_tonnes - mean_landings_species) / sd_landings_species
  )

# Calculate GLOBAL mean and SD for `avg_temp` for landings model
mean_temp_landings <- mean(monthly_data_trimmed_landings$avg_temp, na.rm = TRUE)
sd_temp_landings <- sd(monthly_data_trimmed_landings$avg_temp, na.rm = TRUE)
monthly_data_trimmed_landings$standardised_monthly_temp <-
  (monthly_data_trimmed_landings$avg_temp - mean_temp_landings) / sd_temp_landings


# Function to Fit LM and Predict for a Single Species Across Scenarios (Landings)
predict_lm_for_species_landings <- function(current_species_name,
                                            monthly_data_trimmed_all_species,
                                            scenario_list,
                                            mean_temp, sd_temp,
                                            species_landings_stats_df) {
  message(paste("Processing predictions for Landings for:", current_species_name))
  species_observed_data <- monthly_data_trimmed_all_species %>%
    filter(species_name == current_species_name)
  if (nrow(species_observed_data) < 10) {
    warning(paste("Not enough data for", current_species_name, ". Skipping model fitting."))
    return(NULL)
  }
  species_lm_model <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
                         data = species_observed_data)
  
  # Model summary (commented out print to reduce console output, but available in return)
  # message(paste0("-------- Model Summary for ", current_species_name, " (Landings) -------"))
  # print(summary(species_lm_model))
  
  current_species_stats <- species_landings_stats_df %>%
    filter(species_name == current_species_name)
  current_mean_landings <- current_species_stats$mean_landings_species
  current_sd_landings <- current_species_stats$sd_landings_species
  
  species_predictions_list <- list()
  for (i in seq_along(scenario_list)) {
    scenario_df <- scenario_list[[i]]
    scenario_name <- unique(scenario_df$scenario)
    scenario_df_prepared <- scenario_df %>%
      mutate(species_name = factor(current_species_name, levels = levels(monthly_data_trimmed_all_species$species_name))) %>%
      mutate(standardised_monthly_temp = (avg_temp - mean_temp) / sd_temp) %>%
      mutate(month = format(date, "%m"), month_factor = factor(month, levels = levels(monthly_data_trimmed_all_species$month_factor))) %>%
      filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))
    
    predictions_matrix <- predict(species_lm_model, newdata = scenario_df_prepared, interval = "prediction", level = 0.95)
    
    species_predictions_list[[i]] <- scenario_df_prepared %>%
      mutate(
        predicted_landings_real = (predictions_matrix[, "fit"] * current_sd_landings) + current_mean_landings,
        predicted_landings_lwr = (predictions_matrix[, "lwr"] * current_sd_landings) + current_mean_landings,
        predicted_landings_upr = (predictions_matrix[, "upr"] * current_sd_landings) + current_mean_landings,
        scenario = scenario_name
      ) %>%
      select(date, year, month, species_name, scenario,
             predicted_landings_real, predicted_landings_lwr, predicted_landings_upr)
  }
  bind_rows(species_predictions_list)
}

# Apply prediction function for Landings
all_species_predictions_lm_combined <- lapply(target_species_common, function(s_name) {
  predict_lm_for_species_landings(
    current_species_name = s_name,
    monthly_data_trimmed_all_species = monthly_data_trimmed_landings,
    scenario_list = all_scenario_dfs,
    mean_temp = mean_temp_landings,
    sd_temp = sd_temp_landings,
    species_landings_stats_df = species_landings_stats
  )
}) %>% bind_rows()

# Prepare observed data for landings plots and tables
annual_observed_all_species <- monthly_data_trimmed_landings %>%
  mutate(year = year(date)) %>%
  filter(year < 2024) %>%
  group_by(year, species_name) %>%
  summarise(value = sum(landed_weight_tonnes, na.rm = TRUE), lwr = NA, upr = NA, .groups = 'drop') %>%
  mutate(data_type = "Observed")

# Prepare predicted data for landings plots
t_crit_value_for_pi <- qt(0.975, df = 113) # Assuming same df for Landings as Value
annual_predicted_all_species <- all_species_predictions_lm_combined %>%
  mutate(year = year(date)) %>%
  rowwise() %>%
  mutate(
    pi_width_monthly = predicted_landings_upr - predicted_landings_lwr,
    se_pred_monthly = pi_width_monthly / (2 * t_crit_value_for_pi),
    var_pred_monthly = se_pred_monthly^2
  ) %>%
  ungroup() %>%
  group_by(year, scenario, species_name) %>%
  summarise(
    value = sum(predicted_landings_real, na.rm = TRUE),
    total_var_pred_annual = sum(var_pred_monthly, na.rm = TRUE),
    se_pred_annual = sqrt(total_var_pred_annual),
    lwr = value - (t_crit_value_for_pi * se_pred_annual),
    upr = value + (t_crit_value_for_pi * se_pred_annual),
    .groups = 'drop'
  ) %>%
  mutate(lwr = pmax(0, lwr)) %>%
  mutate(data_type = "Predicted")

## Generate Landings Plots (Crabs, Norway Lobster, European Lobster only) ----------------------
species_to_plot_landings <- c("Crabs", "Norway Lobster", "European Lobster")
scenario_names <- c("SSP1_2.6", "SSP2_4.5", "SSP5_8.5")

for (s_name in species_to_plot_landings) {
  for (s_scenario in scenario_names) {
    message(paste0("Generating Landings Plot for Species: '", s_name, "' and Scenario: '", s_scenario, "'"))
    current_species_observed <- annual_observed_all_species %>% filter(species_name == s_name)
    current_species_scenario_predicted <- annual_predicted_all_species %>% filter(species_name == s_name, scenario == s_scenario)
    last_observed_annual_point <- current_species_observed %>% filter(year == max(year))
    bridging_point_for_predicted_line <- last_observed_annual_point %>%
      mutate(data_type = "Predicted", scenario = s_scenario, lwr = value, upr = value)
    
    plot_data_individual <- bind_rows(current_species_observed, bridging_point_for_predicted_line, current_species_scenario_predicted) %>% arrange(year)
    
    p <- ggplot(plot_data_individual, aes(x = year, y = value)) +
      geom_line(aes(color = data_type, linetype = data_type), size = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill = data_type), alpha = 0.2) +
      labs(title = paste0("Annual Landings for ", s_name, ": Observed & Predicted (", s_scenario, " Scenario)"),
           x = "Year", y = "Total Annual Landings (tonnes)", color = "Data Type", fill = "Data Type") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(breaks = seq(min(plot_data_individual$year), max(plot_data_individual$year), by = 5)) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "black", alpha = 0.6) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_color_manual(values = c("Observed" = "darkgrey", "Predicted" = "darkblue")) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed")) +
      scale_fill_manual(values = c("Observed" = "transparent", "Predicted" = "darkblue")) 
    print(p)
  }
}


# --- 3. Value Analysis (£'000s) -------------------------------------

message("\n--- Starting Value Analysis (£'000s) ---")

# Monthly data processing for Value
monthly_data_value <- UK_shellfish_landings |>
  group_by(date, year, month, species_name) |>
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE),
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE),
    value_000s = sum(value_000s, na.rm = TRUE), # Target variable for this section
    .groups = "drop"
  ) |>
  filter(species_name %in% target_species_common) |>
  left_join(observed_average_monthly_temperature, by = "date") |>
  mutate(
    total_months = as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) -
      (as.numeric(first(format(date, "%Y"))) * 12 + as.numeric(first(format(date, "%m"))))
  )

monthly_data_value$species_name <- as.factor(monthly_data_value$species_name)

## --- CPI Adjustment ---------------------------------
cpi_raw <- read_excel("data/cpi_uk_ons.xlsx", sheet = "filtered_data")
cpi_clean <- cpi_raw |>
  mutate(date = parse_date_time(date, orders = "ym"), month_num = month(date))

monthly_data_value <- monthly_data_value |>
  left_join(cpi_clean %>% select(date, cpi_index), by = "date")

base_cpi_date <- as.Date("2015-07-01")
base_cpi <- cpi_clean |> filter(date == base_cpi_date) |> pull(cpi_index)

if (length(base_cpi) == 0 || is.na(base_cpi)) {
  stop(paste("Base CPI for date", base_cpi_date, "not found in cpi_clean data. Please check CPI data."))
}

monthly_data_value <- monthly_data_value |>
  mutate(real_value = value_000s * (base_cpi / cpi_index)) # real_value in £'000s (July 2015 prices)

# Trim the last 12 months for training
monthly_data_trimmed_value <- monthly_data_value[1:(nrow(monthly_data_value) - 12), ]
monthly_data_trimmed_value$month <- format(monthly_data_trimmed_value$date, "%m")
monthly_data_trimmed_value$month_factor <- as.factor(monthly_data_trimmed_value$month)

# Calculate SPECIES-SPECIFIC means and SDs for REAL VALUE
species_value_stats <- monthly_data_trimmed_value %>%
  group_by(species_name) %>%
  summarise(
    mean_value_species = mean(real_value, na.rm = TRUE),
    sd_value_species = sd(real_value, na.rm = TRUE),
    .groups = "drop"
  )

monthly_data_trimmed_value <- monthly_data_trimmed_value %>%
  left_join(species_value_stats, by = "species_name") %>%
  mutate(
    standardised_monthly_value = (real_value - mean_value_species) / sd_value_species
  )

# Calculate GLOBAL mean and SD for `avg_temp` for value model
mean_temp_value <- mean(monthly_data_trimmed_value$avg_temp, na.rm = TRUE)
sd_temp_value <- sd(monthly_data_trimmed_value$avg_temp, na.rm = TRUE)
monthly_data_trimmed_value$standardised_monthly_temp <-
  (monthly_data_trimmed_value$avg_temp - mean_temp_value) / sd_temp_value


# Function to Fit LM and Predict for a Single Species Across Scenarios (Value)
predict_lm_for_species_value <- function(current_species_name,
                                         monthly_data_trimmed_all_species,
                                         scenario_list,
                                         mean_temp, sd_temp,
                                         species_value_stats_df) {
  message(paste("Processing predictions for REAL VALUE for:", current_species_name))
  species_observed_data <- monthly_data_trimmed_all_species %>%
    filter(species_name == current_species_name)
  if (nrow(species_observed_data) < 10) {
    warning(paste("Not enough data for", current_species_name, ". Skipping model fitting."))
    return(NULL)
  }
  species_lm_model <- lm(standardised_monthly_value ~ standardised_monthly_temp + month_factor,
                         data = species_observed_data)
  
  # Model summary (commented out print to reduce console output, but available in return)
  # message(paste0("-------- Model Summary for ", current_species_name, " (Real Value) -------"))
  # print(summary(species_lm_model))
  
  current_species_stats <- species_value_stats_df %>%
    filter(species_name == current_species_name)
  current_mean_value <- current_species_stats$mean_value_species
  current_sd_value <- current_species_stats$sd_value_species
  
  species_predictions_list <- list()
  for (i in seq_along(scenario_list)) {
    scenario_df <- scenario_list[[i]]
    scenario_name <- unique(scenario_df$scenario)
    scenario_df_prepared <- scenario_df %>%
      mutate(species_name = factor(current_species_name, levels = levels(monthly_data_trimmed_all_species$species_name))) %>%
      mutate(standardised_monthly_temp = (avg_temp - mean_temp) / sd_temp) %>%
      mutate(month = format(date, "%m"), month_factor = factor(month, levels = levels(monthly_data_trimmed_all_species$month_factor))) %>%
      filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))
    
    predictions_matrix <- predict(species_lm_model, newdata = scenario_df_prepared, interval = "prediction", level = 0.95)
    
    species_predictions_list[[i]] <- scenario_df_prepared %>%
      mutate(
        predicted_real_value_real = (predictions_matrix[, "fit"] * current_sd_value) + current_mean_value,
        predicted_real_value_lwr = (predictions_matrix[, "lwr"] * current_sd_value) + current_mean_value,
        predicted_real_value_upr = (predictions_matrix[, "upr"] * current_sd_value) + current_mean_value,
        scenario = scenario_name
      ) %>%
      select(date, year, month, species_name, scenario,
             predicted_real_value_real, predicted_real_value_lwr, predicted_real_value_upr)
  }
  bind_rows(species_predictions_list)
}

# Apply prediction function for Value
all_species_predictions_lm_combined_value <- lapply(target_species_common, function(s_name) {
  predict_lm_for_species_value(
    current_species_name = s_name,
    monthly_data_trimmed_all_species = monthly_data_trimmed_value,
    scenario_list = all_scenario_dfs,
    mean_temp = mean_temp_value,
    sd_temp = sd_temp_value,
    species_value_stats_df = species_value_stats
  )
}) %>% bind_rows()

# Prepare observed data for value plots and tables
annual_observed_all_species_value <- monthly_data_trimmed_value %>%
  mutate(year = year(date)) %>%
  filter(year < 2024) %>%
  group_by(year, species_name) %>%
  summarise(value = sum(real_value, na.rm = TRUE), lwr = NA, upr = NA, .groups = 'drop') %>%
  mutate(data_type = "Observed")

# Prepare predicted data for value plots
t_crit_value_for_pi_value <- qt(0.975, df = 113) 
annual_predicted_all_species_value <- all_species_predictions_lm_combined_value %>%
  mutate(year = year(date)) %>%
  rowwise() %>%
  mutate(
    pi_width_monthly = predicted_real_value_upr - predicted_real_value_lwr,
    se_pred_monthly = pi_width_monthly / (2 * t_crit_value_for_pi_value),
    var_pred_monthly = se_pred_monthly^2
  ) %>%
  ungroup() %>%
  group_by(year, scenario, species_name) %>%
  summarise(
    value = sum(predicted_real_value_real, na.rm = TRUE),
    total_var_pred_annual = sum(var_pred_monthly, na.rm = TRUE),
    se_pred_annual = sqrt(total_var_pred_annual),
    lwr = value - (t_crit_value_for_pi_value * se_pred_annual),
    upr = value + (t_crit_value_for_pi_value * se_pred_annual),
    .groups = 'drop'
  ) %>%
  mutate(lwr = pmax(0, lwr)) %>%
  mutate(data_type = "Predicted")

## Generate Value Plots (Crabs, Norway Lobster, European Lobster only) ------------
species_to_plot_value <- c("Crabs", "Norway Lobster", "European Lobster")

for (s_name in species_to_plot_value) {
  for (s_scenario in scenario_names) {
    message(paste0("Generating Real Value Plot for Species: '", s_name, "' and Scenario: '", s_scenario, "'"))
    current_species_observed_value <- annual_observed_all_species_value %>% filter(species_name == s_name)
    current_species_scenario_predicted_value <- annual_predicted_all_species_value %>% filter(species_name == s_name, scenario == s_scenario)
    last_observed_annual_point_value <- current_species_observed_value %>% filter(year == max(year))
    bridging_point_for_predicted_line_value <- last_observed_annual_point_value %>%
      mutate(data_type = "Predicted", scenario = s_scenario, lwr = value, upr = value)
    
    plot_data_individual_value <- bind_rows(current_species_observed_value, bridging_point_for_predicted_line_value, current_species_scenario_predicted_value) %>% arrange(year)
    
    p <- ggplot(plot_data_individual_value, aes(x = year, y = value)) +
      geom_line(aes(color = data_type, linetype = data_type), size = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill = data_type), alpha = 0.2) +
      labs(title = paste0("Annual Real Value for ", s_name, ": Observed & Predicted (", s_scenario, " Scenario)"),
           x = "Year", y = "Total Annual Real Value (£'000s - July 2015 Prices)", color = "Data Type", fill = "Data Type") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(breaks = seq(min(plot_data_individual_value$year), max(plot_data_individual_value$year), by = 5)) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "black", alpha = 0.6) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_color_manual(values = c("Observed" = "darkgrey", "Predicted" = "darkblue")) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed")) +
      scale_fill_manual(values = c("Observed" = "transparent", "Predicted" = "darkblue")) 
    print(p)
  }
}


# --- 4.Summary Tables ---------------------

message("\n--- Generating and Saving Summary Tables ---")

# Ensure the 'tables' directory exists
if (!dir.exists("tables")) {
  dir.create("tables")
}

# Define the common species and scenarios for tables
table_species <- c("Crabs", "Norway Lobster", "European Lobster", "Scallops") # Include Scallops for tables
table_scenarios <- c("SSP1_2.6", "SSP2_4.5", "SSP5_8.5")

## --- Table 1: Max/Min Landings/Value (2024-2050) ------------

message("\n--- Generating Table 1: Predicted Annual Max/Min Landings/Value (2024-2050) ---")

# Landings Max/Min (Annual sums, then max/min of those sums)
table1_landings_prep <- all_species_predictions_lm_combined %>%
  filter(year >= 2024 & year <= 2050) %>%
  group_by(species_name, scenario, year) %>% # Group by year first to get annual sums
  summarise(Annual_Sum_Landings = sum(predicted_landings_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(
    `Max Annual Landings (tonnes)` = max(Annual_Sum_Landings, na.rm = TRUE),
    `Min Annual Landings (tonnes)` = min(Annual_Sum_Landings, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Species = species_name, Scenario = scenario)

# Value Max/Min (Annual sums, then max/min of those sums)
table1_value_prep <- all_species_predictions_lm_combined_value %>%
  filter(year >= 2024 & year <= 2050) %>%
  group_by(species_name, scenario, year) %>% # Group by year first to get annual sums
  summarise(Annual_Sum_Value = sum(predicted_real_value_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(
    `Max Annual Value (£'000s)` = max(Annual_Sum_Value, na.rm = TRUE),
    `Min Annual Value (£'000s)` = min(Annual_Sum_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Species = species_name, Scenario = scenario)

# Combine Landings and Value into a single wide table (Table 1)
table1_combined_wide <- full_join(table1_landings_prep, table1_value_prep, by = c("Species", "Scenario")) %>%
  arrange(Species, Scenario)

print(table1_combined_wide)
write_csv(table1_combined_wide, "tables/Table1_Predicted_Annual_Max_Min_Landings_Value_2024-2050.csv")
message("Table 1 saved to tables/Table1_Predicted_Annual_Max_Min_Landings_Value_2024-2050.csv")


##--- Table 2: Percentage Change(2023 Baseline) --------------------

message("\n--- Generating Table 2: Percentage Change in Average Annual Landings/Value (2023 Baseline) ---")

# Define baseline and future periods
baseline_years <- 2018:2023 # Using 2018-2023 as a representative baseline from observed data
future_years <- 2024:2050

# Landings Percentage Change Preparation
# Calculate baseline average annual landings (from observed data)
baseline_landings_avg <- annual_observed_all_species %>%
  filter(year %in% baseline_years) %>%
  group_by(species_name) %>%
  summarise(Baseline_Avg_Landings = mean(value, na.rm = TRUE), .groups = 'drop')

# Calculate future average annual landings (from predicted data)
future_landings_avg <- all_species_predictions_lm_combined %>%
  filter(year %in% future_years) %>%
  group_by(species_name, scenario, year) %>% # Sum monthly to annual first
  summarise(Annual_Sum_Landings = sum(predicted_landings_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(Future_Avg_Landings = mean(Annual_Sum_Landings, na.rm = TRUE), .groups = 'drop') # Then average annual

# Join and calculate percentage change
table2_landings_prep <- future_landings_avg %>%
  left_join(baseline_landings_avg, by = "species_name") %>%
  mutate(
    `Percentage Change (Landings tonnes)` = ((Future_Avg_Landings - Baseline_Avg_Landings) / Baseline_Avg_Landings) * 100
  ) %>%
  rename(Species = species_name, Scenario = scenario) %>%
  select(Species, Scenario, `Percentage Change (Landings tonnes)`)


# Value Percentage Change Preparation
# Calculate baseline average annual value (from observed data)
baseline_value_avg <- annual_observed_all_species_value %>%
  filter(year %in% baseline_years) %>%
  group_by(species_name) %>%
  summarise(Baseline_Avg_Value = mean(value, na.rm = TRUE), .groups = 'drop')

# Calculate future average annual value (from predicted data)
future_value_avg <- all_species_predictions_lm_combined_value %>%
  filter(year %in% future_years) %>%
  group_by(species_name, scenario, year) %>% # Sum monthly to annual first
  summarise(Annual_Sum_Value = sum(predicted_real_value_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(Future_Avg_Value = mean(Annual_Sum_Value, na.rm = TRUE), .groups = 'drop') # Then average annual

# Join and calculate percentage change
table2_value_prep <- future_value_avg %>%
  left_join(baseline_value_avg, by = "species_name") %>%
  mutate(
    `Percentage Change (Value £'000s)` = ((Future_Avg_Value - Baseline_Avg_Value) / Baseline_Avg_Value) * 100
  ) %>%
  rename(Species = species_name, Scenario = scenario) %>%
  select(Species, Scenario, `Percentage Change (Value £'000s)`)

# Combine Landings and Value into a single wide table (Table 2)
table2_combined_wide <- full_join(table2_landings_prep, table2_value_prep, by = c("Species", "Scenario")) %>%
  arrange(Species, Scenario)

print(table2_combined_wide)
write_csv(table2_combined_wide, "tables/Table2_Percentage_Change_Landings_Value_Wide.csv")
message("Table 2 saved to tables/Table2_Percentage_Change_Landings_Value_Wide.csv")

# --- 5. Model Performance (R-squared and Coefficients) --------

message("\n--- Generating and Saving Model Performance Tables ---")

# Function to extract R-squared and Adjusted R-squared
extract_r_squared <- function(model_summary, species_name) {
  data.frame(
    Species = species_name,
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared
  )
}

# Function to extract coefficients and p-values
extract_coefficients <- function(model_summary, species_name) {
  coefs <- as.data.frame(model_summary$coefficients)
  coefs$Variable <- rownames(coefs)
  coefs$Species <- species_name
  # Rename columns for clarity and select relevant ones
  colnames(coefs) <- c("Estimate", "Std_Error", "t_value", "P_value", "Variable", "Species")
  coefs %>% select(Species, Variable, Estimate, Std_Error, t_value, P_value)
}


# Landings Model Performance Table (R-squared and Coefficients)
model_r_squared_landings <- purrr::map_dfr(names(all_species_model_summaries_landings),
                                           ~ extract_r_squared(all_species_model_summaries_landings[[.x]], .x))

model_coefficients_landings <- purrr::map_dfr(names(all_species_model_summaries_landings),
                                              ~ extract_coefficients(all_species_model_summaries_landings[[.x]], .x))

message("\nLandings Model R-squared values:")
print(model_r_squared_landings)
write_csv(model_r_squared_landings, "tables/Landings_Model_R_Squared.csv")
message("Landings Model R-squared values saved to tables/Landings_Model_R_Squared.csv")

message("\nLandings Model Coefficients:")
print(model_coefficients_landings)
write_csv(model_coefficients_landings, "tables/Landings_Model_Coefficients.csv")
message("Landings Model Coefficients saved to tables/Landings_Model_Coefficients.csv")


# Value Model Performance Table (R-squared and Coefficients)
model_r_squared_value <- purrr::map_dfr(names(all_species_model_summaries_value),
                                        ~ extract_r_squared(all_species_model_summaries_value[[.x]], .x))

model_coefficients_value <- purrr::map_dfr(names(all_species_model_summaries_value),
                                           ~ extract_coefficients(all_species_model_summaries_value[[.x]], .x))

message("\nValue Model R-squared values:")
print(model_r_squared_value)
write_csv(model_r_squared_value, "tables/Value_Model_R_Squared.csv")
message("Value Model R-squared values saved to tables/Value_Model_R_Squared.csv")

message("\nValue Model Coefficients:")
print(model_coefficients_value)
write_csv(model_coefficients_value, "tables/Value_Model_Coefficients.csv")
message("Value Model Coefficients saved to tables/Value_Model_Coefficients.csv")


# --- 6. Cross-Validation (Blocked Out Code) ---
# This section contains code for performing k-fold cross-validation.
# It is commented out by default. To run it, uncomment the code blocks.
message("\n--- Starting Cross-Validation (Blocked Out Code Section) ---")
message("To run cross-validation, uncomment the relevant code blocks in section 6.")


# # Ensure the 'tables' directory exists for CV results
# if (!dir.exists("tables/cv_results")) {
#   dir.create("tables/cv_results")
# }
#
# # --- Cross-Validation for Landings Models ---
# message("\n--- Cross-Validation for Landings Models ---")
#
# cv_results_landings <- list()
#
# # Define cross-validation control
# # Using 10-fold cross-validation
# train_control_cv <- trainControl(
#   method = "cv",
#   number = 10,
#   verboseIter = TRUE # Show progress
# )
#
# for (s_name in target_species_common) {
#   message(paste0("Performing CV for Landings for: ", s_name))
#   species_data_for_cv <- monthly_data_trimmed_landings %>%
#     filter(species_name == s_name) %>%
#     select(standardised_monthly_landings, standardised_monthly_temp, month_factor)
#
#   # Skip if data is insufficient after filtering
#   if(nrow(species_data_for_cv) < 10) {
#     warning(paste("Not enough data for CV for", s_name, " (Landings). Skipping."))
#     next
#   }
#
#   # Perform cross-validation using caret::train
#   # The `train` function automatically scales predictors internally if needed,
#   # but we've already standardized `standardised_monthly_temp`.
#   # For 'lm' method, it's equivalent to standard linear regression.
#   cv_model <- tryCatch({
#     train(
#       standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
#       data = species_data_for_cv,
#       method = "lm", # Linear model
#       trControl = train_control_cv
#     )
#   }, error = function(e) {
#     message(paste("Error during CV for Landings (", s_name, "): ", e$message))
#     return(NULL)
#   })
#
#   if (!is.null(cv_model)) {
#     # Extract relevant metrics (RMSE, R-squared, MAE)
#     metrics <- data.frame(
#       Species = s_name,
#       Type = "Landings",
#       RMSE = cv_model$results$RMSE,
#       Rsquared = cv_model$results$Rsquared,
#       MAE = cv_model$results$MAE
#     )
#     cv_results_landings[[s_name]] <- metrics
#   }
# }
#
# # Combine and save Landings CV results
# if (length(cv_results_landings) > 0) {
#   df_cv_landings <- bind_rows(cv_results_landings)
#   message("\nLandings Cross-Validation Results:")
#   print(df_cv_landings)
#   write_csv(df_cv_landings, "tables/cv_results/Landings_CV_Results.csv")
#   message("Landings CV results saved to tables/cv_results/Landings_CV_Results.csv")
# } else {
#   message("No Landings CV results to save (likely due to insufficient data for all species).")
# }
#
#
# # --- Cross-Validation for Value Models ---
# message("\n--- Cross-Validation for Value Models ---")
#
# cv_results_value <- list()
#
# for (s_name in target_species_common) {
#   message(paste0("Performing CV for Value for: ", s_name))
#   species_data_for_cv <- monthly_data_trimmed_value %>%
#     filter(species_name == s_name) %>%
#     select(standardised_monthly_value, standardised_monthly_temp, month_factor)
#
#   # Skip if data is insufficient after filtering
#   if(nrow(species_data_for_cv) < 10) {
#     warning(paste("Not enough data for CV for", s_name, " (Value). Skipping."))
#     next
#   }
#
#   cv_model <- tryCatch({
#     train(
#       standardised_monthly_value ~ standardised_monthly_temp + month_factor,
#       data = species_data_for_cv,
#       method = "lm", # Linear model
#       trControl = train_control_cv # Using the same CV control
#     )
#   }, error = function(e) {
#     message(paste("Error during CV for Value (", s_name, "): ", e$message))
#     return(NULL)
#   })
#
#   if (!is.null(cv_model)) {
#     # Extract relevant metrics (RMSE, R-squared, MAE)
#     metrics <- data.frame(
#       Species = s_name,
#       Type = "Value",
#       RMSE = cv_model$results$RMSE,
#       Rsquared = cv_model$results$Rsquared,
#       MAE = cv_model$results$MAE
#     )
#     cv_results_value[[s_name]] <- metrics
#   }
# }
#
# # Combine and save Value CV results
# if (length(cv_results_value) > 0) {
#   df_cv_value <- bind_rows(cv_results_value)
#   message("\nValue Cross-Validation Results:")
#   print(df_cv_value)
#   write_csv(df_cv_value, "tables/cv_results/Value_CV_Results.csv")
#   message("Value CV results saved to tables/cv_results/Value_CV_Results.csv")
# } else {
#   message("No Value CV results to save (likely due to insufficient data for all species).")
# }
#
# message("\n--- End of Cross-Validation Section ---")


# new attempt -------------------------------------------------------------

# Full Shellfish Landings and Value Analysis with Predictions and Summary Tables

# --- 0. Load Libraries ---------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)   # For year() and month() functions
library(tidyr)       # For expand_grid()
library(readr)       # For read_csv and write_csv
library(lmtest)      # For model testing
library(patchwork)   # For combining plots
library(caret)       # For machine learning utilities
library(purrr)       # For map_dfr and map functions
library(readxl)      # For read_excel (your CPI data source)
library(scales)      # For dollar_format in ggplot


# --- 1. Data Loading and Initial Preparation (Common to both Landings and Value) ----

# Load UK shellfish landings data
load("data/tidy_landings_data/UK_shellfish_landings.RData")
# Load observed average monthly temperature data
load("data/tidy_SST_data/observed_average_monthly_temperature.RData")

# Group by species_name and collect unique years for each (for informational purposes)
species_years <- UK_shellfish_landings %>%
  group_by(species_name) %>%
  summarise(years_present = paste(sort(unique(year)), collapse = ", "))

# Define common target species
target_species_common <- c("Crabs", "Norway Lobster", "European Lobster", "Scallops")

## Scenario data loading and initial processing ----------------
# SSP1-2.6 scenario data
scenario_1 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP1_2.6.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP1_2.6",
         date = floor_date(date, unit = "month"),
         year = year(date))

# SSP2-4.5 scenario data
scenario_2 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP2_4.5.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP2_4.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

# SSP5-8.5 scenario data
scenario_3 <- read_csv("data/tidy_SST_data/uk_mean_monthly_sst_SSP5_8.5.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         scenario = "SSP5_8.5",
         date = floor_date(date, unit = "month"),
         year = year(date))

# Combine all scenario dataframes into a list for easy iteration
all_scenario_dfs <- list(scenario_1, scenario_2, scenario_3)


# --- 2. Landings Analysis (Tonnes) ------------------------------

message("\n--- Starting Landings Analysis (Tonnes) ---")

# Monthly data processing for Landings
monthly_data_landings <- UK_shellfish_landings |>
  group_by(date, year, month, species_name) |>
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE),
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE), # Target variable for this section
    value_000s = sum(value_000s, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(species_name %in% target_species_common) |>
  left_join(observed_average_monthly_temperature, by = "date") |>
  mutate(
    total_months = as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) -
      (as.numeric(first(format(date, "%Y"))) * 12 + as.numeric(first(format(date, "%m"))))
  )

monthly_data_landings$species_name <- as.factor(monthly_data_landings$species_name)

# Trim the last 12 months for training
monthly_data_trimmed_landings <- monthly_data_landings[1:(nrow(monthly_data_landings) - 12), ]
monthly_data_trimmed_landings$month <- format(monthly_data_trimmed_landings$date, "%m")
monthly_data_trimmed_landings$month_factor <- as.factor(monthly_data_trimmed_landings$month)

# Calculate SPECIES-SPECIFIC means and SDs for landings
species_landings_stats <- monthly_data_trimmed_landings %>%
  group_by(species_name) %>%
  summarise(
    mean_landings_species = mean(landed_weight_tonnes, na.rm = TRUE),
    sd_landings_species = sd(landed_weight_tonnes, na.rm = TRUE),
    .groups = "drop"
  )

monthly_data_trimmed_landings <- monthly_data_trimmed_landings %>%
  left_join(species_landings_stats, by = "species_name") %>%
  mutate(
    standardised_monthly_landings = (landed_weight_tonnes - mean_landings_species) / sd_landings_species
  )

# Calculate GLOBAL mean and SD for `avg_temp` for landings model
mean_temp_landings <- mean(monthly_data_trimmed_landings$avg_temp, na.rm = TRUE)
sd_temp_landings <- sd(monthly_data_trimmed_landings$avg_temp, na.rm = TRUE)
monthly_data_trimmed_landings$standardised_monthly_temp <-
  (monthly_data_trimmed_landings$avg_temp - mean_temp_landings) / sd_temp_landings


# Function to Fit LM and Predict for a Single Species Across Scenarios (Landings)
predict_lm_for_species_landings <- function(current_species_name,
                                            monthly_data_trimmed_all_species,
                                            scenario_list,
                                            mean_temp, sd_temp,
                                            species_landings_stats_df) {
  message(paste("Processing predictions for Landings for:", current_species_name))
  species_observed_data <- monthly_data_trimmed_all_species %>%
    filter(species_name == current_species_name)
  if (nrow(species_observed_data) < 10) {
    warning(paste("Not enough data for", current_species_name, ". Skipping model fitting."))
    return(NULL)
  }
  species_lm_model <- lm(standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
                         data = species_observed_data)
  
  current_species_stats <- species_landings_stats_df %>%
    filter(species_name == current_species_name)
  current_mean_landings <- current_species_stats$mean_landings_species
  current_sd_landings <- current_species_stats$sd_landings_species
  
  species_predictions_list <- list()
  for (i in seq_along(scenario_list)) {
    scenario_df <- scenario_list[[i]]
    scenario_name <- unique(scenario_df$scenario)
    scenario_df_prepared <- scenario_df %>%
      mutate(species_name = factor(current_species_name, levels = levels(monthly_data_trimmed_all_species$species_name))) %>%
      mutate(standardised_monthly_temp = (avg_temp - mean_temp) / sd_temp) %>%
      mutate(month = format(date, "%m"), month_factor = factor(month, levels = levels(monthly_data_trimmed_all_species$month_factor))) %>%
      filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))
    
    predictions_matrix <- predict(species_lm_model, newdata = scenario_df_prepared, interval = "prediction", level = 0.95)
    
    species_predictions_list[[i]] <- scenario_df_prepared %>%
      mutate(
        predicted_landings_real = (predictions_matrix[, "fit"] * current_sd_landings) + current_mean_landings,
        predicted_landings_lwr = (predictions_matrix[, "lwr"] * current_sd_landings) + current_mean_landings,
        predicted_landings_upr = (predictions_matrix[, "upr"] * current_sd_landings) + current_mean_landings,
        scenario = scenario_name
      ) %>%
      select(date, year, month, species_name, scenario,
             predicted_landings_real, predicted_landings_lwr, predicted_landings_upr)
  }
  combined_predictions <- bind_rows(species_predictions_list)
  
  ### --- MODIFIED LINE --- ###
  return(list(predictions = combined_predictions, model_summary = summary(species_lm_model))) # Return summary here
  ### --- MODIFIED LINE --- ###
}

# Apply prediction function for Landings
### --- START MODIFIED BLOCK --- ###
all_species_results_landings <- lapply(target_species_common, function(s_name) {
  predict_lm_for_species_landings(
    current_species_name = s_name,
    monthly_data_trimmed_all_species = monthly_data_trimmed_landings,
    scenario_list = all_scenario_dfs,
    mean_temp = mean_temp_landings,
    sd_temp = sd_temp_landings,
    species_landings_stats_df = species_landings_stats
  )
})

# Now extract predictions and model summaries from the results list
all_species_predictions_lm_combined <- purrr::map_dfr(all_species_results_landings, ~ .x$predictions)
all_species_model_summaries_landings <- purrr::map(all_species_results_landings, ~ .x$model_summary)
names(all_species_model_summaries_landings) <- target_species_common # Name the list elements for easier access
### --- END MODIFIED BLOCK --- ###

# Prepare observed data for landings plots and tables
annual_observed_all_species <- monthly_data_trimmed_landings %>%
  mutate(year = year(date)) %>%
  filter(year < 2024) %>%
  group_by(year, species_name) %>%
  summarise(value = sum(landed_weight_tonnes, na.rm = TRUE), lwr = NA, upr = NA, .groups = 'drop') %>%
  mutate(data_type = "Observed")

# Prepare predicted data for landings plots
t_crit_value_for_pi <- qt(0.975, df = 113) # Assuming same df for Landings as Value
annual_predicted_all_species <- all_species_predictions_lm_combined %>%
  mutate(year = year(date)) %>%
  rowwise() %>%
  mutate(
    pi_width_monthly = predicted_landings_upr - predicted_landings_lwr,
    se_pred_monthly = pi_width_monthly / (2 * t_crit_value_for_pi),
    var_pred_monthly = se_pred_monthly^2
  ) %>%
  ungroup() %>%
  group_by(year, scenario, species_name) %>%
  summarise(
    value = sum(predicted_landings_real, na.rm = TRUE),
    total_var_pred_annual = sum(var_pred_monthly, na.rm = TRUE),
    se_pred_annual = sqrt(total_var_pred_annual),
    lwr = value - (t_crit_value_for_pi * se_pred_annual),
    upr = value + (t_crit_value_for_pi * se_pred_annual),
    .groups = 'drop'
  ) %>%
  mutate(lwr = pmax(0, lwr)) %>%
  mutate(data_type = "Predicted")

## Generate Landings Plots (Crabs, Norway Lobster, European Lobster only) ----------------------
species_to_plot_landings <- c("Crabs", "Norway Lobster", "European Lobster")
scenario_names <- c("SSP1_2.6", "SSP2_4.5", "SSP5_8.5")

for (s_name in species_to_plot_landings) {
  for (s_scenario in scenario_names) {
    message(paste0("Generating Landings Plot for Species: '", s_name, "' and Scenario: '", s_scenario, "'"))
    current_species_observed <- annual_observed_all_species %>% filter(species_name == s_name)
    current_species_scenario_predicted <- annual_predicted_all_species %>% filter(species_name == s_name, scenario == s_scenario)
    last_observed_annual_point <- current_species_observed %>% filter(year == max(year))
    bridging_point_for_predicted_line <- last_observed_annual_point %>%
      mutate(data_type = "Predicted", scenario = s_scenario, lwr = value, upr = value)
    
    plot_data_individual <- bind_rows(current_species_observed, bridging_point_for_predicted_line, current_species_scenario_predicted) %>% arrange(year)
    
    p <- ggplot(plot_data_individual, aes(x = year, y = value)) +
      geom_line(aes(color = data_type, linetype = data_type), size = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill = data_type), alpha = 0.2) +
      labs(title = paste0("Annual Landings for ", s_name, ": Observed & Predicted (", s_scenario, " Scenario)"),
           x = "Year", y = "Total Annual Landings (tonnes)", color = "Data Type", fill = "Data Type") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(breaks = seq(min(plot_data_individual$year), max(plot_data_individual$year), by = 5)) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "black", alpha = 0.6) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_color_manual(values = c("Observed" = "darkgrey", "Predicted" = "darkblue")) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed")) +
      scale_fill_manual(values = c("Observed" = "transparent", "Predicted" = "darkblue"))
    print(p)
  }
}


# --- 3. Value Analysis (£'000s) -------------------------------------

message("\n--- Starting Value Analysis (£'000s) ---")

# Monthly data processing for Value
monthly_data_value <- UK_shellfish_landings |>
  group_by(date, year, month, species_name) |>
  summarise(
    live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE),
    landed_weight_tonnes = sum(landed_weight_tonnes, na.rm = TRUE),
    value_000s = sum(value_000s, na.rm = TRUE), # Target variable for this section
    .groups = "drop"
  ) |>
  filter(species_name %in% target_species_common) |>
  left_join(observed_average_monthly_temperature, by = "date") |>
  mutate(
    total_months = as.numeric(format(date, "%Y")) * 12 + as.numeric(format(date, "%m")) -
      (as.numeric(first(format(date, "%Y"))) * 12 + as.numeric(first(format(date, "%m"))))
  )

monthly_data_value$species_name <- as.factor(monthly_data_value$species_name)

## --- CPI Adjustment ---------------------------------
cpi_raw <- read_excel("data/cpi_uk_ons.xlsx", sheet = "filtered_data")
cpi_clean <- cpi_raw |>
  mutate(date = parse_date_time(date, orders = "ym"), month_num = month(date))

monthly_data_value <- monthly_data_value |>
  left_join(cpi_clean %>% select(date, cpi_index), by = "date")

base_cpi_date <- as.Date("2015-07-01")
base_cpi <- cpi_clean |> filter(date == base_cpi_date) |> pull(cpi_index)

if (length(base_cpi) == 0 || is.na(base_cpi)) {
  stop(paste("Base CPI for date", base_cpi_date, "not found in cpi_clean data. Please check CPI data."))
}

monthly_data_value <- monthly_data_value |>
  mutate(real_value = value_000s * (base_cpi / cpi_index)) # real_value in £'000s (July 2015 prices)

# Trim the last 12 months for training
monthly_data_trimmed_value <- monthly_data_value[1:(nrow(monthly_data_value) - 12), ]
monthly_data_trimmed_value$month <- format(monthly_data_trimmed_value$date, "%m")
monthly_data_trimmed_value$month_factor <- as.factor(monthly_data_trimmed_value$month)

# Calculate SPECIES-SPECIFIC means and SDs for REAL VALUE
species_value_stats <- monthly_data_trimmed_value %>%
  group_by(species_name) %>%
  summarise(
    mean_value_species = mean(real_value, na.rm = TRUE),
    sd_value_species = sd(real_value, na.rm = TRUE),
    .groups = "drop"
  )

monthly_data_trimmed_value <- monthly_data_trimmed_value %>%
  left_join(species_value_stats, by = "species_name") %>%
  mutate(
    standardised_monthly_value = (real_value - mean_value_species) / sd_value_species
  )

# Calculate GLOBAL mean and SD for `avg_temp` for value model
mean_temp_value <- mean(monthly_data_trimmed_value$avg_temp, na.rm = TRUE)
sd_temp_value <- sd(monthly_data_trimmed_value$avg_temp, na.rm = TRUE)
monthly_data_trimmed_value$standardised_monthly_temp <-
  (monthly_data_trimmed_value$avg_temp - mean_temp_value) / sd_temp_value


# Function to Fit LM and Predict for a Single Species Across Scenarios (Value)
predict_lm_for_species_value <- function(current_species_name,
                                         monthly_data_trimmed_all_species,
                                         scenario_list,
                                         mean_temp, sd_temp,
                                         species_value_stats_df) {
  message(paste("Processing predictions for REAL VALUE for:", current_species_name))
  species_observed_data <- monthly_data_trimmed_all_species %>%
    filter(species_name == current_species_name)
  if (nrow(species_observed_data) < 10) {
    warning(paste("Not enough data for", current_species_name, ". Skipping model fitting."))
    return(NULL)
  }
  species_lm_model <- lm(standardised_monthly_value ~ standardised_monthly_temp + month_factor,
                         data = species_observed_data)
  
  current_species_stats <- species_value_stats_df %>%
    filter(species_name == current_species_name)
  current_mean_value <- current_species_stats$mean_value_species
  current_sd_value <- current_species_stats$sd_value_species
  
  species_predictions_list <- list()
  for (i in seq_along(scenario_list)) {
    scenario_df <- scenario_list[[i]]
    scenario_name <- unique(scenario_df$scenario)
    scenario_df_prepared <- scenario_df %>%
      mutate(species_name = factor(current_species_name, levels = levels(monthly_data_trimmed_all_species$species_name))) %>%
      mutate(standardised_monthly_temp = (avg_temp - mean_temp) / sd_temp) %>%
      mutate(month = format(date, "%m"), month_factor = factor(month, levels = levels(monthly_data_trimmed_all_species$month_factor))) %>%
      filter(date >= as.Date("2024-01-01") & date <= as.Date("2050-12-01"))
    
    predictions_matrix <- predict(species_lm_model, newdata = scenario_df_prepared, interval = "prediction", level = 0.95)
    
    species_predictions_list[[i]] <- scenario_df_prepared %>%
      mutate(
        predicted_real_value_real = (predictions_matrix[, "fit"] * current_sd_value) + current_mean_value,
        predicted_real_value_lwr = (predictions_matrix[, "lwr"] * current_sd_value) + current_mean_value,
        predicted_real_value_upr = (predictions_matrix[, "upr"] * current_sd_value) + current_mean_value,
        scenario = scenario_name
      ) %>%
      select(date, year, month, species_name, scenario,
             predicted_real_value_real, predicted_real_value_lwr, predicted_real_value_upr)
  }
  combined_predictions <- bind_rows(species_predictions_list)
  
  ### --- MODIFIED LINE --- ###
  return(list(predictions = combined_predictions, model_summary = summary(species_lm_model))) # Return summary here
  ### --- MODIFIED LINE --- ###
}

# Apply prediction function for Value
### --- START MODIFIED BLOCK --- ###
all_species_results_value <- lapply(target_species_common, function(s_name) {
  predict_lm_for_species_value(
    current_species_name = s_name,
    monthly_data_trimmed_all_species = monthly_data_trimmed_value,
    scenario_list = all_scenario_dfs,
    mean_temp = mean_temp_value,
    sd_temp = sd_temp_value,
    species_value_stats_df = species_value_stats
  )
})

# Now extract predictions and model summaries from the results list
all_species_predictions_lm_combined_value <- purrr::map_dfr(all_species_results_value, ~ .x$predictions)
all_species_model_summaries_value <- purrr::map(all_species_results_value, ~ .x$model_summary)
names(all_species_model_summaries_value) <- target_species_common # Name the list elements for easier access
### --- END MODIFIED BLOCK --- ###

# Prepare observed data for value plots and tables
annual_observed_all_species_value <- monthly_data_trimmed_value %>%
  mutate(year = year(date)) %>%
  filter(year < 2024) %>%
  group_by(year, species_name) %>%
  summarise(value = sum(real_value, na.rm = TRUE), lwr = NA, upr = NA, .groups = 'drop') %>%
  mutate(data_type = "Observed")

# Prepare predicted data for value plots
t_crit_value_for_pi_value <- qt(0.975, df = 113)
annual_predicted_all_species_value <- all_species_predictions_lm_combined_value %>%
  mutate(year = year(date)) %>%
  rowwise() %>%
  mutate(
    pi_width_monthly = predicted_real_value_upr - predicted_real_value_lwr,
    se_pred_monthly = pi_width_monthly / (2 * t_crit_value_for_pi_value),
    var_pred_monthly = se_pred_monthly^2
  ) %>%
  ungroup() %>%
  group_by(year, scenario, species_name) %>%
  summarise(
    value = sum(predicted_real_value_real, na.rm = TRUE),
    total_var_pred_annual = sum(var_pred_monthly, na.rm = TRUE),
    se_pred_annual = sqrt(total_var_pred_annual),
    lwr = value - (t_crit_value_for_pi_value * se_pred_annual),
    upr = value + (t_crit_value_for_pi_value * se_pred_annual),
    .groups = 'drop'
  ) %>%
  mutate(lwr = pmax(0, lwr)) %>%
  mutate(data_type = "Predicted")

## Generate Value Plots (Crabs, Norway Lobster, European Lobster only) ------------
species_to_plot_value <- c("Crabs", "Norway Lobster", "European Lobster")

for (s_name in species_to_plot_value) {
  for (s_scenario in scenario_names) {
    message(paste0("Generating Real Value Plot for Species: '", s_name, "' and Scenario: '", s_scenario, "'"))
    current_species_observed_value <- annual_observed_all_species_value %>% filter(species_name == s_name)
    current_species_scenario_predicted_value <- annual_predicted_all_species_value %>% filter(species_name == s_name, scenario == s_scenario)
    last_observed_annual_point_value <- current_species_observed_value %>% filter(year == max(year))
    bridging_point_for_predicted_line_value <- last_observed_annual_point_value %>%
      mutate(data_type = "Predicted", scenario = s_scenario, lwr = value, upr = value)
    
    plot_data_individual_value <- bind_rows(current_species_observed_value, bridging_point_for_predicted_line_value, current_species_scenario_predicted_value) %>% arrange(year)
    
    p <- ggplot(plot_data_individual_value, aes(x = year, y = value)) +
      geom_line(aes(color = data_type, linetype = data_type), size = 0.8) +
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill = data_type), alpha = 0.2) +
      labs(title = paste0("Annual Real Value for ", s_name, ": Observed & Predicted (", s_scenario, " Scenario)"),
           x = "Year", y = "Total Annual Real Value (£'000s - July 2015 Prices)", color = "Data Type", fill = "Data Type") +
      scale_y_continuous(limits = c(0, NA), labels = scales::dollar_format(prefix = "£")) +
      scale_x_continuous(breaks = seq(min(plot_data_individual_value$year), max(plot_data_individual_value$year), by = 5)) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "black", alpha = 0.6) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_color_manual(values = c("Observed" = "darkgrey", "Predicted" = "darkblue")) +
      scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "dashed")) +
      scale_fill_manual(values = c("Observed" = "transparent", "Predicted" = "darkblue"))
    print(p)
  }
}


# --- 4.Summary Tables ---------------------

message("\n--- Generating and Saving Summary Tables ---")

# Ensure the 'tables' directory exists
if (!dir.exists("tables")) {
  dir.create("tables")
}

# Define the common species and scenarios for tables
table_species <- c("Crabs", "Norway Lobster", "European Lobster", "Scallops") # Include Scallops for tables
table_scenarios <- c("SSP1_2.6", "SSP2_4.5", "SSP5_8.5")

## --- Table 1: Max/Min Landings/Value (2024-2050) ------------

message("\n--- Generating Table 1: Predicted Annual Max/Min Landings/Value (2024-2050) ---")

# Landings Max/Min (Annual sums, then max/min of those sums)
table1_landings_prep <- all_species_predictions_lm_combined %>%
  filter(year >= 2024 & year <= 2050) %>%
  group_by(species_name, scenario, year) %>% # Group by year first to get annual sums
  summarise(Annual_Sum_Landings = sum(predicted_landings_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(
    `Max Annual Landings (tonnes)` = max(Annual_Sum_Landings, na.rm = TRUE),
    `Min Annual Landings (tonnes)` = min(Annual_Sum_Landings, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Species = species_name, Scenario = scenario)

# Value Max/Min (Annual sums, then max/min of those sums)
table1_value_prep <- all_species_predictions_lm_combined_value %>%
  filter(year >= 2024 & year <= 2050) %>%
  group_by(species_name, scenario, year) %>% # Group by year first to get annual sums
  summarise(Annual_Sum_Value = sum(predicted_real_value_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(
    `Max Annual Value (£'000s)` = max(Annual_Sum_Value, na.rm = TRUE),
    `Min Annual Value (£'000s)` = min(Annual_Sum_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  rename(Species = species_name, Scenario = scenario)

# Combine Landings and Value into a single wide table (Table 1)
table1_combined_wide <- full_join(table1_landings_prep, table1_value_prep, by = c("Species", "Scenario")) %>%
  arrange(Species, Scenario)

print(table1_combined_wide)
write_csv(table1_combined_wide, "tables/Table1_Predicted_Annual_Max_Min_Landings_Value_2024-2050.csv")
message("Table 1 saved to tables/Table1_Predicted_Annual_Max_Min_Landings_Value_2024-2050.csv")


##--- Table 2: Percentage Change(2023 Baseline) --------------------

message("\n--- Generating Table 2: Percentage Change in Average Annual Landings/Value (2023 Baseline) ---")

# Define baseline and future periods
baseline_years <- 2018:2023 # Using 2018-2023 as a representative baseline from observed data
future_years <- 2024:2050

# Landings Percentage Change Preparation
# Calculate baseline average annual landings (from observed data)
baseline_landings_avg <- annual_observed_all_species %>%
  filter(year %in% baseline_years) %>%
  group_by(species_name) %>%
  summarise(Baseline_Avg_Landings = mean(value, na.rm = TRUE), .groups = 'drop')

# Calculate future average annual landings (from predicted data)
future_landings_avg <- all_species_predictions_lm_combined %>%
  filter(year %in% future_years) %>%
  group_by(species_name, scenario, year) %>% # Sum monthly to annual first
  summarise(Annual_Sum_Landings = sum(predicted_landings_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(Future_Avg_Landings = mean(Annual_Sum_Landings, na.rm = TRUE), .groups = 'drop') # Then average annual

# Join and calculate percentage change
table2_landings_prep <- future_landings_avg %>%
  left_join(baseline_landings_avg, by = "species_name") %>%
  mutate(
    `Percentage Change (Landings tonnes)` = ((Future_Avg_Landings - Baseline_Avg_Landings) / Baseline_Avg_Landings) * 100
  ) %>%
  rename(Species = species_name, Scenario = scenario) %>%
  select(Species, Scenario, `Percentage Change (Landings tonnes)`)


# Value Percentage Change Preparation
# Calculate baseline average annual value (from observed data)
baseline_value_avg <- annual_observed_all_species_value %>%
  filter(year %in% baseline_years) %>%
  group_by(species_name) %>%
  summarise(Baseline_Avg_Value = mean(value, na.rm = TRUE), .groups = 'drop')

# Calculate future average annual value (from predicted data)
future_value_avg <- all_species_predictions_lm_combined_value %>%
  filter(year %in% future_years) %>%
  group_by(species_name, scenario, year) %>% # Sum monthly to annual first
  summarise(Annual_Sum_Value = sum(predicted_real_value_real, na.rm = TRUE), .groups = 'drop_last') %>%
  summarise(Future_Avg_Value = mean(Annual_Sum_Value, na.rm = TRUE), .groups = 'drop') # Then average annual

# Join and calculate percentage change
table2_value_prep <- future_value_avg %>%
  left_join(baseline_value_avg, by = "species_name") %>%
  mutate(
    `Percentage Change (Value £'000s)` = ((Future_Avg_Value - Baseline_Avg_Value) / Baseline_Avg_Value) * 100
  ) %>%
  rename(Species = species_name, Scenario = scenario) %>%
  select(Species, Scenario, `Percentage Change (Value £'000s)`)

# Combine Landings and Value into a single wide table (Table 2)
table2_combined_wide <- full_join(table2_landings_prep, table2_value_prep, by = c("Species", "Scenario")) %>%
  arrange(Species, Scenario)

print(table2_combined_wide)
write_csv(table2_combined_wide, "tables/Table2_Percentage_Change_Landings_Value_Wide.csv")
message("Table 2 saved to tables/Table2_Percentage_Change_Landings_Value_Wide.csv")

# --- 5. Model Performance (R-squared and Coefficients) --------

message("\n--- Generating and Saving Model Performance Tables ---")

# Function to extract R-squared and Adjusted R-squared
extract_r_squared <- function(model_summary, species_name) {
  data.frame(
    Species = species_name,
    R_squared = model_summary$r.squared,
    Adjusted_R_squared = model_summary$adj.r.squared
  )
}

# Function to extract coefficients and p-values
extract_coefficients <- function(model_summary, species_name) {
  coefs <- as.data.frame(model_summary$coefficients)
  coefs$Variable <- rownames(coefs)
  coefs$Species <- species_name
  # Rename columns for clarity and select relevant ones
  colnames(coefs) <- c("Estimate", "Std_Error", "t_value", "P_value", "Variable", "Species")
  coefs %>% select(Species, Variable, Estimate, Std_Error, t_value, P_value)
}


# Landings Model Performance Table (R-squared and Coefficients)
model_r_squared_landings <- purrr::map_dfr(names(all_species_model_summaries_landings),
                                           ~ extract_r_squared(all_species_model_summaries_landings[[.x]], .x))

model_coefficients_landings <- purrr::map_dfr(names(all_species_model_summaries_landings),
                                              ~ extract_coefficients(all_species_model_summaries_landings[[.x]], .x))

message("\nLandings Model R-squared values:")
print(model_r_squared_landings)
write_csv(model_r_squared_landings, "tables/Landings_Model_R_Squared.csv")
message("Landings Model R-squared values saved to tables/Landings_Model_R_Squared.csv")

message("\nLandings Model Coefficients:")
print(model_coefficients_landings)
write_csv(model_coefficients_landings, "tables/Landings_Model_Coefficients.csv")
message("Landings Model Coefficients saved to tables/Landings_Model_Coefficients.csv")


# Value Model Performance Table (R-squared and Coefficients)
model_r_squared_value <- purrr::map_dfr(names(all_species_model_summaries_value),
                                        ~ extract_r_squared(all_species_model_summaries_value[[.x]], .x))

model_coefficients_value <- purrr::map_dfr(names(all_species_model_summaries_value),
                                           ~ extract_coefficients(all_species_model_summaries_value[[.x]], .x))

message("\nValue Model R-squared values:")
print(model_r_squared_value)
write_csv(model_r_squared_value, "tables/Value_Model_R_Squared.csv")
message("Value Model R-squared values saved to tables/Value_Model_R_Squared.csv")

message("\nValue Model Coefficients:")
print(model_coefficients_value)
write_csv(model_coefficients_value, "tables/Value_Model_Coefficients.csv")
message("Value Model Coefficients saved to tables/Value_Model_Coefficients.csv")


## --- 6. Cross-Validation ------------------------------
message("\n--- Starting Cross-Validation (Blocked Out Code Section) ---")

# # Ensure the 'tables' directory exists for CV results
if (!dir.exists("tables/cv_results")) {
   dir.create("tables/cv_results")
 }

# # --- Cross-Validation for Landings Models ---
# message("\n--- Cross-Validation for Landings Models ---")
#
cv_results_landings <- list()
#
#  Define cross-validation control
#  Using 10-fold cross-validation
train_control_cv <- trainControl(
   method = "cv",
   number = 10,
   verboseIter = TRUE # Show progress
 )

for (s_name in target_species_common) {
  message(paste0("Performing CV for Landings for: ", s_name))
  species_data_for_cv <- monthly_data_trimmed_landings %>%
    filter(species_name == s_name) %>%
    select(standardised_monthly_landings, standardised_monthly_temp, month_factor) |> 
    na.omit()

  # Skip if data is insufficient after filtering
  if(nrow(species_data_for_cv) < 10) {
    warning(paste("Not enough data for CV for", s_name, " (Landings). Skipping."))
    next
  }

  # Perform cross-validation using caret::train
  # The `train` function automatically scales predictors internally if needed,
  # but we've already standardized `standardised_monthly_temp`.
  # For 'lm' method, it's equivalent to standard linear regression.
  cv_model <- tryCatch({
    train(
      standardised_monthly_landings ~ standardised_monthly_temp + month_factor,
      data = species_data_for_cv,
      method = "lm", # Linear model
      trControl = train_control_cv
    )
  }, error = function(e) {
    message(paste("Error during CV for Landings (", s_name, "): ", e$message))
    return(NULL)
  })

  if (!is.null(cv_model)) {
    # Extract relevant metrics (RMSE, R-squared, MAE)
    metrics <- data.frame(
      Species = s_name,
      Type = "Landings",
      RMSE = cv_model$results$RMSE,
      Rsquared = cv_model$results$Rsquared,
      MAE = cv_model$results$MAE
    )
    cv_results_landings[[s_name]] <- metrics
  }
}

# Combine and save Landings CV results
if (length(cv_results_landings) > 0) {
  df_cv_landings <- bind_rows(cv_results_landings)
  message("\nLandings Cross-Validation Results:")
  print(df_cv_landings)
  write_csv(df_cv_landings, "tables/cv_results/Landings_CV_Results.csv")
  message("Landings CV results saved to tables/cv_results/Landings_CV_Results.csv")
} else {
  message("No Landings CV results to save (likely due to insufficient data for all species).")
}


# --- Cross-Validation for Value Models ---
message("\n--- Cross-Validation for Value Models ---")

cv_results_value <- list()

for (s_name in target_species_common) {
  message(paste0("Performing CV for Value for: ", s_name))
  species_data_for_cv <- monthly_data_trimmed_value %>%
    filter(species_name == s_name) %>%
    select(standardised_monthly_value, standardised_monthly_temp, month_factor) |> 
    na.omit()

  # Skip if data is insufficient after filtering
  if(nrow(species_data_for_cv) < 10) {
    warning(paste("Not enough data for CV for", s_name, " (Value). Skipping."))
    next
  }

  cv_model <- tryCatch({
    train(
      standardised_monthly_value ~ standardised_monthly_temp + month_factor,
      data = species_data_for_cv,
      method = "lm", # Linear model
      trControl = train_control_cv # Using the same CV control
    )
  }, error = function(e) {
    message(paste("Error during CV for Value (", s_name, "): ", e$message))
    return(NULL)
  })

  if (!is.null(cv_model)) {
    # Extract relevant metrics (RMSE, R-squared, MAE)
    metrics <- data.frame(
      Species = s_name,
      Type = "Value",
      RMSE = cv_model$results$RMSE,
      Rsquared = cv_model$results$Rsquared,
      MAE = cv_model$results$MAE
    )
    cv_results_value[[s_name]] <- metrics
  }
}

# Combine and save Value CV results
  {df_cv_value <- bind_rows(cv_results_value)
  message("\nValue Cross-Validation Results:")
  print(df_cv_value)
  write_csv(df_cv_value, "tables/cv_results/Value_CV_Results.csv")
  message("Value CV results saved to tables/cv_results/Value_CV_Results.csv")
} else {
  message("No Value CV results to save (likely due to insufficient data for all species).")
}

message("\n--- End of Cross-Validation Section ---")
