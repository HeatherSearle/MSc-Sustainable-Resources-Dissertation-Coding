# tidying our landings data from DEFRA - key issues, gear category, NA value, value units, duplicates 
library(janitor)
library(dplyr)
load("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/MSc Dissertation/dissertation_coding/data/tidy_landings_data/landings_data_2014_2024.RData")

# tidying DEFRA landings 2014-2024 ----------------------------------------


# removing duplicate rows
get_dupes(landings_data_2014_2024)
landings_data_2014_2024[duplicated(landings_data_2014_2024), ]

# cleaning value names and false NAs
landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(
    gear_category = case_when(
      gear_category %in% c("Pots and traps", "Traps") ~ "pots and traps",
      TRUE ~ gear_category
    )
  )

na_strings <- c("n/a", "N/A", "-", "NA", "", " ")
landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(across(where(is.character), ~ ifelse(. %in% na_strings, NA, .)))

landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(
    port_nationality = case_when(
      port_nationality %in% c("UK - Wales") ~ "Wales",
      TRUE ~ port_nationality
    )
  )

landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(
    port_nationality = case_when(
      port_nationality %in% c("UK - England") ~ "England",
      TRUE ~ port_nationality
    )
  )

landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(
    port_nationality = case_when(
      port_nationality %in% c("UK - Scotland") ~ "Scotland",
      TRUE ~ port_nationality
    )
  )

landings_data_2014_2024 <- landings_data_2014_2024 |> 
  mutate(
    port_nationality = case_when(
      port_nationality %in% c("UK - Northern Ireland") ~ "Northern Ireland",
      TRUE ~ port_nationality
    )
  )

landings_data_2014_2024 |> 
  count(gear_category, sort = TRUE)
landings_data_2014_2024 |> 
  count(port_nationality, sort = TRUE)

# make a date column
landings_data_2014_2024 = landings_data_2014_2024 |> 
  mutate(
    date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d")
  ) 



# Save UK lobster landings subset ----------------------------------------------


UK_lobster_landings <- landings_data_2014_2024 |> 
  filter(species_name == "Lobsters",
         port_nationality %in% c("UK", "Northern Ireland", "Wales", "Scotland", "England"),
         vessel_nationality %in% c("UK", "Northern Ireland", "Wales", "Scotland", "England"))

save(
  UK_lobster_landings, 
  file = "data/tidy_landings_data/UK_lobster_landings.RData"
)
  

# Make a nice plot --------------------------------------------------------

df <- UK_lobster_landings |> 
  group_by(date) |> 
  summarise(total_value = sum(value_000s, na.rm = TRUE), .groups = "drop")

figure_1 <- ggplot(df, aes(x = date, y = total_value)) +
  geom_line(color = "lightpink", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Total Lobster Value by Year",
    x = "Year",
    y = "Total Value (£000s)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::comma,  # adds commas for readability (e.g., 5,000)
    limits = c(0,10000)
  )

figure_1
# ggsave(filename = "Figure_1.jpg", plot = figure_1)


# Tidying Fuel data -------------------------------------------------------
library(readxl)
library(janitor)

average_fuel_costs_data <- read_xls("data/average_fuel_costs_2013-2023.xls")
clean_names(average_fuel_costs_data)


# Tidying SST scenario data ---------------------------------------------------
# data already standardised
# S1
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_1_sst_data.RData")

scenario_1_sst_data <- scenario_1_sst_data |>
  rename(
    date = Date,
    avg_temp = Mean_UK_SST
  )

scenario_1_filtered <- scenario_1_sst_data |>
  filter(year(date) >= 2025, year(date) <= 2045)

scenario_1_filtered <- scenario_1_filtered |>
  mutate(
    month_factor = factor(
      sprintf("%02d", month(date)),
      levels = levels(combined_standardised_data_for_regression_SSTV$month_factor)
    )
  )

scenario_1_filtered$date <- as.Date(scenario_1_filtered$date)

save(
  scenario_1_filtered, 
  file = "data/standardised_data/scenario_1_filtered_standardised.RData")


# S2
# data already standardised
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_2_sst_data.RData")

scenario_2_sst_data <- scenario_2_sst_data |>
  rename(
    date = Date,
    avg_temp = Mean_UK_SST
  )

scenario_2_filtered <- scenario_2_sst_data |>
  filter(year(date) >= 2025, year(date) <= 2045)

# Need to load seasonal LM first 
scenario_2_filtered <- scenario_2_filtered |>
  mutate(
    month_factor = factor(
      sprintf("%02d", month(date)),
      levels = levels(combined_standardised_data_for_regression_SSTV$month_factor)
    )
  )

scenario_2_filtered$date <- as.Date(scenario_2_filtered$date)

save(
  scenario_2_filtered, 
  file = "data/standardised_data/scenario_2_filtered_standardised.RData")

# S3
load("~/OneDrive - University College London/MSc Dissertation/dissertation_coding/data/standardised_data/scenario_2_sst_data.RData")

scenario_3_sst_data <- scenario_3_sst_data |>
  rename(
    date = Date,
    avg_temp = Mean_UK_SST
  )

scenario_3_filtered <- scenario_3_sst_data |>
  filter(year(date) >= 2025, year(date) <= 2045)

# Need to load seasonal LM first 
scenario_3_filtered <- scenario_3_filtered |>
  mutate(
    month_factor = factor(
      sprintf("%02d", month(date)),
      levels = levels(combined_standardised_data_for_regression_SSTV$month_factor)
    )
  )

scenario_3_filtered$date <- as.Date(scenario_3_filtered$date)

save(
  scenario_3_filtered, 
  file = "data/standardised_data/scenario_3_filtered_standardised.RData")
