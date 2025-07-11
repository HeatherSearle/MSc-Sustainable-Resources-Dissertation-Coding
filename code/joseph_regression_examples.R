library(dplyr)
library(janitor)

landings_lobster <- landings_data_2014_2024 %>%
  clean_names() %>%
  filter(
    species_name == "Lobsters",
    port_nationality %in% c("Wales", "England", "Scotland", "Northern Ireland", 	
                            "UK - Scotland", "UK - Wales", "UK - England", "UK - Northern Ireland")
  ) %>%
  group_by(year) %>%
  summarise(
    annual_revenue = sum(value_000s, na.rm = TRUE),  # Already in £000s
    annual_tonnes = sum(live_weight_tonnes, na.rm = TRUE)
  ) %>%
  mutate(
    avg_price_per_tonne = (annual_revenue * 1000) / annual_tonnes  # Convert to £
  )

head(landings_lobster)

regression_data <- landings_lobster %>%
  inner_join(temp_annual %>% clean_names() %>% mutate(year = as.numeric(year)), by = "year") %>%
  inner_join(fuel_data %>% clean_names() %>% mutate(year = as.numeric(year)), by = "year") %>%
  inner_join(employment_data %>% clean_names() %>% mutate(year = as.numeric(year)), by = "year")

regression_data <- regression_data %>%
  mutate(
    log_revenue = log(annual_revenue * 1000),  # convert to £
    log_temp = log(annual_temp_anomaly + abs(min(annual_temp_anomaly)) + 1),
    log_fuel = log(avg_fuel_cost_per_vessel),
    log_emp = log(employment_fte),
    
    # Squared terms
    log_temp_sq = log_temp^2,
    log_fuel_sq = log_fuel^2,
    log_emp_sq = log_emp^2,
    
    # Interaction terms
    log_temp_fuel = log_temp * log_fuel,
    log_temp_emp = log_temp * log_emp,
    log_fuel_emp = log_fuel * log_emp
  )

translog_model <- glm(
  log_revenue ~ log_temp + log_fuel + log_emp +
    log_temp_sq + log_fuel_sq + log_emp_sq +
    log_temp_fuel + log_temp_emp + log_fuel_emp,
  data = regression_data
)

summary(translog_model)



# Testying ----------------------------------------------------------------


landings_lobster <- landings_data_2014_2024 %>%
  clean_names() %>%
  filter(
    species_name == "Lobsters",
    port_nationality %in% c("Wales", "England", "Scotland", "Northern Ireland", 	
                            "UK - Scotland", "UK - Wales", "UK - England", "UK - Northern Ireland")
  ) %>%
  group_by(port_nationality) %>%
  reframe(
    # max_catch = max(live_weight_tonnes, na.rm = TRUE),
    # max_value = max(value_000s, na.rm = TRUE),
    years = unique(year),
    .groups = "drop"
  ) 


# Example Plots -----------------------------------------------------------

df <- landings_data_2014_2024 |> 
  clean_names() %>%
  mutate(
    date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d")
  ) |> 
  filter(
    species_name == "Lobsters",
    port_nationality %in% c("Wales", "England", "Scotland", "Northern Ireland", 	
                            "UK - Scotland", "UK - Wales", "UK - England", "UK - Northern Ireland"),
    gear_category %in% c("Traps", "Pots and traps", "NA")
  ) 

# A basic plot
ggplot(df, aes(x = date, y = live_weight_tonnes, col = gear_category)) +
  geom_point()

# You can pipe the data frame straight into the plot!
df |> 
  group_by(date) |>
  summarise(live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = date, y = live_weight_tonnes)) +
    geom_line()

# What about the different gear categories as a bar huh?
df |> 
  group_by(gear_category) |>
  summarise(live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), .groups = "drop") |> 
  mutate(gear_category = as.factor(gear_category)) |> 
  ggplot(aes(x = gear_category, y = live_weight_tonnes)) +
    geom_bar(stat = "identity") + 
    labs(x = "Gear category", y = "Total live weight (tonnes)") +
    theme_minimal()

# Nicer way puts thyem horizontal 
# Geom_col automatically uses the y value, but geom_bar uses the count instead
library(tidyverse)
library(viridis) # gives us colours
df |> 
  group_by(gear_category) |>
  summarise(live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), .groups = "drop") |>
  mutate(gear_category = fct_reorder(gear_category, live_weight_tonnes)) |>
  # Note I also have to set fill to gear category here to allow me to colour the bars
  ggplot(aes(x = gear_category, y = live_weight_tonnes, fill = gear_category)) +
  geom_col() +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  coord_flip() +    # flips to horizontal, often easier to read
  labs(x = NULL, y = "Total live weight (tonnes)") +
  theme_minimal() + 
  theme(legend.position = "none") # adding a fill makes a legend we don't want

# Time series analysis ----------------------------------------------------

df = df |> 
     group_by(date) |>
      summarise(live_weight_tonnes = sum(live_weight_tonnes, na.rm = TRUE), .groups = "drop") 

# 1. Turn into a ts object (start = Jan 2014, frequency = 12 months)
ts_lobster <- ts(
  df |> 
    arrange(date) |> 
    pull(live_weight_tonnes),
  start = c(2014, 1),
  frequency = 12
)

# 2. Decompose into trend, seasonal, and remainder
dec <- decompose(ts_lobster, type = "additive")

# 3. Extract seasonally-adjusted (i.e. deseasonalised + detrended)
deseasoned  <- ts_lobster - dec$seasonal
detrended    <- ts_lobster - dec$trend
remainder    <- dec$random

# 4. (Optional) Recombine trend + remainder if you only want the seasonal removed:
trend_plus_remainder <- detrended + remainder

# Quick plot
plot(dec)
plot(deseasoned, main="Seasonally Adjusted Lobster Landings")
