# this script is where we are reading in the data from multiple data sources. 
# the working directory is set as the dissertation_coding folder on Onedrive
# First, we are reading in and combining the landings data 2021-2025 from MMO

library(readxl)
library(dplyr)

# Landings Data 2014-24 ----

# All data must be in the same folder - landings data found data/raw_landings_data

expected_cols <- c(
  "Year", "Month", "Port of landing", "Port Nationality", "Vessel nationality",
  "Length Group", "Gear category", "Species code", "Species name",
  "Species group", "Live Weight (tonnes)", "Landed Weight (tonnes)", "Value (£000s)"
)
# Had to specify the columns as 2023 data had 3 extra ones

# List the Excel files in the folder
files <- list.files(
    path = "data/raw_landings_data", 
    pattern = "landings_data_\\d{4}\\.xlsx", #_2014 includes the data 2014-2020
    full.names = TRUE
  )

# Function to read and clean a single file
 # read_clean_data <- function(file_path) {
#  df <- read_excel(file_path, sheet = "Data")
# df_clean <- df |> select(all_of(expected_cols))
# return(df_clean)
# }

read_clean_data <- function(file_path) {
  df <- read_excel(file_path, sheet = "Data")
  
  # Add missing columns with NA
  for (col in setdiff(expected_cols, colnames(df))) {
    df[[col]] <- NA
  }
  
  # Reorder to match expected_cols
  df_clean <- df |>
    select(all_of(expected_cols))
  
  return(df_clean)
}

# Read and combine all data
landings_data_2014_2024 <- files |> 
  lapply(read_clean_data) |> 
  bind_rows()

# Check the combined dataset
str(landings_data_2014_2024)
head(landings_data_2014_2024)
summary(landings_data_2014_2024)

library(janitor)
# Tidy column names - fix silly capitalization and adds _ where needed
landings_data_2014_2024 <- landings_data_2014_2024 |> 
  clean_names() # you could do this in the function
# should also add a date column (use the lubridate package)

# Save the combined dataset as an RData file 
save(
  landings_data_2014_2024, 
  file = "data/tidy_landings_data/landings_data_2014_2024.RData")

write.csv(landings_data_2014_2024,
          file = "data/tidy_landings_data/landings_data_2014_2024.csv",
          row.names = FALSE)

# use command shift R to add a section header

