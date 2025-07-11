# 1. SETUP ------------------------------------------------------------------
# Load necessary libraries
library(terra)
library(ncdf4) # We need this for reading the 2D coordinate arrays
library(dplyr)
library(lubridate)

# Define the scenarios and their file paths
scenarios <- data.frame(
  scenario_name = c("sst_SSP1_2.6", "sst_SSP2_4.5", "sst_SSP5_8.5"),
  path = c(
    "data/raw_SST_data/SSP1-2.6 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP2-4.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP5-8.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_20150116-20501216.nc"
  )
)

# Define common spatial parameters for your Area of Interest (AOI)
min_lon <- -8.33345
max_lon <- 2.999769
min_lat <- 48.53376
max_lat <- 59.80099
target_extent <- ext(min_lon, max_lon, min_lat, max_lat)

# Create a template raster for our desired output grid.
# The rasterize function will place the point data into this grid.
target_grid_template <- rast(target_extent, resolution = 1, crs = "EPSG:4326")


# 2. PROCESSING LOOP --------------------------------------------------------
# Use a list to store the final data frame for each scenario
all_scenarios_data <- list()

for (i in 1:nrow(scenarios)) {
  scenario <- scenarios$scenario_name[i]
  scenario_path <- scenarios$path[i]
  
  message(paste("\nProcessing scenario:", scenario))
  
  # --- Step 1: Load Data and Coordinates ---
  
  # Use terra to read the main data stack. It will correctly interpret the 360-day calendar.
  message("  -> Loading SST data with terra...")
  sst_stack_raw <- rast(scenario_path)
  time_points <- time(sst_stack_raw) # Get the correct dates
  
  # Use ncdf4 to read the 2D longitude and latitude arrays, which terra can't use directly.
  message("  -> Loading 2D coordinate arrays with ncdf4...")
  nc_con <- nc_open(scenario_path)
  lon_2d <- ncvar_get(nc_con, "longitude")
  lat_2d <- ncvar_get(nc_con, "latitude")
  nc_close(nc_con)
  
  # Flatten the coordinate arrays into vectors ONCE before the loop for efficiency.
  lon_vec <- as.vector(lon_2d)
  lat_vec <- as.vector(lat_2d)
  
  # --- Step 2: Regrid Data Layer-by-Layer using the Point-Rasterize Method ---
  
  processed_mean_layers <- list() # List to store regridded monthly means

  message("  -> Regridding month-by-month...")
  for (j in 1:nlyr(sst_stack_raw)) {
    if (j %% 12 == 0) { # Print a progress message every 12 months
      message(paste0("     Processing layer ", j, " of ", nlyr(sst_stack_raw), " (", format(time_points[j], "%Y-%m"), ")"))
    }
    
    # Extract the data for the current time slice and flatten to a vector
    tos_slice <- sst_stack_raw[[j]]
    val_vec <- as.vector(values(tos_slice, na.rm = FALSE))
    
    # Create a data frame of points with their coordinates and SST values
    pts <- data.frame(lon = lon_vec, lat = lat_vec, value = val_vec)
    
    # Remove points with NA values (e.g., land cells)
    pts <- na.omit(pts)
    
    # Convert longitude from 0-360 to the -180 to 180 range
    # This must be done to match the target grid's coordinate system.
    pts$lon <- ifelse(pts$lon > 180, pts$lon - 360, pts$lon)
    
    # Create a SpatVector from these points
    data_vect <- vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326")
    
    # Rasterize for MEAN (cell average)
    regridded_mean <- rasterize(
      data_vect,
      target_grid_template,
      field = "value",
      fun = mean,
      na.rm = TRUE
    )
    names(regridded_mean) <- paste0("mean_layer_", j) # Generic name for mean layer
    processed_mean_layers[[j]] <- regridded_mean
    
  }
  
  # --- Step 3: Combine Layers and Add Time Information ---
  message("  -> Assembling final raster stack...")

  # Stack of regridded monthly means
  uk_sst_mean_stack <- rast(processed_mean_layers)
  time(uk_sst_mean_stack) <- time_points
  
  # Optional: Plot the first layer to visually confirm it worked
  #plot(uk_sst_stack[[1]], main = paste("First time-slice for", scenario))
  
  # --- Step 4: Extract and Save Data ---
  message("  -> Calculating monthly mean and standard deviation...")

  # Overall mean temperature for the UK region (mean of cell means)
  overall_mean_temp <- global(uk_sst_mean_stack, fun = "mean", na.rm = TRUE)
  
  # Between-cell standard deviation (SD of cell means across the UK region)
  # This is your current 'sd_temp'
  between_cell_sd_temp <- global(uk_sst_mean_stack, fun = "sd", na.rm = TRUE)
  
  # Number of *grid cells* that are not NA in the mean stack
  num_grid_cells <- global(uk_sst_mean_stack, fun = "notNA")
  names(num_grid_cells) <- "n_grid_cells"
  
  # Standard Error of the Overall UK Mean
  # This is the standard deviation of the cell means, divided by sqrt(number of cells).
  # This assumes each 1-degree cell mean is an independent observation contributing to the overall UK mean.
  se_overall_uk_mean <- between_cell_sd_temp$sd / sqrt(num_grid_cells$n_grid_cells)
  
  # Note: We are treating each 1 degree cell as an "observation" and ignoring any variation within each cell
  # This is reasonable given the resolution of the model.
  
  # Create the final data frame
  final_df <- data.frame(
    date = time(uk_sst_mean_stack),
    avg_temp = overall_mean_temp$mean,
    sd_temp = between_cell_sd_temp$sd, # Spatial variability of the 1-degree cell means
    n_cells = num_grid_cells$n_grid_cells,
    se_temp = se_overall_uk_mean, # The direct SEM for the overall UK mean
    scenario = scenario
  )
  
  all_scenarios_data[[scenario]] <- final_df
  
  output_path <- paste0("data/tidy_SST_data/uk_mean_monthly_", scenario, ".csv")
  message(paste("  -> Saving data to", output_path))
  write.csv(final_df, output_path, row.names = FALSE)
  
  message(paste("Finished processing:", scenario))
}


# 3. CHECK OUTPUT ---------------------------------------------------------

message("\n--- Processing Complete. Reviewing data: ---")
print(head(all_scenarios_data$sst_SSP5_8.5))
print(tail(all_scenarios_data$sst_SSP5_8.5))
summary(all_scenarios_data$sst_SSP5_8.5)

# 4. PLOTTING ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate) # Ensure lubridate is loaded for year() function

# Combine all scenario data into a single data frame
full_data <- bind_rows(all_scenarios_data)

# Calculate Standard Error of the Mean (SEM) for monthly data
# For a 95% CI, we typically use 1.96 * SEM (for large N, otherwise t-distribution)
# Given your number of cells is likely large enough, 1.96 is reasonable.
full_data <- full_data %>%
  mutate(
    ci_lower_monthly = avg_temp - 1.96 * se_temp,
    ci_upper_monthly = avg_temp + 1.96 * se_temp
  )

# --- Plot 1: Monthly SST with Confidence Interval ---
message("\n--- Generating Monthly SST Plot ---")
monthly_plot <- ggplot(full_data, aes(x = date, y = avg_temp, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = ci_lower_monthly, ymax = ci_upper_monthly), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Monthly Mean Sea Surface Temperature in UK AOI with 95% Confidence Interval",
    subtitle = "Aggregated from 1-degree raster cells",
    x = "Date",
    y = "Average SST (°C)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

print(monthly_plot)

# --- Prepare Data for Annual Plot ---
annual_data <- full_data %>%
  mutate(year = year(date)) %>%
  group_by(year, scenario) %>%
  summarise(
    avg_temp_annual = mean(avg_temp, na.rm = TRUE),
    # Sum of the SQUARED monthly standard errors (variances) for the year
    sum_monthly_se_sq = sum(se_temp^2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    # Annual SEM = sqrt(Sum of monthly SE^2) / Number of months
    # This assumes monthly means are independent.
    annual_sem = sqrt(sum_monthly_se_sq) / 12, # 12 months in a year
    ci_lower_annual = avg_temp_annual - 1.96 * annual_sem,
    ci_upper_annual = avg_temp_annual + 1.96 * annual_sem
  )

# --- Plot 2: Annual SST with Confidence Interval ---
message("\n--- Generating Annual SST Plot ---")
annual_plot <- ggplot(annual_data, aes(x = year, y = avg_temp_annual, color = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = ci_lower_annual, ymax = ci_upper_annual), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Annual Mean Sea Surface Temperature in UK AOI with 95% Confidence Interval",
    subtitle = "Standard Error of Annual Mean based on propagating monthly uncertainties",
    x = "Year",
    y = "Average SST (°C)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

print(annual_plot)
