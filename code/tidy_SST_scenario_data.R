


library(ncdf4)     # For reading NetCDF files
library(raster)    # To work with raster data (e.g., SST grids)
library(terra)     # A modern alternative to 'raster', also supports NetCDF
library(ggplot2)   # For plotting
library(viridis)   # Nice color scales
library(tidyverse) # For data wrangling
library(sf) # Often useful for shapefile handling, though terra can read them directly
library(lubridate)

# regridding one layer ----

scenarios <- data.frame(
  scenario_name = c("sst_SSP1_2.6", "sst_SSP2_4.5", "sst_SSP5_8.5"),
  path = c(
    "data/raw_SST_data/SSP1-2.6 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP2-4.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP5-8.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_20150116-20501216.nc"
  )
)

for (scenario in scenarios$scenario_name) {
  scenario_path <- scenarios |> filter(scenario_name == scenario) |> pull(path)
  
  nc <- nc_open(scenario_path)
  print(nc)
  
  lon_2d <- ncvar_get(nc, "longitude") # Read the 2D longitude array
  lat_2d <- ncvar_get(nc, "latitude") # Read the 2D latitude array
  tos_data <- ncvar_get(nc, "tos") # Read the sea surface temperature data
  
  nc_close(nc)
  
  tos_slice_1 <- tos_data[, , 1]
  
  lon_vec <- as.vector(lon_2d)
  lat_vec <- as.vector(lat_2d)
  val_vec <- as.vector(tos_slice_1)
  
  # Create a data.frame or matrix suitable for creating a SpatVector
  pts <- data.frame(lon = lon_vec, lat = lat_vec, value = val_vec)
  
  # Remove NA values if they represent land or missing data, to avoid issues with `vect`
  pts <- na.omit(pts)
  
  # Create a SpatVector from these points
  data_vect <- vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326")
  # The crs="EPSG:4326" assigns a geographic coordinate system (WGS84) to the points.
  
  # Define the desired resolution for your new grid (e.g., 1 degree resolution)
  res_deg <- 1 # Or 1 degree, or whatever resolution suits your needs
  
  # Define the extent of the new global grid (-180 to 180 longitude, -90 to 90 latitude)
  target_extent <- ext(-180, 180, -90, 90)
  
  # Create an empty SpatRaster to serve as the template for your new grid
  target_rast <- rast(target_extent, resolution = res_deg, crs = "EPSG:4326")
  
  # Regrid the data from the points to the target raster
  regridded_data <- rasterize(
    data_vect,
    target_rast,
    field = "value",
    fun = mean,
    na.rm = TRUE
  )
  
  
  # Cropping to geographical extent of 2014-2024 data ------------------------------------
  
  min_lon <- -8.33345
  max_lon <- 2.999769
  min_lat <- 48.53376
  max_lat <- 59.80099
  
  target_extent <- ext(min_lon, max_lon, min_lat, max_lat)
  print(target_extent)
  
  # Plot the cropped extent to confirm it looks right
  plot(target_extent)
  
  # 1. Crop to the bounding box
  sst_cropped <- crop(regridded_data, target_extent)
  
  plot(sst_cropped)
  
  # Prepping data for analysis ----------------------------------------------
  # need the mean monthly temperature for each month
  
  nc <- nc_open(scenario_path)
  
  # Extract the full 'tos' (Sea Surface Temperature) data array
  # This will be a 3D array: [x_dim, y_dim, time_dim]
  tos_all_layers <- ncvar_get(nc, "tos")
  
  # Extract the time dimension values and attributes for conversion
  time_dim_values <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  time_calendar <- ncatt_get(nc, "time", "calendar")$value # Often "gregorian" or "proleptic_gregorian"
  
  # 3. Convert NetCDF time values to R Date objects
  # The 'units' attribute usually looks like "days since YYYY-MM-DD" or "months since YYYY-MM-DD".
  # Let's parse the origin date from the units string:
  origin_match <- regmatches(time_units,
                             regexec("since (\\d{4}-\\d{2}-\\d{2})", time_units))
  if (length(origin_match) > 0 && length(origin_match[[1]]) > 1) {
    time_origin_date_str <- origin_match[[1]][2]
  } else {
    stop(
      "Could not automatically determine time origin from NetCDF 'units' attribute. Please inspect 'time_units' and set 'time_origin_date_str' manually (e.g., '1950-01-16')."
    )
  }
  
  # Determine the time interval based on units to decide on conversion method
  if (grepl("days", time_units)) {
    time_points_dates <- as.Date(time_dim_values, origin = time_origin_date_str)
  } else if (grepl("months", time_units)) {
    # For "months since YYYY-MM-DD", lubridate is helpful
    library(lubridate)
    origin_date_ymd <- ymd(time_origin_date_str)
    time_points_dates <- origin_date_ymd %m+% months(time_dim_values)
    # For monthly data, sometimes the date represents the middle of the month (e.g., 16th),
    # or the start/end. Ensure consistency with your lobster data.
  } else {
    stop(
      paste(
        "Unsupported time units:",
        time_units,
        ". Please adjust time conversion manually."
      )
    )
  }
  
  # Determine the total number of time layers (months in your case)
  n_time_layers <- dim(tos_all_layers)[3]
  
  # 4. Define your Target Regular Grid (re-use from previous steps)
  # This is the grid onto which you will regrid the curvilinear data.
  res_deg <- 0.25 # A finer resolution than 1 degree might be good for a smaller region, e.g., 0.25 degree
  UK_extent <- ext(-10, 5, 48, 62) # A tighter extent around the UK might be more efficient for regridding
  # (adjust these min/max lon/lat values to cover your EEZ fully)
  target_rast <- rast(target_extent, resolution = res_deg, crs = "EPSG:4326")
  
  # --- B. Main Loop for Processing Each Monthly Time Layer ---
  sst_for_regression_df <- data.frame(Date = as.Date(character()), Mean_UK_SST = numeric())
  
  # Loop through each time layer (month)
  for (i in 1:n_time_layers)  {
    current_date <- time_points_dates[i]
    message(paste0(
      "Processing layer ",
      i,
      " of ",
      n_time_layers,
      " (",
      format(current_date, "%Y-%m"),
      ")"
    ))
    
    # Extract data for the current time slice ([x_dim, y_dim])
    tos_slice <- tos_all_layers[, , i]
    
    # --- Handle potential missing values (important for data integrity) ---
    # Some climate model outputs use a specific value (e.g., 1e+20, -999) for missing data
    # instead of R's NA. Check your NetCDF attributes for "_FillValue" or "missing_value".
    # If so, convert them to R's NA:
    fill_value_attr <- ncatt_get(nc, "tos", "missing_value")
    if (!is.null(fill_value_attr$value)) {
      tos_slice[tos_slice == fill_value_attr$value] <- NA
    }
    
    # Flatten the 2D lon/lat and data for point creation
    # Ensure dimensions match. If lon_2d/lat_2d are [y,x] and tos_slice is [x,y], you'd need transpose.
    # Assuming they are consistent (e.g., both [x,y])
    lon_vec <- as.vector(lon_2d)
    lat_vec <- as.vector(lat_2d)
    val_vec <- as.vector(tos_slice)
    
    # Create a data frame of points with their actual geographic coordinates and data values
    pts <- data.frame(lon = lon_vec,
                      lat = lat_vec,
                      value = val_vec)
    
    # Remove points with NA values (e.g., land cells or missing data points)
    pts <- na.omit(pts)
    
    # Skip this time layer if no valid data points remain after NA removal
    if (nrow(pts) == 0) {
      warning(paste0(
        "    No valid data points for layer ",
        i,
        " (",
        format(current_date, "%Y-%m"),
        "). Skipping this month."
      ))
      next # Move to the next iteration of the loop
    }
    
    # Create a SpatVector from these points, assigning the geographic CRS
    data_vect <- vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326")
    
    regridded_slice <- rasterize(
      data_vect,
      target_rast,
      field = "value",
      fun = mean,
      na.rm = TRUE
    )
    
    sst_cropped <- crop(regridded_slice, target_extent)
    
    mean_sst_for_month <- global(sst_cropped, fun = "mean", na.rm = TRUE)
    
    # Add the result to our data frame
    sst_for_regression_df <- rbind(
      sst_for_regression_df,
      data.frame(Date = current_date, Mean_UK_SST = mean_sst_for_month$mean)
    ) # 'global' returns a data.frame, access the 'mean' column
    
  }
  
  # Review the final data frame
  head(sst_for_regression_df)
  tail(sst_for_regression_df)
  summary(sst_for_regression_df$Mean_UK_SST)
  
  sst_for_regression_df <- sst_for_regression_df |> 
    rename(date = Date, avg_temp = Mean_UK_SST)
  
  # Save this data frame for later use
  write.csv(
    sst_for_regression_df,
    paste0("data/tidy_SST_data/uk_mean_monthly_", scenario, ".csv"),
    row.names = FALSE
  )
}


# correcting calendar -----------------------------------------------------

library(ncdf4)
library(dplyr)
library(terra)
library(lubridate)

library(ncdf4) # Still useful if you need to inspect netCDF structure, but less for direct data extraction now
library(dplyr)
library(terra) # Main workhorse now
library(lubridate) # Still useful for date manipulations if needed later, but less for initial conversion

library(ncdf4) # Still loaded, but less critical for data extraction now
library(dplyr)
library(terra)
library(lubridate)

scenarios <- data.frame(
  scenario_name = c("sst_SSP1_2.6", "sst_SSP2_4.5", "sst_SSP5_8.5"),
  path = c(
    "data/raw_SST_data/SSP1-2.6 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP2-4.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP5-8.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_20150116-20501216.nc"
  )
)

# Define common spatial parameters outside the loop
res_deg <- 0.25 # Your target spatial resolution
min_lon <- -8.33345
max_lon <- 2.999769
min_lat <- 48.53376
max_lat <- 59.80099
target_extent <- ext(min_lon, max_lon, min_lat, max_lat)
target_rast_template <- rast(target_extent, resolution = res_deg, crs = "EPSG:4326")


for (scenario in scenarios$scenario_name) {
  scenario_path <- scenarios |> filter(scenario_name == scenario) |> pull(path)
  
  message(paste0("Processing scenario: ", scenario))
  
  # Use terra::rast to read the NetCDF file
  # terra automatically handles the variable name (tos), time dimension, and 360_day calendar
  sst_stack <- rast(scenario_path)
  
  if (min(ext(sst_stack)$xmin) >= 0 && max(ext(sst_stack)$xmax) > 180) {
    message("Rotating longitude from 0-360 to -180-180 for scenario: ", scenario)
    sst_stack <- rotate(sst_stack) # This handles the longitude conversion
  }
  
  # Check the structure and time (for debugging/confirmation)
  message("\n--- SpatRaster Information for current scenario ---")
  print(sst_stack)
  message("Time points extracted by terra:")
  print(head(time(sst_stack)))
  print(tail(time(sst_stack)))
  message("---------------------------------------------------\n")
  
  # Extract time points directly from the SpatRaster stack
  time_points_dates <- time(sst_stack) # These are now correctly converted dates
  
  # No need for Kelvin to Celsius conversion as 'unit : degC' is confirmed by print(sst_stack)
  
  # --- No need to crop time extent to January 2014, as data correctly starts Jan 2015 ---
  # We will process all layers as read by terra.
  sst_stack_to_process <- sst_stack
  time_points_dates_to_process <- time_points_dates
  n_time_layers_to_process <- nlyr(sst_stack_to_process)
  
  
  # --- Main Loop for Processing Each Monthly Time Layer ---
  sst_for_regression_df <- data.frame(Date = as.Date(character()), Mean_UK_SST = numeric())
  
  for (i in 1:n_time_layers_to_process) {
    current_date <- time_points_dates_to_process[i]
    message(paste0(
      "Processing monthly layer ", i, " of ", n_time_layers_to_process,
      " (", format(current_date, "%Y-%m"), ") for ", scenario
    ))
    
    # Extract the current layer (time slice) from the stack
    current_sst_layer <- sst_stack_to_process[[i]]
    
    # Convert current_sst_layer to points and then rasterize to the target grid
    # This step is appropriate if the source data is on a curvilinear grid
    pts <- as.data.frame(current_sst_layer, xy=TRUE) # Extracts x, y (lon, lat) and value
    names(pts)[3] <- "value" # Rename value column
    pts <- na.omit(pts) # Remove NA values (e.g., land or missing data)
    
    if (nrow(pts) == 0) {
      warning(paste0("No valid data points after NA removal for layer ", i, " (", format(current_date, "%Y-%m"), "). Skipping."))
      next
    }
    
    data_vect <- vect(pts, geom = c("x", "y"), crs = "EPSG:4326")
    
    regridded_slice <- rasterize(
      data_vect,
      target_rast_template,
      field = "value",
      fun = mean, # Mean of points within each target grid cell
      na.rm = TRUE
    )
    
    # Crop the regridded slice to your defined UK extent
    sst_cropped <- crop(regridded_slice, target_extent)
    
    # Calculate the mean SST for the cropped region for this month
    mean_sst_for_month <- global(sst_cropped, fun = "mean", na.rm = TRUE)$mean
    
    # Add the result to our data frame
    sst_for_regression_df <- rbind(
      sst_for_regression_df,
      data.frame(Date = current_date, Mean_UK_SST = mean_sst_for_month)
    )
  }
  
  sst_for_regression_df <- sst_for_regression_df |>
    rename(avg_temp = Mean_UK_SST)
  
  # Review the final data frame
  head(sst_for_regression_df)
  tail(sst_for_regression_df)
  summary(sst_for_regression_df$Mean_UK_SST)

  
 
}

# Save this data frame for later use
write.csv(
  sst_for_regression_df,
  paste0("data/tidy_SST_data/uk_mean_monthly_", scenario, ".csv"),
  row.names = FALSE
)

sst_for_regression_df[which.min(sst_for_regression_df$avg_temp), ]
observed_average_monthly_temperature[which.max(observed_average_monthly_temperature$avg_temp), ]

sst_for_regression_df <- sst_for_regression_df |> 
  rename(date = Date, avg_temp = Mean_UK_SST)


# attempt 263 -------------------------------------------------------------

scenarios <- data.frame(
  scenario_name = c("sst_SSP1_2.6", "sst_SSP2_4.5", "sst_SSP5_8.5"),
  path = c(
    "data/raw_SST_data/SSP1-2.6 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp126_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP2-4.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp245_r1i1p1f3_gn_20150116-20501216.nc",
    "data/raw_SST_data/SSP5-8.5 SST CMIP6 climate projections HadGEM3-GC31-LL 2015-2050/tos_Omon_HadGEM3-GC31-LL_ssp585_r1i1p1f3_gn_20150116-20501216.nc"
  )
)

# Define common spatial parameters outside the loop
res_deg <- 1 # Your target spatial resolution
min_lon <- -8.33345
max_lon <- 2.999769
min_lat <- 48.53376
max_lat <- 59.80099
target_extent <- ext(min_lon, max_lon, min_lat, max_lat)
target_rast_template <- rast(target_extent, resolution = res_deg, crs = "EPSG:4326")


for (scenario in scenarios$scenario_name) {
  scenario_path <- scenarios |> filter(scenario_name == scenario) |> pull(path)
  
  message(paste0("Processing scenario: ", scenario))
  
  # Use terra::rast to read the NetCDF file
  sst_stack <- rast(scenario_path)
  
  # --- NEW: Rotate longitude from 0-360 to -180-180 if needed ---
  # Check the extent of the loaded stack. If xmin is non-negative and xmax goes beyond 180, it's likely 0-360.
  if (min(ext(sst_stack)$xmin) >= 0 && max(ext(sst_stack)$xmax) > 180) {
    message("Rotating longitude from 0-360 to -180-180 for scenario: ", scenario)
    sst_stack <- rotate(sst_stack) # This handles the longitude conversion
  }
  
  # Check the structure and time (for debugging/confirmation)
  message("\n--- SpatRaster Information for current scenario (after potential rotation) ---")
  print(sst_stack) # Observe the new extent after rotation: it should be -180 to 180.
  message("Time points extracted by terra:")
  print(head(time(sst_stack)))
  print(tail(time(sst_stack)))
  message("---------------------------------------------------\n")
  
  # Extract time points directly from the SpatRaster stack
  time_points_dates <- time(sst_stack)
  
  sst_stack_to_process <- sst_stack
  time_points_dates_to_process <- time_points_dates
  n_time_layers_to_process <- nlyr(sst_stack_to_process)
  
  sst_for_regression_df <- data.frame(Date = as.Date(character()), Mean_UK_SST = numeric())
  
  for (i in 1:n_time_layers_to_process) {
    current_date <- time_points_dates_to_process[i]
    message(paste0(
      "Processing monthly layer ", i, " of ", n_time_layers_to_process,
      " (", format(current_date, "%Y-%m"), ") for ", scenario
    ))
    
    current_sst_layer <- sst_stack_to_process[[i]]
    
    # Convert current_sst_layer to points and then rasterize to the target grid
    # This step is appropriate if the source data is on a curvilinear grid
    pts <- as.data.frame(current_sst_layer, xy=TRUE) # Extracts x, y (lon, lat) and value
    names(pts)[3] <- "value" # Rename value column
    pts <- na.omit(pts) # Remove NA values (e.g., land or missing data)
    
    if (nrow(pts) == 0) {
      warning(paste0("No valid data points after NA removal for layer ", i, " (", format(current_date, "%Y-%m"), "). Skipping."))
      next
    }
    
    data_vect <- vect(pts, geom = c("x", "y"), crs = "EPSG:4326")
    
    regridded_slice <- rasterize(
      data_vect,
      target_rast_template,
      field = "value",
      fun = mean, # Mean of points within each target grid cell
      na.rm = TRUE
    )
    
    # Crop the regridded slice to your defined UK extent
    # This crop will now work correctly as the longitudes are aligned
    sst_cropped <- crop(regridded_slice, target_extent)
    
    # Calculate the mean SST for the cropped region for this month
    mean_sst_for_month <- global(sst_cropped, fun = "mean", na.rm = TRUE)$mean
    
    # Add the result to our data frame
    sst_for_regression_df <- rbind(
      sst_for_regression_df,
      data.frame(Date = current_date, Mean_UK_SST = mean_sst_for_month)
    )
  }
  
  sst_for_regression_df <- sst_for_regression_df |>
    rename(date = Date, avg_temp = Mean_UK_SST)
  
  message(paste0("Finished processing ", scenario, ". Final data summary:"))
  print(head(sst_for_regression_df))
  print(tail(sst_for_regression_df))
  summary(sst_for_regression_df$avg_temp)
}
