
library(ncdf4)
library(raster)
library(ggplot2)
library(sf)

# Define the file path (assuming the file is in your working directory)
# (old 0-50m) nc_file_path <- "data/raw_SST_data/NWSHELF_MULTIYEAR_PHY_004_009_tempmonthly.nc"
nc_file_path <- "data/raw_SST_data/cmems_mod_nws_phy-t_my_7km-3D_P1M-m_1752132330263.nc"

# Open the NetCDF file
nc_data <- nc_open(nc_file_path)

# You can inspect the file structure to understand its dimensions and variables
print(nc_data)

lon_values <- nc_data$dim$longitude$vals
cat("Min Longitude:", min(lon_values), "\n")
cat("Max Longitude:", max(lon_values), "\n")

lat_values <- nc_data$dim$latitude$vals
cat("Min Latitude: ", min(lat_values), "\n")
cat("Max Latitude: ", max(lat_values), "\n")

temp_var_name <- "thetao" 
depth_var_name <- "depth"

time_var <- ncvar_get(nc_data, "time")
time_units <- ncatt_get(nc_data, "time", "units")$value
time_origin <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
dates <- time_origin + time_var # This should be your vector of all dates

num_depth_levels <- nc_data$dim[[depth_var_name]]$len

temperature_brick_0m <- brick(nc_file_path, varname = temp_var_name, level = 1)
print(temperature_brick_0m)

if (nlayers(temperature_brick_0m) == length(dates)) {
  names(temperature_brick_0m) <- dates
  print("Layer names assigned to dates.")
  print(head(names(temperature_brick_0m)))
  print(tail(names(temperature_brick_0m)))
} else {
  stop(paste("ERROR: Number of layers in brick (", nlayers(temperature_brick_0m),
             ") does not match number of dates (", length(dates), ").",
             " 'raster::brick' did not interpret the time dimension as layers correctly. Additional debugging needed."))
}

temp_df_raw <- as.data.frame(temperature_brick_0m, xy = TRUE, na.rm = TRUE, long = TRUE, value = TRUE)
colnames(temp_df_raw) <- c("longitude", "latitude", "date_raw", "temperature")
observed_temperature_data <- temp_df_raw # Assigning to final object name now
observed_temperature_data$date <- as.POSIXct(observed_temperature_data$date_raw, tz = "UTC")
observed_temperature_data$date_raw <- NULL

# 13. Extract year and month (these will now work correctly)
observed_temperature_data$year <- format(observed_temperature_data$date, "%Y")
observed_temperature_data$month <- format(observed_temperature_data$date, "%m")

print(head(observed_temperature_data)) # Still expects to show 2014-01-01 for many rows
print(tail(observed_temperature_data)) # ***** THIS IS THE KEY CHECK NOW: It should show 2024 dates *****
print(str(observed_temperature_data))
print("Unique dates in data:")
print(unique(observed_temperature_data$date)) # Should show all 126 dates
print(paste("Number of unique dates:", length(unique(observed_temperature_data$date))))

save(
  observed_temperature_data, 
  file = "data/tidy_SST_data/observed_temperature_data.RData"
)


# Plot sea temperature ----------------------------------------------------

month_to_plot <- as.POSIXct("2014-01-01", tz = "UTC")
single_month_data <- subset(observed_temperature_data, date == month_to_plot)

extent_plot_single_month <- ggplot(data = single_month_data, aes(x = longitude, y = latitude, fill = temperature)) +
  geom_raster() + # Creates the colored grid
  scale_fill_viridis_c(option = "C", name = "Temperature (°C)") + # Good color palette
  coord_quickmap() + # Ensures correct aspect ratio for geographical data
  labs(
    title = paste("Sea Temperature for", format(month_to_plot, "%B %Y")), # Title includes Month Year
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() + # Clean theme
  # Add country borders for context. Adjust xlim/ylim to focus on the UK area.
  borders("world",
          xlim = c(-15, 10), # Adjust as needed to clearly see the UK data extent
          ylim = c(45, 65),  # Adjust as needed
          colour = "black", fill = "grey90", size = 0.2)

print(extent_plot_single_month)


# Making Monthly Average --------------------------------------------------

observed_average_monthly_temperature <- observed_temperature_data |> 
  group_by(date) |>  
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE), .groups = "drop" 
  )

head(observed_average_monthly_temperature)
tail(observed_average_monthly_temperature)
str(observed_average_monthly_temperature)
summary(observed_average_monthly_temperature$avg_temp)

plot(observed_average_monthly_temperature)
lines(observed_average_monthly_temperature, col = "red")

observed_average_monthly_temperature[which.min(observed_average_monthly_temperature$avg_temp), ]
observed_average_monthly_temperature[which.max(observed_average_monthly_temperature$avg_temp), ]

# Change so date columns as both Date rather than Posixt
observed_average_monthly_temperature <- observed_average_monthly_temperature |> mutate(date = as.Date(date))

save(
  observed_average_monthly_temperature, 
  file = "data/tidy_SST_data/observed_average_monthly_temperature.RData"
)
