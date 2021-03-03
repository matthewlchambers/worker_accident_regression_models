# Title     : Get Weather Data
# Objective : Download and extract some weather data as covariates for my analysis
# Created by: Matthew Chambers, with thanks to Bing Yang Tan for reminding me about the raster package and wget and
# introducing me to exactextractr
# Created on: 7/10/2020

# Load the necesary packages. Raster provides some excellent tools for working with geodata and exactextractr
# is a tool for getting summary statistics from a raster image by region using a shapefile. Tidyverse is for
# general data manipulation, magrittr supplies the pipe operators such as %>% and %<>%, and lubridate provides
# tools for working intelligently with dates.
library(raster)
library(exactextractr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)

# NOTE: The abbreviation 'geo_height' below refers to geopotential height, an estimate of the height
# of a particular pressure level (or the earth's surface) above mean sea level. I use this to ensure that
# I am identifying thermal inversions in a consistent way.

# Define a helper function that will be repeatedly useful. Note it returns a string when given an integer.
z_pad <- function (i) return (str_pad(i, 2, pad = '0'))

# Define input parameters for the shapefile outlining the regions for which I want summarized data
shapefile_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/County Shapefile'
shapefile_file <- function () return ('cb_2016_us_county_20m.shp')

## Define input parameters for the population density raster
#population_density_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Population Density'
#population_density_file <- function () return ('pden2010_60m.tif')

# Define input parameters for the files we wish to download. Note that the ftp directories must include / at
# the end, since they will be pasted toether with the filename to get a url.
surface_temperature_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
surface_temperature_file <- function (year) return (paste0('air.2m.', year, '.nc'))
surface_geo_height_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/time_invariant/'
surface_geo_height_file <- function () return (paste0('hgt.sfc.nc')) # Note this is identical for all years.
temperature_levels_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/'
temperature_levels_file <- function (year, month) return (paste0('air.', year, z_pad(month), '.nc'))
geo_height_levels_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/'
geo_height_levels_file <- function (year, month) return (paste0('hgt.', year, z_pad(month), '.nc'))

# Define output parameters for the output data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Weather Data'
output_file <- function (year, type = NULL) return (paste0(year, '_', type, '_data', '.csv'))

## For testing purposes, before I run this locally using a for loop.
year <- 2016
month <- 1
day <- 1

# Define height relative to surface (in meters) at which to test for inversions
height <- 200

# Define begin and end years
begin_year <- 2016
end_year <- 2016

# Get the time-invariant file that gives surface geopotential height outside the for loop for efficiency.
setwd(tempdir())
system(paste('wget -nc', paste0(surface_geo_height_dir, surface_geo_height_file())))
surface_geo_height_layer <- raster(surface_geo_height_file())

# Likewise, open the time-invariant shapefile with county information
counties <- st_read(file.path(shapefile_dir, shapefile_file()))

# Define the function that identifies the temperature at a given height above the earth's surface by
# linear interpolation between pressure levels above and below the given height. This uses the geopotential height
# at the earth's surfac, together with the geopotential height of the various pressure levels. This function will
# be used iteratively, moving upward through the various pressure levels of the atmosphere. 6 rasters are givn as
# inputs, along with the desired height above the surface. These 4 rasters are: a raster of temperature at the given
# heigh, which is initially empty but is iteratively filled in, and rasters of surface geopotential height,
# geopotential height at pressure levels i and i + 1 (notates as j in the function), and temperature at pressure
# levels i and i + 1 (j).
temperature_interpolation <- function (h, output, surf_geo_height, geo_height_i, geo_height_j, temp_i, temp_j) {
  # Use nested conditional statements to fill in pixels with the temperature at the given height above
  # the surface, if we are on the correct layer to do so.
  ifelse(
    # If any of the applicable rasters have NA values, we can't make the appropriate calculation, so make no change
    is.na(surf_geo_height) | is.na(geo_height_i) | is.na(geo_height_j) | is.na(temp_i) | is.na(temp_j),
    output,
    ifelse(
      # If layer i happens to be at exactly the right height above the surface, use its temperature as is.
      geo_height_i == surf_geo_height + h,
      temp_i,
      ifelse(
        # If layer i is below and layer j is above the desired height (above the surface), then use linear
        # interpolation to estimate the temperature at the given height above the surface.
        geo_height_i < surf_geo_height + h & geo_height_j > surf_geo_height,
        temp_i + (temp_j - temp_i) * ((surf_geo_height + h - geo_height_i) / (geo_height_j - geo_height_i)),
        # If the desired height is not between layers i and j, then make no change to the output raster. Recall
        # that this function is being applied iteratively over the pressure levels, so an earlier or later
        # iteration should hae the desired information.
        output
      )
    )
  )
}

# Define the function that implements the temperature_interpolation function above at 200 m above the surface
temp_interp_at_200m <- function (...) {
  temperature_interpolation(200, ...)
}

# Define the function that implements the temperature_interpolation function above at 400 m above the surface
temp_interp_at_400m <- function (...) {
  temperature_interpolation(400, ...)
}

# Define the function that implements the temperature_interpolation function above at 600 m above the surface
temp_interp_at_600m <- function (...) {
  temperature_interpolation(600, ...)
}

## Open the population density raster and reproject it to match the other rasters for calculating
## population weighted averages later
#population_density_raster <- raster(file.path(population_density_dir, population_density_file()))
#population_density_raster %<>% projectRaster(surface_geo_height_brick)

#for (year in begin_year:end_year) {
  # Download the surface temperature file for the year in question to the temporary directory, then open it
  # as a raster brick
  system(paste('wget -nc', paste0(surface_temperature_dir, surface_temperature_file(year))))
  surface_temperature_brick <- brick(surface_temperature_file(year)) # tempdir() already set as working directory

  #for (month in 1:12) {
    # Download this month's files for temperature and geopotential height at different pressure levels.
    system(paste('wget -nc', paste0(temperature_levels_dir, temperature_levels_file(year, month))))
    system(paste('wget -nc', paste0(geo_height_levels_dir, geo_height_levels_file(year, month))))


    # Loop through all the days in the current month. There may be a better way to get the number of the last day.
    #for (day in 1:day(ymd(paste(year, month, '1', sep = '-')) + months(1) - days(1))) {
      # Open the files so I can extract data. tempdir() already set as working directory.
      surface_temperature_day <- surface_temperature_brick[[paste0('X', year, '.', z_pad(month), '.',
                                                                   z_pad(day), '.00.00.00')]]
      temperature_levels_day_brick <- brick(temperature_levels_file(year, month), lvar = 4, level = day)
      geo_height_levels_day_brick <- brick(geo_height_levels_file(year, month), lvar = 4, level = day)

      # Initialize empty rasters for temperature at different levels. These will later be assembled into
      # a brick.
      temperature_at_200m_day <- (surface_temperature_day * 0) %>% setValues(NA)
      temperature_at_400m_day <- (surface_temperature_day * 0) %>% setValues(NA)
      temperature_at_600m_day <- (surface_temperature_day * 0) %>% setValues(NA)

      # Iterate over the different pressure levels. I believe 20 should be more than enough to get even the
      # highest points in the contiguous United States.
      for (i in 1:20) {
        temperature_at_200m_day <- overlay(
          temperature_at_200m_day, surface_geo_height_layer,
          geo_height_levels_day_brick[[i]], geo_height_levels_day_brick[[i + 1]],
          temperature_levels_day_brick[[i]], temperature_levels_day_brick[[i + 1]],
          fun = temp_interp_at_200m
        )
        temperature_at_400m_day <- overlay(
          temperature_at_400m_day, surface_geo_height_layer,
          geo_height_levels_day_brick[[i]], geo_height_levels_day_brick[[i + 1]],
          temperature_levels_day_brick[[i]], temperature_levels_day_brick[[i + 1]],
          fun = temp_interp_at_400m
        )
        temperature_at_600m_day <- overlay(
          temperature_at_600m_day, surface_geo_height_layer,
          geo_height_levels_day_brick[[i]], geo_height_levels_day_brick[[i + 1]],
          temperature_levels_day_brick[[i]], temperature_levels_day_brick[[i + 1]],
          fun = temp_interp_at_600m
        )
      }

      # Form a raster brick giving the temperatures at different heights, and naming the layers accordingly,
      # for use in extracting county averages.
      temperature_heights_day_brick <- brick(surface_temperature_day,
                                             temperature_at_200m_day,
                                             temperature_at_400m_day,
                                             temperature_at_600m_day)
      names(temperature_heights_day_brick) <- c('0m', '200m', '400m', '600m')

      # Extract the temperature data and create a tibble with the fips information attached.
      mean_temperature_heights_day_data <- temperature_heights_day_brick %>% exact_extract(counties, 'mean') %>%
        bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value)) %>%
        mutate(date = ymd(paste(year, month, day, sep = '-')))

      #mean_precipitation_data <- precipitation_brick %>% exact_extract(counties, 'mean') %>%
      #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
      #mean_wind_speed_data <- wind_speed_brick %>% exact_extract(counties, 'mean') %>%
      #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
      #mean_u_wind_unit_data <- u_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
      #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
      #mean_v_wind_unit_data <- v_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
      #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
      #mean_relative_humidity_data <- relative_humidity_brick %>% exact_extract(counties, 'mean') %>%
      #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))

      ## Reshape the tibbles so that each observation corresponds to a county-day, and change the date variable to
      ## an R readable date variables.
      #mean_temperature_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'temperature'
      #  ) %>% mutate(date = date %>% ymd_hms() %>% date())
      #mean_precipitation_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'precipitation'
      #  ) %>% mutate(date = date %>% ymd())
      #mean_wind_speed_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'wind_speed'
      #  ) %>% mutate(date = date %>% ymd_hms() %>% date())
      #mean_relative_humidity_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'relative_humidity'
      #  ) %>% mutate(date = date %>% ymd_hms() %>% date())
      #
      ## Reshape the average u- and v- wind direction unit vector components so the average wind direction can be calculated
      #mean_u_wind_unit_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'u_wind_unit'
      #  ) %>% mutate(date = date %>% ymd_hms() %>% date())
      #mean_v_wind_unit_data %<>%
      #  pivot_longer(
      #    starts_with('mean.X'),
      #    names_to = 'date',
      #    names_prefix = 'mean.X',
      #    values_to = 'v_wind_unit'
      #  ) %>% mutate(date = date %>% ymd_hms() %>% date())
      #
      ## Use average u- and v- unit vector components to get unit vector average wind direction: average wind direction
      ## unweighted by wind speed.
      #mean_wind_direction_data <- left_join(mean_u_wind_unit_data, mean_v_wind_unit_data) %>%
      #  mutate(wind_direction = (180 / pi) * atan2(u_wind_unit, v_wind_unit) + 180)
      #
      ## Merge the weather data together into a single tibble, then save as a csv
      #weather_data <- mean_temperature_data %>% left_join(mean_precipitation_data) %>%
      #  left_join(mean_wind_speed_data) %>% left_join(mean_wind_direction_data %>%
      #                                                select(-c(u_wind_unit, v_wind_unit))) %>%
      #  left_join(mean_relative_humidity_data)
      #write_csv(weather_data, file.path(output_dir, output_file(year,'temp_precip_wind_hum')))
#    }
#  }
#}
