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

# Define input parameters for the shapefile outlining the regions for which I want summarized data
shapefile_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/County Shapefile'
shapefile_file <- function () return ('cb_2016_us_county_20m.shp')

## Define input parameters for the population density raster
#population_density_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Population Density'
#population_density_file <- function () return ('pden2010_60m.tif')

# Define input parameters for the files we wish to download. Note that the ftp directories must inlcude / at
# the end, since they will be pasted toether with the filename to get a url.
#precipitation_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
#precipitation_file <- function (year) return (paste0('apcp.', year, '.nc'))
#temperature_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
#temperature_file <- function (year) return (paste0('air.2m.', year, '.nc'))
u_wind_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
u_wind_file <- function (year) return (paste0('uwnd.10m.', year, '.nc'))
v_wind_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
v_wind_file <- function (year) return (paste0('vwnd.10m.', year, '.nc'))
relative_humidity_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
relative_humidity_file <- function (year) return (paste0('rhum.2m.', year, '.nc'))

# Define output parameters for the output data file(s) to be saved.
output_file <- function (year, type = NULL) return (paste0(year, '_', type, '_data', '.csv'))


# Define begin and end years
begin_year <- 2004
end_year <- 2015


for (year in begin_year:end_year) {
  # Download the files for the year in question to the temporary directory.
  setwd(tempdir())
  #system(paste('wget -nc', paste0(precipitation_dir, precipitation_file(year))))
  #system(paste('wget -nc', paste0(temperature_dir, temperature_file(year))))
  system(paste('wget -nc', paste0(u_wind_dir, u_wind_file(year))))
  system(paste('wget -nc', paste0(v_wind_dir, v_wind_file(year))))
  system(paste('wget -nc', paste0(relative_humidity_dir, relative_humidity_file(year))))

  # Open the files so I can extract data. Raster bricks are incredibly convenient to work with.
  #precipitation_brick <- brick(precipitation_file(year)) # tempdir() already set as working directory
  #temperature_brick <- brick(temperature_file(year))
  u_wind_brick <- brick(u_wind_file(year))
  v_wind_brick <- brick(v_wind_file(year))
  relative_humidity_brick <- brick(relative_humidity_file(year))

  ## Open the population density raster and reproject it to match the other rasters for calculating
  ## population weighted averages later
  #population_density_raster <- raster(file.path(population_density_dir, population_density_file()))
  #population_density_raster %<>% projectRaster(precipitation_brick)

  # Calculate wind speed from u- and v-components. Normalize those components to unit vector directions
  # by dividing by wind speed. Average wind direction will be calculated later.
  wind_speed_brick <- (u_wind_brick ^ 2 + v_wind_brick ^ 2) ^ 0.5
  u_wind_unit_brick <- u_wind_brick / wind_speed_brick
  v_wind_unit_brick <- v_wind_brick / wind_speed_brick

  # Assign correct names (with date information) to layers in the raster bricks just created by calculation
  names(wind_speed_brick) <- names(u_wind_brick)
  names(u_wind_unit_brick) <- names(u_wind_brick)
  names(v_wind_unit_brick) <- names(u_wind_brick)

  # Open the shapefile, so I can extract daily means of the weather variables by county.
  counties <- st_read(file.path(shapefile_dir, shapefile_file()))

  # Extract the weather data and create tibbles with the fips information attached.
  #mean_temperature_data <- temperature_brick %>% exact_extract(counties, 'mean') %>%
  #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  #mean_precipitation_data <- precipitation_brick %>% exact_extract(counties, 'mean') %>%
  #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_wind_speed_data <- wind_speed_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_u_wind_unit_data <- u_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_v_wind_unit_data <- v_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_relative_humidity_data <- relative_humidity_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_wind_speed_data <- wind_speed_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_u_wind_unit_data <- u_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_v_wind_unit_data <- v_wind_unit_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  # The lines below need to be adjusted to reflect a circular mean, then uncommented
  #mean_wind_direction_data <- wind_direction_brick %>% exact_extract(counties, 'mean') %>%
  #  bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))

  # Reshape the tibbles so that each observation corresponds to a county-day, and change the date variable to
  # an R readable date variables.
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
  mean_wind_speed_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'wind_speed'
    ) %>% mutate(date = date %>% ymd() %>% date())
  mean_relative_humidity_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'relative_humidity'
    ) %>% mutate(date = date %>% ymd() %>% date())

  # Reshape the average u- and v- wind direction unit vector components so the average wind direction can be calculated
  mean_u_wind_unit_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'u_wind_unit'
    ) %>% mutate(date = date %>% ymd() %>% date())
  mean_v_wind_unit_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'v_wind_unit'
    ) %>% mutate(date = date %>% ymd() %>% date())

  # Use average u- and v- unit vector components to get unit vector average wind direction: average wind direction
  # unweighted by wind speed.
  mean_wind_direction_data <- left_join(mean_u_wind_unit_data, mean_v_wind_unit_data) %>%
    mutate(wind_direction = (180 / pi) * atan2(u_wind_unit, v_wind_unit) + 180)

  # Merge the weather data together into a single tibble, then save as a csv
  #weather_data <- mean_temperature_data %>% left_join(mean_precipitation_data) %>%
  #  left_join(mean_wind_speed_data) %>% left_join(mean_wind_direction_data %>%
  #                                                select(-c(u_wind_unit, v_wind_unit))) %>%
  #  left_join(mean_relative_humidity_data)
  #write_csv(weather_data, file.path(output_dir, output_file(year,'temp_precip_wind_hum')))
  weather_data <- mean_wind_speed_data %>% left_join(mean_wind_direction_data %>% select(-c(u_wind_unit, v_wind_unit))) %>%
    left_join(mean_relative_humidity_data)
  write_csv(weather_data, file.path(output_dir, output_file(year,'wind_hum')))
}
