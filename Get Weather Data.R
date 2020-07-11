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

# Define input parameters for the shapefile outlining the regions for which we want summarized data
shapefile_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/County Shapefile'
shapefile_file <- function () return('cb_2016_us_county_20m.shp')

# Define input parameters for the files we wish to download. Note that the ftp directories must inlcude / at
# the end, since they will be pasted to gether with the filename to get a url.
precipitation_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
precipitation_file <- function (year) return(paste0('apcp.', year, '.nc'))
temperature_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
temperature_file <- function (year) return(paste0('air.2m.', year, '.nc'))

# Define output parameters for the output data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Weather Data'
output_file <- function (year, type = NULL) return(paste0(year, '_', type, '_data', '.csv'))

## For testing purposes, before I run this locally using a for loop.
#year <- 2005

# Define begin and end years
begin_year <- 2003
end_year <- 2016


for (year in begin_year:end_year) {
  # Download the files for the year in question to the temporary directory.
  setwd(tempdir())
  system(paste('wget -nc', paste0(precipitation_dir, precipitation_file(year))))
  system(paste('wget -nc', paste0(temperature_dir, temperature_file(year))))

  # Open the files so I can extract data. Raster bricks are incredibly convenient to work with.
  precipitation_brick <- brick(precipitation_file(year)) # tempdir() already set as working directory
  temperature_brick <- brick(temperature_file(year)) # tempdir() already set as working directory

  # Open the shapefile, so I can extract daily means of the weather variables by county.
  counties <- st_read(file.path(shapefile_dir, shapefile_file()))

  # Extract the weather data and create tibbles with the fips information attached.
  mean_temperature_data <- temperature_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  mean_precipitation_data <- precipitation_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))

  # Reshape the tibbles so that each observation corresponds to a county-day, and change the date variable to
  # an R readable date variables.
  mean_temperature_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'mean_temperature'
    ) %>% mutate(date = date %>% ymd())
  mean_precipitation_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'mean_precipitation'
    ) %>% mutate(date = date %>% ymd())

  # Merge the weather data together into a single tibble, then save as a csv
  weather_data <- left_join(mean_temperature_data, mean_precipitation_data)
  write_csv(weather_data, file.path(output_dir, output_file(year,'mean_temp_mean_precip')))
}
