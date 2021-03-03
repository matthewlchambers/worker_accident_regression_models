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
boundary_height_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
boundary_height_file <- function (year) return (paste0('hpbl.', year, '.nc'))
surface_temperature_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/'
surface_temperature_file <- function (year) return (paste0('air.2m.', year, '.nc'))
surface_geo_height_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/time_invariant/'
surface_geo_height_file <- function () return (paste0('hgt.sfc.nc')) # Note this is identical for all years.
levels_temperature_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/'
levels_temperature_file <- function (year, month) return (paste0('air.', year, z_pad(month), '.nc'))
levels_geo_height_dir <- 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/'
levels_geo_height_file <- function (year, month) return (paste0('hgt.', year, z_pad(month), '.nc'))

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
surface_geo_height <- raster(surface_geo_height_file())

# Likewise, open the time-invariant shapefile with county information
counties <- st_read(file.path(shapefile_dir, shapefile_file()))

## Open the population density raster and reproject it to match the other rasters for calculating
## population weighted averages later
#population_density_raster <- raster(file.path(population_density_dir, population_density_file()))
#population_density_raster %<>% projectRaster(surface_geo_height_brick)

for (year in begin_year:end_year) {
  # Initialize the tibble that will hold this year's data
  year_data <- tibble()

  # Download the surface temperature file for the year in question to the temporary directory, then open it
  # as a raster brick
  system(paste('wget -nc', paste0(surface_temperature_dir, surface_temperature_file(year))))
  surface_temperature_year <- brick(surface_temperature_file(year)) # tempdir() already set as working directory
  names(surface_temperature_year) <- str_trunc(names(surface_temperature_year), 11, ellipsis = '')

  for (month in 1:12) {
    # Download this month's files for temperature and geopotential height at different pressure levels.
    system(paste('wget -nc', paste0(levels_temperature_dir, levels_temperature_file(year, month))))
    system(paste('wget -nc', paste0(levels_geo_height_dir, levels_geo_height_file(year, month))))


    # Loop through all the days in the current month. There may be a better way to get the number of the last day.
    for (day in 1:day(ymd(paste(year, month, '1', sep = '-')) + months(1) - days(1))) {
      # Open the files so I can extract data. tempdir() already set as working directory.
      surface_temperature_day <- surface_temperature_year[[paste0('X', year, '.', z_pad(month), '.', z_pad(day))]]
      levels_temperature_day <- brick(levels_temperature_file(year, month), lvar = 4, level = day)
      levels_geo_height_day <- brick(levels_geo_height_file(year, month), lvar = 4, level = day)

      # Initialize raster to identify first pressure level above surface.
      first_level <- (surface_temperature_day * 0) %>% setValues(1)

      # Iterate over the different pressure levels to fill in the raster which gives the index of the first pressure
      # level that is at least 100 meters above the surface so the first layer is thick enough for the calculated lapse
      # rates to make sense
      for (i in 2:25) {
        first_level <- first_level + (levels_geo_height_day[[i - 1]] <= surface_geo_height + 100)
      }

      # Create a raster brick with 4 layers, giving the temperature at each of the first 4 pressure levels above
      # the Earth's surface
      first_levels_temperature <- calc(
        stack(first_level, levels_temperature_day),
        fun = function (x) {
          level <- x[1]
          temps <- x[-1] # Note that unlike in Python, this notation means "everything but index number 1"
          return (c(temps[level], temps[level + 1], temps[level + 2], temps[level + 3]))
        }
      )

      # Create a similar raster brick giving the geopotential height of each of the first 4 pressure levels above
      # the Earth's surface
      first_levels_height <- calc(
        stack(first_level, levels_geo_height_day),
        fun = function (x) {
          level <- x[1]
          heights <- x[-1] # Note that unlike in Python, this notation means "everything but index number 1"
          return (c(heights[level], heights[level + 1], heights[level + 2], heights[level + 3]))
        }
      )

      # For each of the raster bricks created above, combine it with the surface temperature/geopotential height
      first_levels_temperature %<>% brick(surface_temperature_day, .)
      first_levels_height %<>% brick(surface_geo_height, .)

      # Now, calculate the thickness of each layer, which I need for its own sake and to calculate the
      # environmental lapse rate
      layer_height <- calc(
        first_levels_height,
        fun = function (x) {
          return(c(x[[2]] - x[[1]], x[[3]] - x[[2]], x[[4]] - x[[3]], x[[5]] - x[[4]]))
        }
      )

      # Now, calculate the temperature change within each layer. Then the lapse rate can be calculated from
      # simple raster math
      layer_temp_change <- calc(
        first_levels_temperature,
        fun = function (x) {
          return(c(x[[2]] - x[[1]], x[[3]] - x[[2]], x[[4]] - x[[3]], x[[5]] - x[[4]]))
        }
      )

      # Calculate the mean environmental lapse rate within each layer, in degrees per meter. Note that by
      # meteorological definition, positive lapse rates represent cooling with increased altitude
      layer_lapse_rate <- -1 * layer_temp_change / layer_height

      # Extract the temperature data and create a tibble with the fips information attached.
      layer_lapse_rate_data <- layer_lapse_rate %>% exact_extract(counties, 'mean') %>%
        bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value)) %>%
        mutate(date = ymd(paste(year, month, day, sep = '-'))) %>% rename(layer_1_lapse = mean.layer.1,
                                                                          layer_2_lapse = mean.layer.2,
                                                                          layer_3_lapse = mean.layer.3,
                                                                          layer_4_lapse = mean.layer.4)

      layer_height_data <- layer_height %>% exact_extract(counties, 'mean') %>%
        bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value)) %>%
        mutate(date = ymd(paste(year, month, day, sep = '-'))) %>% rename(layer_1_thickness = mean.layer.1,
                                                                          layer_2_thickness = mean.layer.2,
                                                                          layer_3_thickness = mean.layer.3,
                                                                          layer_4_thickness = mean.layer.4)

      # Merge the data together into a single tibble, then add to data frame
      inversion_data <- layer_lapse_rate_data %>% left_join(layer_height_data)
      year_data %<>% bind_rows(inversion_data)
    }
  }

  # Now get the planetary boundary layer height data for the year and save it with the convective stability data
  system(paste('wget -nc', paste0(boundary_height_dir, boundary_height_file(year))))
  boundary_height_brick <- brick(boundary_height_file(year)) # tempdir() already set as working directory
  boundary_height_data <- boundary_height_brick %>% exact_extract(counties, 'mean') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))
  boundary_height_data %<>%
    pivot_longer(
      starts_with('mean.X'),
      names_to = 'date',
      names_prefix = 'mean.X',
      values_to = 'pbl_height'
    ) %>% mutate(date = date %>% ymd())

  year_data %<>% left_join(boundary_height_data)

  # Write the csv for the year
  write_csv(year_data, file.path(output_dir, output_file(year,'convective_stability_2')))
}
