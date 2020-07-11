# Title     : Get Weather Data
# Objective : Download and extract some weather data as covariates for my analysis
# Created by: Matthew Chambers, with thanks to Bing Yang Tan for
# Created on: 7/10/2020

# Load the necesary packages. Raster provides some excellent tools for working with geodata, ncdf4 provides tools for
# working with the .nc4 files provided by NOAA, and exactextractr is a tool for getting summary statistics from a
# raster image by region using a shapefile. Tidyverse is for general data manipulation, magrittr supplies the
# pipe operators such as %>% and %<>%, and lubridate provides tools for working intelligently with dates.
library(raster)
library(ncdf4)
library(exactextractr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(sf)

# Define the years to be covered
begin_year <- 2003
end_year <- 2016

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
output_file <- function (type = NULL) return(paste0(type, '_data', '.csv'))

# Ensure the output directory exists, and ask to create it or get a new directory name if it doesn't.
while (TRUE) {
  if (dir.exists(output_dir)) break else {
    create_dir <- readline(prompt = paste('Output directory', output_dir, 'does not exist. Create it (Y/N)? '))
    if (create_dir == 'Y') dir.create(output_dir) else {
      output_dir <- readline(prompt = 'Please enter desired output directory, or QUIT to quit: ')
      if (output_dir == 'QUIT') quit(status = 42)
    }
  }
}

# For testing purposes, before I build the for loop to contain all this:
year <- 2003

# Download the files for the year in question to the temporary directory.
setwd(tempdir())
system(paste('wget -nc', paste0(precipitation_dir, precipitation_file(year))))
system(paste('wget -nc', paste0(temperature_dir, temperature_file(year))))

# Open the files so I can extract data.
precipitation_brick <- brick(precipitation_file(year)) # tempdir() already set as working directory
temperature_brick <- brick(temperature_file(year)) # tempdir() already set as working directory

# Open the shapefile, so I can extract daily means of the weather variables.
counties <- st_read(file.path(shapefile_dir, shapefile_file()))

# I don't think the line below is doing what I want.
test <- exact_extract(temperature_brick, counties)
