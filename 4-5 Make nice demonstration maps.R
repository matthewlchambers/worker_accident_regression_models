# Title     : Maps
# Objective : Make nice maps to inlcue in my finished paper.
# Created by: Matthew Chambers
# Created on: 9/14/2020

# Load the necesary packages. Raster provides some excellent tools for working with geodata. Tidyverse is for
# general data manipulation, magrittr supplies the pipe operators such as %>% and %<>%, and lubridate provides
# tools for working intelligently with dates. rgdal is for reading shapefiles, and ggplot2 is for mapping them.
# Broom helps clean up data.
library(raster)
library(tidyverse)
library(magrittr)
library(lubridate)
library(rgdal)
library(ggplot2)
library(maps)

# Define input parameters for the shapefile outlining the regions for which I want summarized data
shapefile_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/County Shapefile'
shapefile_file <- function () return ('cb_2016_us_county_20m.shp')

# Define input parameters for the data file we will be summarizing to create the maps
accidents_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models'
accidents_file <- function () return('construction_accidents_2003_to_2015.csv')

map_info <- map_data('county')