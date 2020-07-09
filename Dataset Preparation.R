# Title     : Dataset Preparation
# Objective : Prepare my main dataset for analysis
# Created by: Matthew Chambers
# Created on: 7/8/2020

# Load the necessary packages. Tidyverse is for general data manipulation, magrittr supplies the pipe operators
# such as %>% and %<>%, and lubridate provides tools for working intelligently with dates.
library(magrittr)
library(tidyverse)
library(lubridate)

# Define input parameters for the different sources of data. Note that in the column type strings,
# either _ or - omits the column. Alternating makes counting easier.
osha_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/OSHA data scraping'
osha_file <- function (year) return(paste0('osha_fatality_catastrophe_incidents_', year, '.csv'))
osha_input_column_types <- '_-_ic_-_-_-ccc-_-c_-_-_-_-_-_-_'

qcew_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/QCEW'
qcew_file <- function (year) return(paste0(year, '.q1-q4.singlefile.csv'))
qcew_input_column_types <- 'iiciiii_iiii_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-'

inversion_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
inversion_file <- function (year) return(paste0('inversions_', year, '.csv'))
inversion_input_column_types <- 'iD_-_d'

# Define output parameters for the cleaned data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models'
output_file <- function (type = NULL) return(paste0('construction_accidents_', type, '.csv'))

# Define functions to read in the different types of files, incorporating whatever tweaks need to be made
# to read in cleanly
read_in_osha <- function (year) {
  X <- read_csv(file.path(osha_data_dir, osha_file(year)), col_types = osha_input_column_types, progress = FALSE) %>%
  # For some reason, reading the date column in as a date doesn't work, so I have to read it in as character and
  # parse it to a date here.
  mutate(event_date = mdy(event_date)) %>% rename(date = event_date) %>%
  mutate(naics = gsub('X', '', naics) %>% as.integer()) %>%
  mutate(sic = gsub('X', '', sic) %>% as.integer())
  return (X)
}

read_in_qcew <- function (year) {
  X <- read_csv(file.path(qcew_data_dir, qcew_file(year)), col_types = qcew_input_column_types, progress = FALSE) %>%
  # Some rows have parsing errors due to fips codes beginning with 'C'. These are statistical areas which overlap
  # counties and must therefore be excluded to avoid double counting. Because of the parsing errors, these have
  # NA in the area_fips column.
  drop_na(area_fips) %>% rename(fips = area_fips)
  return (X)
}

read_in_inversions <- function (year) {
  X <- read_csv(file.path(inversion_data_dir, inversion_file(year)),
                col_types = inversion_input_column_types, progress = FALSE)
  return (X)
}

osha_test <- read_in_osha(2000)
qcew_test <- read_in_qcew(2000)
inversion_test <- read_in_inversions(2000)
