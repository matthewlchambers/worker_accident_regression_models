# Title     : Further Summary Stats
# Objective : Prepare further summary stats, including percentage of events that
#             don't fit the definition of catastrophes given by OSHA.
# Created by: Matthew Chambers
# Created on: 9/10/2020

# Load the necessary packages. Tidyverse is for general data manipulation, magrittr supplies the pipe operators
# such as %>% and %<>%, and lubridate provides tools for working intelligently with dates.
library(magrittr)
library(tidyverse)
library(lubridate)
library(janitor)

# Define the years to be covered. Note that OSHA accidents don't consistently have the establishment's
# NAICS code until 2003.
begin_year <- 2003
end_year <- 2015

# Define input parameters for the different sources of data.
osha_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/OSHA data scraping'
osha_file <- function (year) return (paste0('osha_fatality_catastrophe_incidents_', year, '.csv'))
# I have a file with geocoded addresses to label the OSHA incidents with.
geocode_file <- 'geocoded_addresses.csv'

# Define output parameters for the cleaned data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models'
# Uncomment the line below to send output to a directory set up for nighttime inversion data (as a robustness check)
#output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models/Nighttime Inversions'
output_file <- function () return(paste0('construction_accidents_fatal_or_not.csv'))

# Define functions to read in the different types of files, incorporating whatever tweaks need to be made
# to read in cleanly. Note that in the column type strings, either _ or - omits the column, but alternating
# makes counting easier.
read_in_osha <- function (year) {
  osha_input_column_types <- '_-_ic_-_-_-ccc-_-c_-_-_-_-_-c-_'
  X <- read_csv(file.path(osha_data_dir, osha_file(year)), col_types = osha_input_column_types, progress = FALSE) %>%
  # For some reason, reading the date column in as a date doesn't work, so I have to read it in as character and
  # parse it to a date here.
    mutate(event_date = mdy(event_date)) %>% rename(date = event_date) %>%
    mutate(naics = gsub('X', '', naics) %>% as.integer()) %>%
    mutate(sic = gsub('X', '', sic) %>% as.integer()) %>%
    mutate(fatality = degree == 'Fatality') %>% mutate(hosp_inj = degree == 'Hospitalized injury') %>%
    group_by(date, est_address) %>% mutate(fatality = max(fatality)) %>%
    mutate(hosp_inj = max(hosp_inj)) %>% ungroup() %>%
    # Drop duplicate observations
    distinct(date, est_address, .keep_all = TRUE) %>%
    # This is the point of this file, to identify which incidents match different possible catastrophe definitions.
    mutate(non_fatal_less_than_three = !fatality & num_injured < 3) %>%
    mutate(non_fatal_non_hosp = !fatality & !hosp_inj)
  return (X)
}

read_in_geocodes <- function () {
  geocode_input_column_types <- '_-_-_-_-_-c_-i'
  X <- read_csv(file.path(osha_data_dir, geocode_file),
                col_types = geocode_input_column_types, progress = FALSE)
  return (X)
}

process_osha_year <- function (year) {
  osha_data <- read_in_osha(year)
  geocoded_addresses <- read_in_geocodes()

  osha_data %<>%
    # I only want to consider privately owned establishments, as OSHA's applicability to government workplaces
    # is inconsistent.
    filter(ownership == 'Private') %>% select(-ownership) %>%
    # First, to save time and memory, I filter to construction only and drop the NAICS code
    mutate(naics_2 = floor(naics / 10000) %>% as.integer()) %>%
    filter(naics_2 == 23) %>% select(-c('naics', 'naics_2', 'sic')) %>%
    # Now I merge in the FIPS code from my geocoded address file, and drop any observations which could
    # not be geocoded.
    left_join(geocoded_addresses) %>% drop_na(fips) %>% select(-est_address) %>%
    # I group by county and date, then get the sum of persons injured (in case of multiple accidents), as well as
    # the number of incidents.
    group_by(fips, date) %>% summarize(num_injured = num_injured %>% sum(), num_accidents = n())

  return (osha_data)
}


