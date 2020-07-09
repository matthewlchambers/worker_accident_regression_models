# Title     : Dataset Preparation
# Objective : Prepare my main dataset for analysis
# Created by: Matthew Chambers
# Created on: 7/8/2020

# Load the necessary packages. Tidyverse is for general data manipulation, magrittr supplies the pipe operators
# such as %>% and %<>%, and lubridate provides tools for working intelligently with dates.
library(magrittr)
library(tidyverse)
library(lubridate)

# Define the years to be covered. Note that OSHA accidents don't consistently have the establishment's
# NAICS code until 2003.
begin_year <- 2003
end_year <- 2016

# Define input parameters for the different sources of data.
osha_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/OSHA data scraping'
osha_file <- function (year) return(paste0('osha_fatality_catastrophe_incidents_', year, '.csv'))
# I have a file with geocoded addresses to label the OSHA incidents with.
geocode_file <- 'geocoded_addresses.csv'

qcew_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/QCEW'
qcew_file <- function (year) return(paste0(year, '.q1-q4.singlefile.csv'))

inversion_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
inversion_file <- function (year) return(paste0('inversions_', year, '.csv'))

# Define output parameters for the cleaned data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models'
output_file <- function (type = NULL) return(paste0('construction_accidents_', type, '.csv'))

# Define functions to read in the different types of files, incorporating whatever tweaks need to be made
# to read in cleanly. Note that in the column type strings, either _ or - omits the column, but alternating
# makes counting easier.
read_in_osha <- function (year) {
  osha_input_column_types <- '_-_ic_-_-_-ccc-_-c_-_-_-_-_-_-_'
  X <- read_csv(file.path(osha_data_dir, osha_file(year)), col_types = osha_input_column_types, progress = FALSE) %>%
  # For some reason, reading the date column in as a date doesn't work, so I have to read it in as character and
  # parse it to a date here.
  mutate(event_date = mdy(event_date)) %>% rename(date = event_date) %>%
  mutate(naics = gsub('X', '', naics) %>% as.integer()) %>%
  mutate(sic = gsub('X', '', sic) %>% as.integer())
  return (X)
}

read_in_qcew <- function (year) {
  qcew_input_column_types <- 'iici_ii_iiii_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-'
  X <- read_csv(file.path(qcew_data_dir, qcew_file(year)), col_types = qcew_input_column_types, progress = FALSE) %>%
  # Some rows have parsing errors due to fips codes beginning with 'C'. These are statistical areas which overlap
  # counties and must therefore be excluded to avoid double counting. Because of the parsing errors, these have
  # NA in the area_fips column.
  drop_na(area_fips) %>% rename(fips = area_fips)
  return (X)
}

read_in_inversions <- function (year) {
  inversion_input_column_types <- 'iD_-_d'
  X <- read_csv(file.path(inversion_data_dir, inversion_file(year)),
                col_types = inversion_input_column_types, progress = FALSE)
  return (X)
}

read_in_geocodes <- function () {
  geocode_input_column_types <- '_-_-_-_-_-c_-i'
  X <- read_csv(file.path(osha_data_dir, geocode_file),
                col_types = geocode_input_column_types, progress = FALSE)
  return (X)
}

process_one_year <- function (year) {
  qcew_data <- read_in_qcew(year)
  osha_data <- read_in_osha(year)
  inversion_data <- read_in_inversions(year)

  # I only want to consider privately owned establishments, as OSHA's applicability to government workplaces
  # is inconsistent. Fortunately, both the QCEW and OSHA data include information on firm ownership. So I
  # filter both datasets to only include privately owned establishments.
  qcew_data %<>%
    filter(own_code == 5) %>% select(-own_code)
  osha_data %<>%
    filter(ownership == 'Private') %>% select(-ownership)

  # First I work with the QCEW data
  qcew_data %<>%
    # Per https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.htm, aggregation level 74 represents data
    # aggregated by county, sector (2 digit NAICS), and ownership groups. Aggregation level 71 contains county level
    # total employment, which I also need. NAICS code 23 represents the construction industry, which I am
    # currently focusing on. If I come back and decide to expand that, I need to not drop the industry code.
    filter((agglvl_code == 74 & industry_code == 23) | agglvl_code == 71) %>% select(-industry_code) %>%
    # I rename the monthly employment level variables for ease of working with the pivot_longer function.
    rename(emp_m1 = month1_emplvl) %>% rename(emp_m2 = month2_emplvl) %>% rename(emp_m3 = month3_emplvl) %>%
    # I reshape the data into a monthly, rather than quarterly, form.
    pivot_longer(
      cols = starts_with('emp_m'),
      names_to = 'month_in_qtr',
      names_prefix = 'emp_m',
      values_to = 'employment'
    ) %>% mutate(month_in_qtr = month_in_qtr %>% as.integer()) %>%  # month_in_qtr should be an integer for math
    # I generate a month variable from the quarter and month-in-quarter variables (qtr and month_in_qtr).
    mutate(qtr = qtr - 1) %>% mutate(month = (month_in_qtr + qtr * 3) %>% as.integer()) %>%
    # I don't really need to keep the quarter and month-in-quarter variables around any longer.
    select(-c('qtr', 'month_in_qtr')) %>%
    # I now create a new variables for total employment using aggregation level 71, then group observations
    # to fill that value in for all aggregation level 74 (specific industry) observations
    mutate(total_employment = ifelse(agglvl_code == 71, employment, NA)) %>%
    group_by(fips, year, month) %>% fill(total_employment, .direction = 'downup') %>% ungroup() %>%
    filter(agglvl_code == 74) %>% select(-agglvl_code)

  # Now I work with the OSHA data, and the first thing I need to do is load the file of geocoded addresses
  # so I can identify the county in whcih each accident occurred.
  geocoded_addresses <- read_in_geocodes()
  osha_data %<>%
    # First, to save time and memory, I filter to construction only and drop the NAICS code
    mutate(naics_2 = floor(naics / 10000) %>% as.integer()) %>%
    filter(naics_2 == 23) %>% select(-c('naics', 'naics_2', 'sic')) %>%
    # Now I merge in the FIPS code from my geocoded address file, and drop any observations which could
    # not be geocoded.
    left_join(geocoded_addresses) %>% drop_na(fips) %>% select(-est_address)

  # I now assemble my main data file. The inversion data for a given year are my starting point, since they
  # constitute a nice balanced panel.
  mydata <- inversion_data %>%
    # First I merge in the OSHA accident data.
    left_join(osha_data) %>% mutate(num_injured = replace_na(num_injured, 0) %>% as.integer()) %>%
    # Then I merge in the QCEW employment data, after creating year and month variables to merge on. I then
    # drop the year and month variables since I no longer need them.
    mutate(year = year(date)) %>% mutate(month = month(date)) %>%
    left_join(qcew_data) %>% select(-c('year', 'month')) %>%
    # I generate a weekday factor variables, since weekday effects are likely important.
    mutate(weekday = wday(date) %>% as_factor())

  return(mydata)
}

all_data <- NULL

for (i in begin_year:end_year) {
  year_data <- process_one_year(i)

  if (is.null(all_data)) {
    all_data <- year_data
  } else {
    all_data %<>%
      bind_rows(year_data)
  }
}

write.csv(all_data, file.path(output_dir, output_file(paste0(begin_year, '_to_', end_year))), row.names = FALSE)
