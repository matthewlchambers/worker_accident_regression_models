# Title     : Dataset Preparation
# Objective : Prepare my main dataset for analysis
# Created by: Matthew Chambers
# Created on: 7/8/2020

# Load the necessary packages. Tidyverse is for general data manipulation, magrittr supplies the pipe operators
# such as %>% and %<>%, lubridate provides tools for working intelligently with dates, and haven works with Stata files.
library(magrittr)
library(tidyverse)
library(lubridate)
library(haven)

# Define the years to be covered. Note that OSHA accidents don't consistently have the establishment's
# NAICS code until 2003.
begin_year <- 2003
end_year <- 2015

# Define input parameters for the different sources of data.

# First is the OSHA data for workplace accidents
osha_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/OSHA data scraping'
osha_file <- function (year) return (paste0('osha_fatality_catastrophe_incidents_', year, '.csv'))
# I have a file with geocoded addresses to label the OSHA incidents with.
geocode_file <- 'geocoded_addresses.csv'

# This is the NHTSA data for traffic accidents
nhtsa_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/NHTSA'
nhtsa_file <- function (year) return (paste0(year, '_ACCIDENT.CSV'))

# QCEW data are to identify the number of employees in a county by industry by month so I can control for it
qcew_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/QCEW'
qcew_file <- function (year) return (paste0(year, '.q1-q4.singlefile.csv'))

# Population estimates (by year) from the Census Bureau
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage
population_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Population Density'
population_estimates_file <- function () return ('annual_county_population_estimates.dta')

# These are the data from Bing Yang Tan, with some modification by myself. I use these as an instrument in
# early versions of the analysis
inversion_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
# Uncomment the line below (and one under output_dir, below, after running script number 11 to create a data file
# that uses nighttime inversions only (as a robustness check)
#inversion_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data/Nighttime Inversions'
inversion_file <- function (year) return (paste0('inversions_', year, '.csv'))

# There are multiple files here because the weather data were collected in multiple passes through NARR
weather_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Weather Data'
temperature_precipitation_file <- function (year) return (paste0(year, '_mean_temp_mean_precip_data.csv'))
wind_humidity_file <- function (year) return (paste0(year, '_wind_hum_data.csv'))
convective_stability_file <- function (year) return (paste0(year, '_convective_stability_data.csv'))

# These are the PM 2.5 data from Di, et al. They are estimated on a 1 km x 1 km grid over the contiguous US.
# I use these instead of EPA's ground based monitor data because
pollution_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Pollution Data'
pollution_file <- function (year) return (paste0('PM25_prediction_', year, '.csv'))

# Define output parameters for the cleaned data file(s) to be saved.
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models/Fatality Only'
# Uncomment the line below to send output to a directory set up for nighttime inversion data (as a robustness check)
#output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Data for Regression Models/Nighttime Inversions'
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
    mutate(sic = gsub('X', '', sic) %>% as.integer()) %>%
    # Drop duplicate observations
    distinct(date, est_address, .keep_all = TRUE)
  return (X)
}

read_in_osha_fatalities <- function (year) {
  osha_input_column_types <- '_-_ic_-_-_-ccc-_-c_-_-_-_-_-c-_'
  X <- read_csv(file.path(osha_data_dir, osha_file(year)), col_types = osha_input_column_types, progress = FALSE) %>%
  # For some reason, reading the date column in as a date doesn't work, so I have to read it in as character and
  # parse it to a date here.
    filter(degree == 'Fatality') %>% select(-degree) %>%
    mutate(event_date = mdy(event_date)) %>% rename(date = event_date) %>%
    mutate(naics = gsub('X', '', naics) %>% as.integer()) %>%
    mutate(sic = gsub('X', '', sic) %>% as.integer()) %>%
    # Drop duplicate observations
    distinct(date, est_address, .keep_all = TRUE)
  return (X)
}

read_in_nhtsa <- function (year) {
  X <- read_csv(file.path(nhtsa_data_dir, nhtsa_file(year)), progress = FALSE) %>% rename_all(tolower) %>%
    mutate(date = ymd(paste(year, month, day, sep = '-'))) %>%
    mutate(fips = (state * 1000 + county) %>% as.integer()) %>%
    mutate(accident_occurred = 1 %>% as.integer()) %>%
    select(c(fips, date, accident_occurred))
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

read_in_population_estimates <- function (y) {
  X <- read_dta(file.path(population_data_dir, population_estimates_file())) %>% filter(year == y) %>%
    mutate(year = year %>% as.integer()) %>%
    mutate(fips = fips %>% as.integer()) %>%
    mutate(population = population %>% as.integer())
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

read_in_weather <- function (year) {
  temperature_precipitation_input_column_types <- 'iDdd'
  temperature_precipitation <- read_csv(file.path(weather_data_dir, temperature_precipitation_file(year)),
                                        col_types = temperature_precipitation_input_column_types, progress = FALSE)
  wind_humidity_input_column_types <- 'iDddd'
  wind_humidity <- read_csv(file.path(weather_data_dir, wind_humidity_file(year)),
                            col_types = wind_humidity_input_column_types, progress = FALSE)
  convective_stability_input_column_types <- 'ddddiDd'
  convective_stability <- read_csv(file.path(weather_data_dir, convective_stability_file(year)),
                                   col_types = convective_stability_input_column_types, progress = FALSE) %>%
    rename(layer_1_lapse = mean.layer.1,
           layer_2_lapse = mean.layer.2,
           layer_3_lapse = mean.layer.3,
           layer_4_lapse = mean.layer.4)
  X <- temperature_precipitation %>% left_join(wind_humidity) %>% left_join(convective_stability)
  return (X)
}

read_in_pollution <- function (year) {
  pollution_input_column_types <- 'iDd'
  X <- read_csv(file.path(pollution_data_dir, pollution_file(year)),
                col_types = pollution_input_column_types, progress = FALSE)
  return (X)
}

process_qcew_year <- function (year) {
  qcew_data <- read_in_qcew(year)

  qcew_data %<>%
    # I only want to consider privately owned establishments, as OSHA's applicability to government workplaces
    # is inconsistent.
    filter(own_code == 5) %>% select(-own_code) %>%
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
    group_by(fips, month) %>% fill(total_employment, .direction = 'downup') %>% ungroup() %>%
    filter(agglvl_code == 74) %>% select(-agglvl_code)

  return (qcew_data)
}

process_osha_year <- function (year) {
  osha_data <- read_in_osha(year)
  geocoded_addresses <- read_in_geocodes()

  osha_data %<>%
    # I only want to consider privately owned establishments, as OSHA's applicability to government workplaces
    # is inconsistent.
    filter(ownership == 'Private') %>% select(-ownership) %>%
    # First, to save time and memory, I filter to the chosen industry only and drop the NAICS code
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

process_osha_fatalities_year <- function (year) {
  osha_data <- read_in_osha_fatalities(year)
  geocoded_addresses <- read_in_geocodes()

  osha_data %<>%
    # I only want to consider privately owned establishments, as OSHA's applicability to government workplaces
    # is inconsistent.
    filter(ownership == 'Private') %>% select(-ownership) %>%
    # First, to save time and memory, I filter to the chosen industry only and drop the NAICS code
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

process_one_year <- function (year) {
  # Read in all the data I need for this year.
  qcew_data <- process_qcew_year(year)
  osha_data <- process_osha_year(year)
  inversion_data <- read_in_inversions(year)
  weather_data <- read_in_weather(year)
  pollution_data <- read_in_pollution(year)

  # I assemble my main data file. The inversion data for a given year are my starting point, since they
  # constitute a nice balanced panel.
  mydata <- inversion_data %>%
    # First I merge in the weather data
    left_join(weather_data) %>%
    # Then I merge in the pollution data
    left_join(pollution_data) %>%
    # Then I merge in the OSHA accident data.
    left_join(osha_data) %>%
    # Replace NAs (days unrepresented in the OSHA data) with 0s, as they represent observations with no accident.
    mutate(num_injured = num_injured %>% replace_na(0) %>% as.integer()) %>%
    mutate(num_accidents = num_accidents %>% replace_na(0) %>% as.integer()) %>%
    # Then I merge in the QCEW employment data, after creating year and month variables to merge on. I then
    # drop the year and month variables since I no longer need them.
    mutate(year = year(date)) %>% mutate(month = month(date)) %>%
    left_join(qcew_data) %>% select(-c('year', 'month')) %>%
    # I generate a weekday factor variables, since weekday effects are likely important.
    mutate(weekday = wday(date) %>% as_factor())

  return(mydata)
}

process_one_fatalities_year <- function (year) {
  # Read in all the data I need for this year.
  qcew_data <- process_qcew_year(year)
  osha_data <- process_osha_fatalities_year(year)
  inversion_data <- read_in_inversions(year)
  weather_data <- read_in_weather(year)
  pollution_data <- read_in_pollution(year)

  # I assemble my main data file. The inversion data for a given year are my starting point, since they
  # constitute a nice balanced panel.
  mydata <- inversion_data %>%
    # First I merge in the weather data
    left_join(weather_data) %>%
    # Then I merge in the pollution data
    left_join(pollution_data) %>%
    # Then I merge in the OSHA accident data.
    left_join(osha_data) %>%
    # Replace NAs (days unrepresented in the OSHA data) with 0s, as they represent observations with no accident.
    mutate(num_injured = num_injured %>% replace_na(0) %>% as.integer()) %>%
    mutate(num_accidents = num_accidents %>% replace_na(0) %>% as.integer()) %>%
    # Then I merge in the QCEW employment data, after creating year and month variables to merge on. I then
    # drop the year and month variables since I no longer need them.
    mutate(year = year(date)) %>% mutate(month = month(date)) %>%
    left_join(qcew_data) %>% select(-c('year', 'month')) %>%
    # I generate a weekday factor variables, since weekday effects are likely important.
    mutate(weekday = wday(date) %>% as_factor())

  return(mydata)
}

process_one_year_traffic <- function (year) {
  # Read in all the data I need for this year.
  nhtsa_data <- read_in_nhtsa(year)
  population_estimates_data <- read_in_population_estimates(year)
  inversion_data <- read_in_inversions(year)
  weather_data <- read_in_weather(year)
  pollution_data <- read_in_pollution(year)

  # I assemble my main data file. The inversion data for a given year are my starting point, since they
  # constitute a nice balanced panel.
  mydata <- inversion_data %>%
    # First I merge in the weather data
    left_join(weather_data) %>%
    # Then I merge in the pollution data
    left_join(pollution_data) %>%
    # Then I merge in the OSHA accident data.
    left_join(nhtsa_data) %>%
    # Replace NAs (days unrepresented in the NHTSA data) with 0s, as they represent observations with no accident.
    mutate(accident_occurred = accident_occurred %>% replace_na(0) %>% as.integer()) %>%
    # Then I merge in the population estimate data
    left_join(population_estimates_data) %>%
    # I generate weekday factor variables, since weekday effects are likely important.
    mutate(weekday = wday(date) %>% as_factor())

  return(mydata)
}

all_data <- NULL

for (i in begin_year:end_year) {
  # Make sure to change the output directory if you mess around with these lines
  #year_data <- process_one_year(i)
  year_data <- process_one_fatalities_year(i)
  #year_data <- process_one_year_traffic(i)

  if (is.null(all_data)) {
    all_data <- year_data
  } else {
    all_data %<>%
      bind_rows(year_data)
  }
}

write_csv(all_data, file.path(output_dir, output_file(paste0(begin_year, '_to_', end_year))))
