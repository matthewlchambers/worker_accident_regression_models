# Title     : Prepare inversion data from Bing Yang Tan
# Objective : see above
# Created by: Matthew Chambers
# Created on: 7/3/2020

# Load the necessary packages. Tidyverse is for general data manipulation, magrittr supplies the pipe operators
# such as %>% and %<>%, lubridate provides tools for working intelligently with dates, and haven allows
# me to work with Stata format .dta files.
library(magrittr)
library(lubridate)
library(tidyverse)
library(haven)

# Define input parameters
input_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data/Stata files from Bing Yang'
input_file <- function (year, month) return (paste0('inversions_', year, str_pad(month, 2, pad = '0'), '.dta'))

# Define output parameters
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
output_file <- function (year) return (paste0('inversions_', year, '.csv'))

# Save the number of seconds from 1-1-1800 to 1-1-1970 as a constant, for converting date-times later.
conversion_constant <- (ymd_hms('1970-01-01 00:00:00') - ymd_hms('1800-01-01 00:00:00')) %>%
  as.double.difftime(units = 'secs')

# Set the year range
begin_year <- 2000
end_year <- 2016

# Loop over the years to create a data file for each one.
for (year in begin_year:end_year) {
  year_data <- NULL

  # Loop over the months since the data files from Bing Yang Tan include a separate file for each month.
  for (month in 1:12) {
    month_data <- read_dta(file.path(input_dir, input_file(year, month))) %>%
      # Convert the time (which begins as hours since 1-1-1800) into a date-time.
      mutate(ti = (ti * 3600 - conversion_constant) %>% as_datetime()) %>%
      mutate(day_of_month = day(ti)) %>%
      # Create a single date variable.
      mutate(date = make_date(year, month, day_of_month)) %>%
      # Create a single variable for the county's FIPS code.
      mutate(fips = statefip * 1000 + countyfip) %>%
      # Drop the variables I no longer need.
      select(!c('statefip', 'countyfip', 'ti', 'year', 'month', 'day_of_month')) %>%
      # Create a variable to give the fraction of the county covered by a thermal inversion.
      mutate(inversion_coverage = inversions_fraction / total_weight) %>%
      # Group and summarize to get the average amount of thermal inversion coverage over the course of the day.
      group_by(fips, date) %>% summarize_all(mean) %>% ungroup()

    # Bind the different months together to produce a single data file for the year.
    if (is.null(year_data)) {
      year_data <- month_data
    } else {
      year_data %<>%
        bind_rows(month_data)
    }
  }

  # Write the yearly data file to disk.
  write.csv(year_data, file.path(output_dir, output_file(year)), row.names = FALSE)
}
