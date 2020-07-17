# Title     : Get Pollution Data
# Objective : Process .rds files from Dr. Qian Di, with daily PM 2.5 levels across the U.S., to get daily county means.
# Created by: Matthew Chambers
# Created on: 7/14/2020

# Load the necesary packages. Raster provides some excellent tools for working with geodata and exactextractr
# is a tool for getting summary statistics from a raster image by region using a shapefile. Tidyverse is for
# general data manipulation, magrittr supplies the pipe operators such as %>% and %<>%, and lubridate provides
# tools for working intelligently with dates.
library(raster)
library(exactextractr)
library(tidyverse)
library(magrittr)
library(sf)
library(lubridate)
library(FNN)
library(Matrix)
library(sp)

# Define input parameters
pollution_data_dir <- 'D:/Pollution File for Matthew/Extracted'
pollution_file <- function (year, month, day) return (paste0('PredictionStep2_PM25_USGrid_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '.rds'))

grid_data_dir <- 'D:/Pollution File for Matthew/Extracted'
grid_file <- function () return ('USGridSite.rds')

# Define input parameters for the shapefile outlining the regions for which I want summarized data
shapefile_dir <- 'D:/Pollution File for Matthew/County Shapefile'
shapefile_file <- function () return('cb_2016_us_county_20m.shp')

# Define output parameters
output_dir <- 'D:/Pollution File for Matthew/Yearly Data'
output_file <- function (year) return (paste0('PM25_prediction_', year, '.csv'))

# Define beginning and end years
begin_year <- '2000'
end_year <- '2015'

# Function to read in a single days worth of PM2.5 data
read_in_day <- function (date) {
  return (file.path(pollution_data_dir, pollution_file(year(date), month(date), day(date))) %>% readRDS() %>% t())
}

# Read in the grid, as I'll need to use it repeatedly and don't want to waste time loading it for every single day.
grid_points <- readRDS(file.path(grid_data_dir, grid_file()))

# Identify the subset of grid points that are located within my region of interest, the contiguous US
region_of_interest <- grid_points[which(grid_points$Lat < 49.35 & grid_points$Lat > 24.75 & grid_points$Lon > -124.8 & grid_points$Lon < -66.96),]

# This function exists to contain code provided by Dr. Di that I don't fully understand yet.
get_dists_weight <- function () {
  # How many nearest neighbors should be found for each grid point?
  N_Neighbor <- 4

  # calculate distance matrix
  dist <- get.knnx(grid_points[, c("Lat", "Lon")], region_of_interest[, c("Lat", "Lon")], k=N_Neighbor)

  # get distance and standardize
  dist_num <- dist$nn.dist
  dist_num[dist_num >2000] <- NA
  dist_num <- dist_num + 1/1000 # 1/0 is not allowed

  dist_num[dist_num==0] <- NA
  dist_num <- 1/dist_num
  dist_num_sum <- rowSums(dist_num,na.rm=TRUE)
  dist_num <- dist_num/matrix(rep(dist_num_sum,N_Neighbor),ncol=N_Neighbor,byrow = FALSE)
  dist_num[is.na(dist_num)] <- 0

  ## construct sparse matrix
  ColIndex <- dist$nn.index
  RowIndex <- rep(1:nrow(region_of_interest), times = N_Neighbor)
  dim(ColIndex) <- c(nrow(region_of_interest)*N_Neighbor, 1)
  dim(RowIndex) <- c(nrow(region_of_interest)*N_Neighbor, 1)
  dim(dist_num) <- c(nrow(region_of_interest)*N_Neighbor, 1)

  dists_weight <- sparseMatrix(x = as.numeric(dist_num),i=RowIndex,j = ColIndex,dims=c(nrow(region_of_interest), nrow(grid_points)))
}

# Again, this is something I only want to have to do once.
dists_weight <- get_dists_weight()

rasterize_range <- function (beg_date, end_date) {
  # The following code is pieced together from code provided to me by Dr. Qian Di along with the data files.
  # I've modified it to create a raster brick from a series of dates, rather than having to rasterize each
  # day individually.

  # This function does exactly what its name implies :-) The dist_weights matrix was generated earlier.
  left_multiply_by_dist_weights <- function (data_matrix) return ((dists_weight %*% data_matrix)@x)

  # For some reason, both the variable named spg and attaching the weighted matrix to Temp as Temp$Values
  # are necessary for the rasterization to work. I'm not sure why, but it would take too long to try and solve.
  Temp<-region_of_interest
  # Make a list of dates within a year so I can open all the files for assembly into a raster brick.
  date_list <- beg_date:end_date %>% as_date() %>% as.list()
  # Read in the list of raw pollution data from Di, et al. for each date in the given range. This corresponds
  # to a list of 'TempData' objects from Dr. Di's original code.
  raw_pollution_matrix <- lapply(date_list, read_in_day)
  # This generates the correctly weighted PM2.5 values for each grid point.
  Temp$Values <- sapply(raw_pollution_matrix, left_multiply_by_dist_weights)
  colnames(Temp$Values) <- date_list %>% lapply(paste)
  spg <- Temp
  coordinates(spg) <- ~ Lon + Lat
  r <- raster(ext=extent(spg),ncol=500,nrow=500)
  crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
  x <- rasterize(Temp[,c("Lon","Lat")], r, Temp$Values, fun=mean)

  return (x)
}

process_year <- function (year) {
  # Open the shapefile, so I can extract daily means of the weather variables by county.
  counties <- st_read(file.path(shapefile_dir, shapefile_file()))
  # I have to split up the year into blocks to work with for memory purposes. First, set the size of those blocks
  date_block_size <- 10 %>% days()
  # Initialize values before starting the loop
  beg_date <- ymd(paste0(year, '0101'))
  year_data <- NULL

  while (beg_date <= ymd(paste0(year, '1231'))) {
    # Set the end date for this block. Don't run past the end of the year, and remember that ranges in R are inclusive.
    end_date <- min(beg_date + date_block_size - days(), ymd(paste0(year, '1231')))

    # Get the pollution data raster
    pollution_raster <- rasterize_range(beg_date, end_date)

    # Use exactextractr to get county level mean pollution
    mean_pollution_data <- pollution_raster %>% exact_extract(counties, 'mean') %>% #as_tibble_col(column_name = 'pm25') %>%
      bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))

    # Reshape the tibble so that each observation corresponds to a county-day, and change the date variable to
    # an R readable date variables.
    mean_pollution_data %<>%
      pivot_longer(
        starts_with('mean.X'),
        names_to = 'date',
        names_prefix = 'mean.X',
        values_to = 'mean_pm25'
      ) %>% mutate(date = date %>% ymd())

    # Bind the different blocks together to produce a single data file for the year.
    if (year_data %>% is.null()) {
      year_data <- mean_pollution_data
    } else {
      year_data %<>%
        bind_rows(mean_pollution_data)
    }

    # Update beg_date
    beg_date <- beg_date + date_block_size
  }

  # After finishing the while loop, write the entire year to a csv.
  write_csv(year_data, file.path(output_dir, output_file(year)))
}

for (year in begin_year:end_year) {
  process_year(year)
}
