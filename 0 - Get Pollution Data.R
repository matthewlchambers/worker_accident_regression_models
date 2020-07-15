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
pollution_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Pollution Test Data'
pollution_file <- function (year, month, day) return (paste0('PredictionStep2_PM25_USGrid_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '.rds'))

grid_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Pollution Test Data'
grid_file <- function () return ('USGridSite.rds')

# Define input parameters for the shapefile outlining the regions for which I want summarized data
shapefile_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/County Shapefile'
shapefile_file <- function () return('cb_2016_us_county_20m.shp')

# Define output parameters
output_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Inversion data'
output_file <- function (year) return (paste0('inversions_', year, '.csv'))

# Define beginning and end dates
begin_date <- '2000-01-01' %>% ymd()
end_date <- '2015-12-31' %>% ymd()

# Test day. Remove this after setting up a for loop.
date <- '2000-01-13' %>% ymd()

# Read in the grid, as I'll need to use it repeatedly and don't want to waste time loading it for every single day.
SiteData_1km <- readRDS(file.path(grid_data_dir, grid_file()))
SiteData_point <- SiteData_1km[which(SiteData_1km$Lat < 49.35 & SiteData_1km$Lat > 24.75 & SiteData_1km$Lon > -124.8 & SiteData_1km$Lon < -66.96),]

get_dists_weight <- function () {
  # The following code is pieced together from code provided to me by Dr. Qian Di along with the data files.
  N_Neighbor <- 4

  SiteData_1km <- readRDS(file.path(grid_data_dir, grid_file()))
  SiteData_point <- SiteData_1km[which(SiteData_1km$Lat < 49.35 & SiteData_1km$Lat > 24.75 & SiteData_1km$Lon > -124.8 & SiteData_1km$Lon < -66.96),]
  #SiteData_point <- SiteData_1km
  ## calculate distance matrix
  dist <- get.knnx(SiteData_1km[,c("Lat","Lon")], SiteData_point[,c("Lat","Lon")], k=N_Neighbor)
  # get distance and standardize

  dist_num <- dist$nn.dist
  dist_num[dist_num >2000] <-NA
  dist_num <- dist_num + 1/1000# 1/0 is not allowed

  dist_num[dist_num==0]<-NA
  dist_num <- 1/dist_num
  dist_num_sum <- rowSums(dist_num,na.rm=TRUE)
  dist_num<- dist_num/matrix(rep(dist_num_sum,N_Neighbor),ncol=N_Neighbor,byrow = FALSE)
  dist_num[is.na(dist_num)]<-0

  ## construct sparse matrix
  ColIndex <- dist$nn.index
  RowIndex <- rep(1:nrow(SiteData_point),times = N_Neighbor) # I think this wasn't showing an alert before I commented out some of the library()s
  dim(ColIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
  dim(RowIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
  dim(dist_num) <- c(nrow(SiteData_point)*N_Neighbor,1)

  dists_weight <- sparseMatrix(x = as.numeric(dist_num),i=RowIndex,j = ColIndex,dims=c(nrow(SiteData_point),nrow(SiteData_1km)))
}

dists_weight <- get_dists_weight()

rasterize_day <- function (date) {
  # The following code is pieced together from code provided to me by Dr. Qian Di along with the data files.
  N_Neighbor <- 4

  SiteData_1km <- readRDS(file.path(grid_data_dir, grid_file()))
  SiteData_point <- SiteData_1km[which(SiteData_1km$Lat < 49.35 & SiteData_1km$Lat > 24.75 & SiteData_1km$Lon > -124.8 & SiteData_1km$Lon < -66.96),]
  #SiteData_point <- SiteData_1km
  ## calculate distance matrix
  dist <- get.knnx(SiteData_1km[,c("Lat","Lon")], SiteData_point[,c("Lat","Lon")], k=N_Neighbor)
  # get distance and standardize

  dist_num <- dist$nn.dist
  dist_num[dist_num >2000] <-NA
  dist_num <- dist_num + 1/1000# 1/0 is not allowed

  dist_num[dist_num==0]<-NA
  dist_num <- 1/dist_num
  dist_num_sum <- rowSums(dist_num,na.rm=TRUE)
  dist_num<- dist_num/matrix(rep(dist_num_sum,N_Neighbor),ncol=N_Neighbor,byrow = FALSE)
  dist_num[is.na(dist_num)]<-0

  ## construct sparse matrix
  ColIndex <- dist$nn.index
  RowIndex <- rep(1:nrow(SiteData_point),times = N_Neighbor) # I think this wasn't showing an alert before I commented out some of the library()s
  dim(ColIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
  dim(RowIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
  dim(dist_num) <- c(nrow(SiteData_point)*N_Neighbor,1)

  dists_weight <- sparseMatrix(x = as.numeric(dist_num),i=RowIndex,j = ColIndex,dims=c(nrow(SiteData_point),nrow(SiteData_1km)))
  Temp<-SiteData_point
  #Temp$Value <- 0
  TempData <- t(readRDS(file.path(pollution_data_dir, pollution_file(year(date), month(date), day(date)))))
  Temp$Value <- (dists_weight%*%TempData)@x # I might be able to use lapply() to spread this matrix mult across
  # a list of pollution datas from all days in a year, so I can create a matrix with columns for each day, which will
  # let me make a raster brick using rasterize, which will hopefully speed up the process (it seems much faster
  # with the weather data, where each year was a raster brick).
  spg <- Temp
  coordinates(spg) <- ~ Lon + Lat
  e <- extent(spg)
  r <- raster(ext=extent(spg),ncol=500,nrow=500)
  crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
  x <- rasterize(Temp[,c("Lon","Lat")], r, Temp$Value, fun=mean)

  return (x)
}

rasterize_year <- function (year) {
  # The following code is pieced together from code provided to me by Dr. Qian Di along with the data files.
  #SiteData_point <- SiteData_1km
  Temp<-SiteData_point
  #Temp$Value <- 0
  TempData <- t(readRDS(file.path(pollution_data_dir, pollution_file(year(date), month(date), day(date)))))
  Temp$Value <- (dists_weight%*%TempData)@x # I might be able to use lapply() to spread this matrix mult across
  # a list of pollution datas from all days in a year, so I can create a matrix with columns for each day, which will
  # let me make a raster brick using rasterize, which will hopefully speed up the process (it seems much faster
  # with the weather data, where each year was a raster brick).
  spg <- Temp
  coordinates(spg) <- ~ Lon + Lat
  e <- extent(spg)
  r <- raster(ext=extent(spg),ncol=500,nrow=500)
  crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
  x <- rasterize(Temp[,c("Lon","Lat")], r, Temp$Value, fun=mean)

  return (x)
}

process_day <- function (date) {
  # Get the pollution data raster
  pollution_raster <- rasterize_day(date)
  names(pollution_raster) <- 'pm25'

  # Open the shapefile, so I can extract daily means of the weather variables by county.
  counties <- st_read(file.path(shapefile_dir, shapefile_file()))

  # Use exactextractr to get county level mean pollution
  county_means <- pollution_raster %>% exact_extract(counties, 'mean') %>% as_tibble_col(column_name = 'pm25') %>%
    bind_cols(counties$fips %>% as.integer() %>% as_tibble() %>% rename(fips = value))

  return (county_means)
}

test <- process_day(date)
