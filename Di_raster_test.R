# Title     : TODO
# Objective : TODO
# Created by: Matthew Chambers
# Created on: 7/14/2020
#require(Hmisc)
library(FNN)
library(Matrix)
#library(rgeos)
library(sp)
#library(rgdal)
library(raster)
#library(gstat)
#library(gam)
#library(mgcv)
#library(randomForest)
library(tidyverse)

# Define input parameters
pollution_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Pollution Test Data'
pollution_file <- function (year, month, day) return (paste0('PredictionStep2_PM25_USGrid_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '_',
                                                             year, str_pad(month, 2, pad = '0'),
                                                             str_pad(day, 2, pad = '0'), '.rds'))

grid_data_dir <- 'E:/Research Projects/Worker Accidents and Pollution/Data/Pollution Test Data'
grid_file <- function () return ('USGridSite.rds')

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
TempData <- t(readRDS(file.path(pollution_data_dir, pollution_file(2000, 01, 14))))
Temp$Value <- (dists_weight%*%TempData)@x
spg <- Temp
coordinates(spg) <- ~ Lon + Lat
e <- extent(spg)
r <- raster(ext=extent(spg),ncol=2500,nrow=2500)
crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
x <- rasterize(Temp[,c("Lon","Lat")], r, Temp$Value, fun=mean)
