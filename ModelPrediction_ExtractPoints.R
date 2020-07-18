## extract results based on the points send my collaborators
require(Hmisc)
library(FNN)
library(Matrix)
library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(gstat)
library(sp)
library(gam)
library(mgcv)
library(randomForest)

source("ModelFunctions.R")

NAME <- "NO2"
YEAR_Start <- 2010
YEAR_End <- 2010
Option <- "Daily"##"Daily","Month","Annual" --> daily mean, monthly mean or annual mean?
N_Neighbor <- 4 # how many neighboring grid cells we take into account; if 4, we find the 4 nearest 1 km grid cells, and take average
Point_Name <-"BostonArea"
AverageFigure <- TRUE ## need to plot annual concentration figures as well?
Uncertainty <- FALSE ## need monthly uncertainty as well?
Step4 <- TRUE ## need to do step 4 modeling??
OutputFormat <- "Month" #"Month": one file per year;"Month":one file per month; attention: if choosing month here, Option must be "Daily"

Sep <- "\\"
# Sep = "/"
DirPath <- "D:\\GoogleDrive\\Research\\USTemperature\\"
# DirPath <- "Z:\\"

# DirPath <- "/media/qnap2/"

##################
## DO NOT CHANGE BELOW
DataPath <- paste0("A:\\QianDi\\","assembled_data",Sep,"prediction",Sep,NAME,"_USGrid",Sep) ## where the 1 km prediction is stored
DataPath_Uncertainty <- paste0(DirPath,"assembled_data",Sep,"prediction",Sep,NAME,"_USGrid_Uncertainty",Sep,"") ## where the 1 km uncertainty 

## points of 1 km grid cell
SiteData_1km <- readRDS(paste0(DirPath,"processed_data",Sep,"USGrid",Sep,"Location",Sep,"USGridSite.rds"))
# SiteData_1km <- readRDS(paste0(DirPath,"processed_data",Sep,"USGrid",Sep,"Location",Sep,"USGridSite_North_America_Equidistant_Conic.rds"))

###############
## CASE 1 points of interests are provided
SiteData_point <- readRDS(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,"Location",Sep,Point_Name,"Site.rds"))


# SiteData_point <- read.csv("C:\\Users\\qid335\\Downloads\\FHSJoinListOut.csv")
# Index <- is.element(SiteData_1km$SiteCode,SiteData_point$SiteCode)
# SiteData_point <- SiteData_1km[Index,]
# saveRDS(SiteData_point,"FHSSite.rds")

##############
## CASE 2: points are defined by a region, Option has to be "Nearest"
# SiteData_point<-SiteData_1km[which(SiteData_1km$Lat<40.2328 & SiteData_1km$Lat>39.7544 & SiteData_1km$Lon> -75.411994 & SiteData_1km$Lon< -74.824964),]



#######################################
#### DO NOT CHANGE BELOW #############

  
## read background shape file
# US<-readOGR(dsn = paste0(DirPath,"raw_data",Sep,"data",Sep,"shapefile",Sep,"US_North_America_Equidistant_Conic.shp"))
US<-readOGR(dsn = paste0(DirPath,"raw_data",Sep,"data",Sep,"shapefile",Sep,"US.shp"))

if(Step4)
{
  Mod4 <- readRDS(paste0(DirPath,"assembled_data",Sep,"training",Sep,NAME,"_200",Sep,"Mod_Stage4_100.rds"))
  VariableID = 100
  col = c(rep("character",6))
  col[c(2,3)] = "logical"
  col[c(7,8)] = "numeric"
  VariableList = read.csv(paste0(DirPath,"assembled_data",Sep,"VariableList_",VariableID,".csv"),colClasses = col)
  VariableList = VariableList[!is.na(VariableList$READ),]
}

#####################################
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
RowIndex <- rep(1:nrow(SiteData_point),times = N_Neighbor)
dim(ColIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
dim(RowIndex) <- c(nrow(SiteData_point)*N_Neighbor,1)
dim(dist_num) <- c(nrow(SiteData_point)*N_Neighbor,1)

dists_weight <- sparseMatrix(x = as.numeric(dist_num),i=RowIndex,j = ColIndex,dims=c(nrow(SiteData_point),nrow(SiteData_1km)))

## read data
for(j in YEAR_Start:YEAR_End)
{
  if("Annual" == OutputFormat)
  {
    ## about daily concentration
    N_Day <- as.numeric(as.Date(paste0(j,"-12-31"))-as.Date(paste0(j,"-01-01"))+1)
    StartDate <- as.Date(paste0(j,"-01-01"))
    if(!file.exists(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".rds")))
    {
      Result = data.frame()
      # Result = data.frame(matrix(NA, ncol = N_Day * nrow(SiteData_point), nrow = 5))
      # names(Result) <- c("SiteCode","Lon","Lat","Calendar","Value")
      
      for(i in 1:N_Day)
      {
        CurrentDate = StartDate+i-1
        print(system.time({  
        
          InputFileStep1 = paste0(DataPath,"PredictionStep2_",NAME,"_USGrid_",format(as.Date(CurrentDate,origin = as.Date("1970-01-01")),"%Y%m%d"),"_",format(as.Date(CurrentDate,origin = as.Date("1970-01-01")),"%Y%m%d"),".rds")
      
          TempData <- t(readRDS(InputFileStep1))
          Temp<-SiteData_point
          Temp$Calendar <- as.Date(CurrentDate,origin = as.Date("1970-01-01"))
          Temp$Value <- (dists_weight%*%TempData)@x
          ## step 4 modeling
          if(Step4)
          {
            InputData <- ReadData(DirPath,Sep,VariableID,NAME,Point_Name,as.Date(CurrentDate,origin = as.Date("1970-01-01")),as.Date(CurrentDate,origin = as.Date("1970-01-01")),"prediction",SiteData_point,VariableList)
            InputData <- StandardData(DirPath,InputData,Sep,paste0(j,"_",VariableID))
            Residual <- predict(Mod4,newdata = InputData)
            Temp$Value <- Temp$Value + Residual
          }
          Result <- rbind(Result,Temp)
          # Result[(nrow(SiteData_point)*(i-1)+1):(nrow(SiteData_point)*i)] <- Temp
      
          cat(sprintf("reading...%s,%f,%f\n",InputFileStep1,max(Temp$Value,na.rm = TRUE),min(Temp$Value,na.rm = TRUE)))
          
        }))
        ### for testing only!!! make maps every day
        # spg <- Temp
        # coordinates(spg) <- ~ Lon + Lat
        # e <- extent(spg)
        # r <- raster(ext=extent(spg),ncol=50,nrow=50)
        # crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
        # x <- rasterize(Temp[,c("Lon","Lat")], r, Temp$Value, fun=mean)
        # r.range = c(min(Temp$Value,na.rm = TRUE),max(Temp$Value,na.rm = TRUE))
        # plot(x,col=topo.colors(100),
        #      main = paste0("average PM2.5 ",Point_Name," Year ",j),
        #      axis.args=list(at=seq(r.range[1], r.range[2], 2),
        #                     labels=round(seq(r.range[1], r.range[2], 2),0.5),
        #                     cex.axis=0.6),
        #      lwd=20,
        #      xlim=c(min(Temp$Lon)-plot_margin,max(Temp$Lon)+plot_margin),ylim=c(min(Temp$Lat)-plot_margin,max(Temp$Lat)+plot_margin),
        #      legend.args=list(text='PM2.5 (microgram/m^3)', side=4, font=2, line=2.5, cex=0.8))
      }
    
      ## make annual average figures
      if(AverageFigure)
      {
        Temp_Agg <- aggregate(Value~SiteCode,data = Result,FUN=mean,na.action = na.omit)
        Temp<-SiteData_point
        Result_plot<-merge(Temp,Temp_Agg,by="SiteCode",all.x = TRUE)
    
        # rbPal <- colorRampPalette(c('red','blue'))
        # Col <- rbPal(10)[as.numeric(cut(Result_plot$Value,breaks = 10))]
        # jpeg(paste0(DirPath,Point_Name,"_",j,".jpg"),width=8,height=6,res=600,units='in')
        # plot_margin = 0#IQR(SiteData_point$Lat)/4
        # plot(US,main=paste0("average PM2.5 ",Point_Name," Year ",j),xlim=c(min(SiteData_point$Lon)-plot_margin,max(SiteData_point$Lon)+plot_margin),ylim=c(min(SiteData_point$Lat)-plot_margin,max(SiteData_point$Lat)+plot_margin))
        # points(Result_plot$Lat ~ Result_plot$Lon, col = Col, cex=0.9, pch = 15)
    
        # US<-readOGR(dsn = paste0("C:\\Users\\qid335\\Downloads\\PhillyPlanning_City_Limits\\tl_2016_us_primaryroads.shp"))
        # plot_margin = 0
        # plot(US,main=paste0("average PM2.5 ",Point_Name," Year ",j),xlim=c(min(SiteData_point$Lon)-plot_margin,max(SiteData_point$Lon)+plot_margin),ylim=c(min(SiteData_point$Lat)-plot_margin,max(SiteData_point$Lat)+plot_margin))
        #
        jpeg(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".jpg"),width=8,height=6,res=600,units='in')
        spg <- Result_plot
        coordinates(spg) <- ~ Lon + Lat
        e <- extent(spg)
        r <- raster(ext=extent(spg),ncol=500,nrow=500)
        # crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
        crs(r)<- "+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
        x <- rasterize(Result_plot[,c("Lon","Lat")], r, Result_plot$Value, fun=mean)
        r.range = c(min(Result_plot$Value,na.rm = TRUE),max(Result_plot$Value,na.rm = TRUE))
        
        x1 <- raster(ext = extent(x),nrow = 500, ncol = 500)
        crs(x1) <- crs(x)
        x1 <- resample(x,x1,method = "bilinear")
        plot_margin <-0
        plot(x,col=topo.colors(100),
             main = paste0("average ",NAME," ",Point_Name,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),j),
             axis.args=list(at=seq(r.range[1], r.range[2], 2),
                            labels=round(seq(r.range[1], r.range[2], 2),0.5),
                            cex.axis=0.6),
             lwd=1,asp = 1,
             xlim=c(min(SiteData_point$Lon)-plot_margin,max(SiteData_point$Lon)+plot_margin),ylim=c(min(SiteData_point$Lat)-plot_margin,max(SiteData_point$Lat)+plot_margin),
             legend.args=list(text='PM2.5 (microgram/m^3)', side=4, font=2, line=2.5, cex=0.8))
        plot(US,add=TRUE)
        # plot(US_1,add=TRUE)
        writeRaster(x1, filename=paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",NAME,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".tif"), format="GTiff", overwrite=TRUE)   
        dev.off()
      }
    
      if("Month" == Option)
      {
        Result$Month <- as.numeric(format(Result$Calendar,"%m"))
        Temp_Agg <- aggregate(Value~SiteCode+Month,data = Result,FUN=mean,na.action = na.omit)
        Temp<-SiteData_point
        Result<-merge(Temp,Temp_Agg,by="SiteCode",all.x = TRUE)
      }else if("Annual" == Option)
      {
        Temp_Agg <- aggregate(Value~SiteCode,data = Result,FUN=mean,na.action = na.omit)
        Temp<-SiteData_point
        Result<-merge(Temp,Temp_Agg,by="SiteCode",all.x = TRUE)
      }
    
      saveRDS(Result,paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",NAME,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".rds"))
    }
  }
  
  if("Month" == OutputFormat)
  {
    for(k in 1:12)
    {
      ## about daily concentration
      N_Day <- monthDays(as.Date(paste0(j,"-",k,"-",1)))
      StartDate <- as.Date(paste0(j,"-",k,"-01"))
      
      if(!file.exists(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".rds")))
      {
        Result = data.frame()
        # Result = data.frame(matrix(NA, ncol = N_Day * nrow(SiteData_point), nrow = 5))
        # names(Result) <- c("SiteCode","Lon","Lat","Calendar","Value")
        
        for(i in 1:N_Day)
        {
          CurrentDate = StartDate+i-1
          print(system.time({  
            
            InputFileStep1 = paste0(DataPath,"PredictionStep2_",NAME,"_USGrid_",format(as.Date(CurrentDate,origin = as.Date("1970-01-01")),"%Y%m%d"),"_",format(as.Date(CurrentDate,origin = as.Date("1970-01-01")),"%Y%m%d"),".rds")
            
            TempData <- t(readRDS(InputFileStep1))
            Temp<-SiteData_point
            Temp$Calendar <- as.Date(CurrentDate,origin = as.Date("1970-01-01"))
            Temp$Value <- (dists_weight%*%TempData)@x
            ## step 4 modeling
            if(Step4)
            {
              InputData <- ReadData(DirPath,Sep,VariableID,NAME,Point_Name,as.Date(CurrentDate,origin = as.Date("1970-01-01")),as.Date(CurrentDate,origin = as.Date("1970-01-01")),"prediction",SiteData_point,VariableList)
              InputData <- StandardData(DirPath,InputData,Sep,paste0(j,"_",VariableID))
              Residual <- predict(Mod4,newdata = InputData)
              Temp$Value <- Temp$Value + Residual
            }
            Result <- rbind(Result,Temp)
            # Result[(nrow(SiteData_point)*(i-1)+1):(nrow(SiteData_point)*i)] <- Temp
            
            cat(sprintf("reading...%s,%f,%f\n",InputFileStep1,max(Temp$Value,na.rm = TRUE),min(Temp$Value,na.rm = TRUE)))
            
          }))
        }
        
        ## make annual average figures
        if(AverageFigure)
        {
          Temp_Agg <- aggregate(Value~SiteCode,data = Result,FUN=mean,na.action = na.omit)
          Temp<-SiteData_point
          Result_plot<-merge(Temp,Temp_Agg,by="SiteCode",all.x = TRUE)
          
          jpeg(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".jpg"),width=8,height=6,res=600,units='in')
          spg <- Result_plot
          coordinates(spg) <- ~ Lon + Lat
          e <- extent(spg)
          r <- raster(ext=extent(spg),ncol=500,nrow=500)
          # crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
          crs(r)<- "+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
          x <- rasterize(Result_plot[,c("Lon","Lat")], r, Result_plot$Value, fun=mean)
          r.range = c(min(Result_plot$Value,na.rm = TRUE),max(Result_plot$Value,na.rm = TRUE))
          
          x1 <- raster(ext = extent(x),nrow = 500, ncol = 500)
          crs(x1) <- crs(x)
          x1 <- resample(x,x1,method = "bilinear")
          plot_margin <-0
          plot(x,col=topo.colors(100),
               main = paste0("average ",NAME," ",Point_Name," ",format(StartDate,"%Y%m%d")," ",format(StartDate+N_Day-1,"%Y%m%d")),
               axis.args=list(at=seq(r.range[1], r.range[2], 2),
                              labels=round(seq(r.range[1], r.range[2], 2),0.5),
                              cex.axis=0.6),
               lwd=1,asp = 1,
               xlim=c(min(SiteData_point$Lon)-plot_margin,max(SiteData_point$Lon)+plot_margin),ylim=c(min(SiteData_point$Lat)-plot_margin,max(SiteData_point$Lat)+plot_margin),
               legend.args=list(text='PM2.5 (microgram/m^3)', side=4, font=2, line=2.5, cex=0.8))
          plot(US,add=TRUE)
          # plot(US_1,add=TRUE)
          writeRaster(x1, filename=paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",NAME,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".tif"), format="GTiff", overwrite=TRUE)   
          dev.off()
        }
        
        saveRDS(Result,paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",NAME,"_",format(StartDate,"%Y%m%d"),"_",format(StartDate+N_Day-1,"%Y%m%d"),".rds"))
      }
    }
    
  }
  
  ## extract uncertainty
  if(Uncertainty)
  {
    ## about monthly concentration
    Result = data.frame()
    
    for(i in 1:12)
    {
      N_Day <- monthDays(as.Date(paste0(j,"-",i,"-",1)))
      
      InputFileStep1 = paste0(DataPath_Uncertainty,"PredictionUncertainty_",NAME,"_USGrid_",format(as.Date(paste0(j,"-",i,"-","01")),"%Y%m%d"),"_",format(as.Date(paste0(j,"-",i,"-","01"))+N_Day-1,"%Y%m%d"),".rds")
      
      TempData <- as.matrix(readRDS(InputFileStep1))
      Temp<-SiteData_point
      Temp$Calendar <- paste0(format(as.Date(paste0(j,"-",i,"-","01")),"%Y%m%d"),"_",format(as.Date(paste0(j,"-",i,"-","01"))+N_Day-1,"%Y%m%d"))
      Temp$Value <- (dists_weight%*%TempData)@x
      Result <- rbind(Result,Temp)
      
      cat(sprintf("reading...%s,%f,%f\n",InputFileStep1,max(Temp$Value,na.rm = TRUE),min(Temp$Value,na.rm = TRUE)))
      
    }
    
    ## make annual average figures
    if(AverageFigure)
    {
      Temp_Agg <- aggregate(Value~SiteCode,data = Result,FUN=mean,na.action = na.omit)
      Temp<-SiteData_point
      Result_plot<-merge(Temp,Temp_Agg,by="SiteCode",all.x = TRUE)
      
      jpeg(paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",j,"_Uncertainty.jpg"),width=8,height=6,res=600,units='in')
      spg <- Result_plot
      coordinates(spg) <- ~ Lon + Lat
      e <- extent(spg)
      r <- raster(ext=extent(spg),ncol=50,nrow=50)
      crs(r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
      x <- rasterize(Result_plot[,c("Lon","Lat")], r, Result_plot$Value, fun=mean)
      r.range = c(min(Result_plot$Value,na.rm = TRUE),max(Result_plot$Value,na.rm = TRUE))
      plot_margin <-0
      plot(x,col=topo.colors(100),
           main = paste0("standard deviation of PM2.5 ",Point_Name," Year ",j),
           axis.args=list(at=seq(r.range[1], r.range[2], 0.2),
                          labels=round(seq(r.range[1], r.range[2], 0.2),0.5),
                          cex.axis=0.6),
           lwd=20,
           xlim=c(min(SiteData_point$Lon)-plot_margin,max(SiteData_point$Lon)+plot_margin),ylim=c(min(SiteData_point$Lat)-plot_margin,max(SiteData_point$Lat)+plot_margin),
           legend.args=list(text='PM2.5 (microgram/m^3)', side=4, font=2, line=2.5, cex=0.8)) 
      plot(US,add=TRUE)
      # plot(US_1,add=TRUE)
      dev.off()
    }
    
    saveRDS(Result,paste0(DirPath,"processed_data",Sep,Point_Name,Sep,Point_Name,"_",NAME,"_",j,"_Uncertainty.rds"))
    
  }
  
}


##################################
## if they just return me the ID index
# SiteData_point <- SiteData_1km[c(1,10,100),]
# Index <- is.element(SiteData_1km$ID,SiteData_point$ID)
# SiteData_point <- SiteData_1km[Index,]

# ####################################
# ## create 100 meter grid cells --- in practice, our collaborators will provide us the points
# library(pracma)
# SiteData <- readRDS("D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\USTemperature\\AllUSGrid\\USGrid\\Location\\USGridSite.rds")
# SiteData1 <- SiteData[which(SiteData$Lon > -71.47 & SiteData$Lat < 42.57 & SiteData$Lat > 42.10 & SiteData$Lon < -70.68),]
# 
# SiteData_GCS <- readRDS("D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\USTemperature\\AllUSGrid\\USGrid\\Location\\USGridSite_North_America_Equidistant_Conic.rds")
# Index = is.element(SiteData$SiteCode,SiteData1$SiteCode)
# SiteData1_GCS <- SiteData_GCS[Index,]
# MaxLat <- max(SiteData1_GCS$Lat)
# MinLat <- min(SiteData1_GCS$Lat)
# MaxLon <- max(SiteData1_GCS$Lon)
# MinLon <- min(SiteData1_GCS$Lon)
# 
# SiteData_100 <- meshgrid(seq(MinLon,MaxLon,100),seq(MinLat,MaxLat,100))
# size <- dim(SiteData_100$X)
# X <- SiteData_100$X
# dim(X)<-c(size[1]*size[2],1)
# Y <- SiteData_100$Y
# dim(Y)<-c(size[1]*size[2],1)
# SiteData_100 <- data.frame(SiteCode = as.character(seq(1:(size[1]*size[2]))),Lon=X,Lat = Y, stringsAsFactors=FALSE)
# 
# saveRDS(SiteData_100,"D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\BostonArea\\Location\\BostonAreaSite_North_America_Equidistant_Conic.rds")
# write.csv(SiteData_100,"D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\BostonArea\\Location\\BostonAreaSite_North_America_Equidistant_Conic.csv")
# 
# 
# # #####################################
# # fit stage 4 modeling
# # library(gam)
# # library(parallel)
# # library(mgcv)
# # 
# # ## get data
# # ## get land-use variables at 100 meters
# # DirPath <- "D:\\GoogleDrive\\Research\\USTemperature\\"
# # VariableID_list <-c(99937,99937,99938,99938,99938,99940,99940,99940,99940,99940,99940,99940,99940,99940,99940,99939)
# # YEAR_List <-seq(2000,2015)
# # Sep = "\\"
# # SiteName_Train = "AQRVPM25"
# # # ## prepare data set
# # source("ModelFunctions.R")
# # #
# # ##location of monitors
# # SiteData<-ReadLocation(paste0(DirPath,"processed_data",Sep,SiteName_Train,Sep,"Location",Sep,SiteName_Train,"Site_North_America_Equidistant_Conic"))
# # names(SiteData)[which(names(SiteData) == "Lat")]<-c("Other_Lat")
# # names(SiteData)[which(names(SiteData) == "Lon")]<-c("Other_Lon")
# # 
# # OutputData <- data.frame()
# # for(i in 1:length(YEAR_List))
# # {
# #   cat(sprintf("reading...%d\n",i))
# #   Inputdata <- readRDS(paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_",YEAR_List[i],"_",VariableID_list[i],Sep,"InputData.rds"))
# # Temp1<-Inputdata[,c("USElevation_mea100","NLCD_Barren100","NLCD_Developed100","NLCD_Developed10000","NLCD_Herbaceous100","NLCD_Planted100","NLCD_Shrubland100","NLCD_Water100","NLCD_Wetlands100","NLCD_Impervious100","NLCD_canopy100","RoadDensity_roads1000","REANALYSIS_shum_2m_DailyMean","REANALYSIS_windspeed_10m_DailyMean","REANALYSIS_air_sfc_DailyMean","REANALYSIS_hpbl_DailyMean")]
# #   OutputData <- rbind(OutputData,Temp1)
# # }
# # saveRDS(OutputData,paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"InputData_Step4_100.rds"))
# # 
# # 
# # #######################
# # ## fitting the SD model
# library(randomForest)
# DirPath <- "D:\\GoogleDrive\\Research\\USTemperature\\"
# Sep = "\\"
# InputData <- readRDS(paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"InputData_Step4_100.rds"))
# OutputData <- readRDS(paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"OutputData_CV.rds"))
# OutputData$Date = as.Date(OutputData$CalendarDay,origin = as.Date("1970-01-01"))
# OutputData$Year = as.numeric(format(OutputData$Date,"%Y"))
# OutputData$Month = as.numeric(format(OutputData$Date,"%m"))
# OutputData$res <- OutputData$MonitorData - OutputData$pred_ensemble_2
# 
# Data = cbind(OutputData,InputData)
# Data = Data[complete.cases(Data),]
# saveRDS(Data,paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"InputData_Step4_100_input.rds"))
# Data = readRDS(paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"InputData_Step4_100_input.rds"))
# 
# # h2o.init(nthreads = -1)
# # Data_h2o <- as.h2o(Data)
# # Mod4 <- gam(res ~ USElevation_mea100+NLCD_Barren100+NLCD_Developed100+NLCD_Developed10000+NLCD_Herbaceous100+NLCD_Planted100+NLCD_Shrubland100+NLCD_Water100+NLCD_Wetlands100+NLCD_Impervious100+NLCD_canopy100+RoadDensity_roads1000+REANALYSIS_shum_2m_DailyMean+REANALYSIS_windspeed_10m_DailyMean+REANALYSIS_air_sfc_DailyMean+REANALYSIS_hpbl_DailyMean,data = Data)
# 
# Mod4_ <- randomForest(res ~ USElevation_mea100+NLCD_Barren100+NLCD_Developed100+NLCD_Developed10000+NLCD_Herbaceous100+NLCD_Planted100+NLCD_Shrubland100+NLCD_Water100+NLCD_Wetlands100+NLCD_Impervious100+NLCD_canopy100+RoadDensity_roads1000+REANALYSIS_shum_2m_DailyMean+REANALYSIS_windspeed_10m_DailyMean+REANALYSIS_air_sfc_DailyMean+REANALYSIS_hpbl_DailyMean,data = Data,ntree=500, mtry=2, importance=TRUE)
# 
# Mod4 <- randomForest(res ~ USElevation_mea100+NLCD_Barren100+NLCD_Developed100+NLCD_Developed10000+NLCD_Herbaceous100+NLCD_Planted100+NLCD_Shrubland100+NLCD_Water100+NLCD_Wetlands100+NLCD_Impervious100+NLCD_canopy100+RoadDensity_roads1000+REANALYSIS_shum_2m_DailyMean+REANALYSIS_windspeed_10m_DailyMean+REANALYSIS_air_sfc_DailyMean+REANALYSIS_hpbl_DailyMean,data = Data,ntree=500, mtry=2, importance=TRUE)
# 
# # saveRDS(Mod4,paste0(DirPath,"assembled_data",Sep,"training",Sep,"PM25_99940",Sep,"Mod_Stage4_100.rds"))


### Step 4 for NO2
# library(randomForest)
# OutputData <- readRDS("D:\\GoogleDrive\\Research\\USTemperature\\assembled_data\\training\\NO2_200\\OutputData_CV.rds")
# OutputData$res <- OutputData$MonitorData - OutputData$pred_ensemble_2
# 
# VarList <- c("res","USElevation_mea100","NLCD_Barren100","NLCD_Developed100","NLCD_Developed10000","NLCD_Herbaceous100","NLCD_Planted100","NLCD_Shrubland100","NLCD_Water100","NLCD_Wetlands100","NLCD_Impervious100","NLCD_canopy100","RoadDensity_roads1000","TruckRoute_ShortDis100","TruckRoute_Traffic100","TruckRoute_Density100","REANALYSIS_shum_2m_DailyMean","REANALYSIS_windspeed_10m_DailyMean","REANALYSIS_air_sfc_DailyMean","REANALYSIS_hpbl_DailyMean")
# Data <- OutputData[complete.cases(OutputData[,VarList]),VarList]
# summary(Data)
# Mod4 <- randomForest(res ~ USElevation_mea100+NLCD_Barren100+NLCD_Developed100+NLCD_Developed10000+NLCD_Herbaceous100+NLCD_Planted100+NLCD_Shrubland100+NLCD_Water100+NLCD_Wetlands100+NLCD_Impervious100+NLCD_canopy100+RoadDensity_roads1000+TruckRoute_ShortDis100+TruckRoute_Traffic100+TruckRoute_Density100+REANALYSIS_shum_2m_DailyMean+REANALYSIS_windspeed_10m_DailyMean+REANALYSIS_air_sfc_DailyMean+REANALYSIS_hpbl_DailyMean,data = Data,ntree=500, mtry=2, importance=TRUE)
# saveRDS(Mod4,"D:\\GoogleDrive\\Research\\USTemperature\\assembled_data\\training\\NO2_200\\Mod_Stage4_100.rds")

### change coordinates from North America Equidistant Conic to WGS84
# library(rgeos)
# library(sp)
# library(rgdal)
# library(raster)
# library(gstat)
# library(sp)
# SiteData <- readRDS("D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\BostonArea\\Location\\BostonAreaSite_North_America_Equidistant_Conic.rds")
# SiteData_1 <- SiteData
# coordinates(SiteData_1) <- c("Lon", "Lat")
# proj4string(SiteData_1) <- CRS("+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # North America Equidistant Conic
# CRS.new <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # WGS84
# SiteData_1 <- sp::spTransform(SiteData_1, CRS.new)
# SiteData[,"Lat"] <-SiteData_1@coords[,"Lat"]
# SiteData[,"Lon"] <-SiteData_1@coords[,"Lon"]
# saveRDS(SiteData,"D:\\GoogleDrive\\Research\\USTemperature\\processed_data\\BostonArea\\Location\\BostonAreaSite.rds")
