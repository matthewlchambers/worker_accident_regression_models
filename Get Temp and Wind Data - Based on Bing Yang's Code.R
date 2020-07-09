library(ncdf4)
library(raster)
library(maptools)
library(rgdal)
library(sp)
library(sf)

library(exactextractr)
year_single = as.numeric(commandArgs(trailingOnly=TRUE))

if (!dir.exists(paste0("./", year_single))) {
  dir.create(paste0("./", year_single))
  
}
months = c("01")

for (month_single in months) {

shp<-st_read("./cb_2016_us_county_20m/cb_2016_us_county_20m.shp")
statefip<-shp$STATEFP
countyfip<-shp$COUNTYFP
year<-rep(year_single, length(statefip))
month<-rep(month_single, length(statefip))
#system(paste0("wget --directory-prefix=./inversions/ -nc ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/pres.sfc.", year_single, ".nc"))
#system(paste0("wget --directory-prefix=./inversions/ -nc ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/air.2m.", year_single, ".nc"))
#system(paste0("wget --directory-prefix=./inversions/ -nc ftp://ftp.cdc.noaa.gov/Datasets/NARR/pressure/air.", year_single, month_single, ".nc"))

surf_pres_file<-paste0("./inversions/pres.sfc.", year_single, ".nc")
surf_pres <- nc_open(surf_pres_file)
temp_file<-paste0("./inversions/air.", year_single, month_single, ".nc")
temp <- nc_open(temp_file)
surf_temp_file<-paste0("./inversions/air.2m.", year_single, ".nc")
surf_temp <- nc_open(surf_temp_file)
time_temp<-ncvar_get(temp,"time")
time_surf_pres<-ncvar_get(surf_pres,"time")
surf_pres_array<-ncvar_get(surf_pres, "pres")
time_surf_temp<-ncvar_get(surf_temp,"time")
surf_temp_array<-ncvar_get(surf_temp,"air")
level <- ncvar_get(temp,"level")
time_temp<-ncvar_get(temp,"time")
temp_array <- ncvar_get(temp, "air")
temp_temp<-raster(temp_file, level=1)

for (t in time_temp) {
  start_time<-Sys.time()
  
  ti<-rep(t, length(statefip))

surf_pres_mat<-surf_pres_array[, , which(time_surf_pres == t)]/100
#divided by 100 to convert to hpa then divided by 25
next_higher_mat<-(25*(round(surf_pres_mat/25))-25)
next_higher_mat<-ifelse(next_higher_mat>300 & next_higher_mat<700, 50*floor(next_higher_mat/50), next_higher_mat)

next_higher_mat<-ifelse(next_higher_mat>1000, 1000, next_higher_mat)
uniq_values<-min(unique(as.vector(next_higher_mat[!is.na(next_higher_mat)])) %in% level)

#surf_pres_rounded<-50*(round(surf_pres_mat/50))-50


surf_temp_mat<-surf_temp_array[, , which(time_surf_temp == t)]


asdf<-extent(temp_temp)
asdf_crs<-crs(temp_temp)

for (mpa in level) {
pres_bin_mat <- ifelse(next_higher_mat == mpa, 1, 0)
if (!exists("surf_pres_mat_2")){
surf_pres_mat_2<-pres_bin_mat*temp_array[, , which(level==mpa), which(time_temp == t)]
}
else {
  surf_pres_mat_2<-surf_pres_mat_2+pres_bin_mat*temp_array[, , which(level==mpa), which(time_temp == t)]
}
}
temp_diff<-((surf_temp_mat - surf_pres_mat_2)<0)
rm("surf_pres_mat_2")
max(temp_diff, na.rm=TRUE)
diff_raster<-raster(temp_diff)
extent(diff_raster)<-asdf
crs(diff_raster)<-asdf_crs


weighted_sum<-function(x){
  sum(x[, 1]*x[, 2])
}

sum_of_weights<-function(x){
  sum(x[, 2])
}
totals<-function(x){
  sum(x[, 1])
  
  
}

#list_inversions<-extract(diff_raster, shp, weights=TRUE, normalizeWeights=FALSE, small=TRUE)

# for(i in 1:50) {
#   stuff<-list_pm25[[i]]
#   test<-!is.nan(stuff[, 1])
#   stuff[, 1]<-ifelse(test, stuff[, 1], 0)
#   stuff[, 2]<-stuff[, 2]*test
#   list_pm25[[i]]<-stuff
#   
# }
#inversions<-sapply(list_inversions, weighted_sum)
#total_weight<-sapply(list_inversions, sum_of_weights)
list_inversions<-exact_extract(diff_raster, shp)
inversions_total<-sapply(list_inversions, totals)
inversions_fraction<-sapply(list_inversions, weighted_sum)
total_weight<-sapply(list_inversions, sum_of_weights)
df<-cbind.data.frame(statefip, countyfip, year, month, ti,  inversions_total, inversions_fraction,  total_weight)
write.csv(df, file=paste("./",year_single, "/inversions_", year_single, month_single, "_", t, ".csv", sep=""), row.names=FALSE)
end_time<-Sys.time()
print(end_time-start_time)
}
unlink(temp_file)
}