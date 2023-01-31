
##### DATA AND MODEL
require(sp)
require(dplyr)
require(ggplot2)
require(ggmap)
require(sf)
require(raster)
require(terra)
require(stars)
require(starsExtra)
require(shadow)
require(rgdal)

# calib with bg = all et pres = all
dataset = read.csv("_data_prod/env_topo.csv") %>%
  inner_join(read.csv("_data_prod/env_clim.csv")) %>%
  inner_join(read.csv("_data_prod/env_soil.csv")) %>%
  inner_join(read.csv("_data_prod/env_ndvi.csv"))
head(dataset)
dim(dataset)
# calib with bg = all et pres = france
dataset_fr = dataset[-which(dataset$pays=="Italie"),]
# calib with bg = wm et pres = all (extra)
dataset_wm = read.csv("_data_prod/env_topo_wm.csv") %>%
  inner_join(read.csv("_data_prod/env_clim_wm.csv")) %>%
  inner_join(read.csv("_data_prod/env_soil_wm.csv")) %>%
  inner_join(read.csv("_data_prod/env_ndvi_wm.csv"))
head(dataset_wm)
dim(dataset_wm)

##-------------
data = dataset_fr
##------------

summary(data)
data_calib = na.omit(data[,which(colnames(data)%in% c("slope", "CI", "northing", "easting", "bio2", "bio3", "bio4","bio5", "bio6", "bio12", "bio15", "gdd0", 'gsl', "gsp", "scd", "sand", "ndvi", "pa"))])

####################### quick SDM
#====================================
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecm.1486
library(randomForest)

set.seed(126)
tuneRF(x=data_calib[,-1],y=as.factor(data_calib$pa))

rf.ericar<-randomForest(as.factor(pa)~.,mtry=4,ntree=1000,data=data_calib)

varImpPlot(rf.ericar)
rf.ericar
rf.predict.ericar.dataset <- predict(rf.ericar, type='prob', new = data_calib)[,2]


############## spatial RF
# library(spatialRF)
# dependent.variable.name <- "response"
# predictor.variable.names <- colnames(dataset)[-which(colnames(dataset)%in% c("response"))]
# 
# model.non.spatial <- spatialRF::rf(
#   data = dataset,
#   dependent.variable.name = dependent.variable.name,
#   predictor.variable.names = predictor.variable.names,
#   seed = 126,
#   verbose = TRUE
# )



### stack raster prediction
#-----------------------------
topo_rast =  rast("_data_prod/predRast_topo.grd")
clim_rast = rast("_data_prod/predRast_clim.grd")
soil_ndvi_rast = rast("_data_prod/predRast_soil_ndvi.grd")

topo_clip = crop(topo_rast, clim_rast)
rast_envTot <- c(topo_clip, clim_rast, soil_ndvi_rast)
names(rast_envTot)
plot(rast_envTot) ## attention long !!


rf.predict.ericar <- predict(rast_envTot, rf.ericar, type = "response") ## attention long !!
plot(rf.predict.ericar)
writeRaster(rf.predict.ericar, file = "_data_prod/rf.predict.ericar_pa_fr.tif", overwrite=TRUE)

rf.predict.ericar.p <- predict(rast_envTot, rf.ericar, type = "prob")
plot(rf.predict.ericar.p$X1)
writeRaster(rf.predict.ericar.p$X1, file = "_data_prod/rf.predict.ericar_prob_fr.tif", overwrite=TRUE)

                                                
# plot results large raster image
##-----------------------------------                               
library(leaflet)
library(leafem)
library(stars)
sites_all = read.csv("_data_prod/sites_all.csv")

rf.predict.ericar= read_stars("_data_prod/rf.predict.ericar_pa.tif", proxy = FALSE,  package="stars")
plot(rf.predict.ericar)
rf.values = raster("_data_prod/rf.predict.ericar_pa.tif")
rf.values = rf.values-1
summary(rf.values[])

pcol <-c("#FF000000", "#ff9988", "#007fff")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  leafem::addGeoRaster(rf.predict.ericar, opacity = 0.6, colorOptions = colorOptions(palette = pcol)) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "black")

##
rf.predict.ericar.p= read_stars("_data_prod/rf.predict.ericar_prob.tif", proxy = FALSE,  package="stars")
plot(rf.predict.ericar.p)
rf.values.p = raster("_data_prod/rf.predict.ericar_prob.tif")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  leafem::addGeoRaster(rf.predict.ericar.p, opacity = 0.8, colorOptions = colorOptions(palette = "Blues")) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "black") 

