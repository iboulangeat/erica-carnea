
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


##-------------
data = readRDS("data_env_topo_clim_soil_ndvi.rds")
##------------

summary(data$obs)
summary(data$bg)

data$obs$response = 1
data$bg$response = 0

dataset = rbind(data$obs@data[, -c(1:5)], data$bg@data)
#dataset$lf = as.factor(dataset$lf)
dataset = na.omit(dataset)

####################### quick SDM
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecm.1486
library(randomForest)

set.seed(126)
#dataset = na.omit(dataset[,which(colnames(dataset)%in% c("slope", "CI", "northing", "easting", "bio2", "bio3", "bio4", "bio6", "bio12", "bio15", "gdd0", 'gsl', "gsp", "scd", "response"))])
tuneRF(x=dataset[,c(1:(ncol(dataset)-1))],y=as.factor(dataset$response))

rf.ericar<-randomForest(as.factor(response)~.,mtry=8,ntree=1000,data=dataset)

varImpPlot(rf.ericar)
rf.ericar
rf.predict.ericar.dataset <- predict(rf.ericar, type='prob', new = dataset)[,2]


plot(rf.predict.ericar.dataset~dataset$gsp)

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


###################### predict on stack 


library(leaflet) 
rasalti = raster('ras_alti.tif')
sites_df = read.csv("sites_df.csv")

pal <- colorNumeric(c("#ffeda0", "#feb24c", "#f03b20"), values(rasalti),
                    na.color = "transparent")

leaflet(sites_df)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  addRasterImage(rasalti, colors = pal, opacity = 0.7) %>%
  setView(lng=5.5,lat=45,zoom=7) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 0.3, opacity = 0.8, color = "red")

###-------------------------------------------------
topo_rast =  rast("_data/predRast_topo.grd")
clim_rast = rast("_data/predRast_clim.grd")
soil_ndvi_rast = rast("_data/predRast_soil_ndvi.grd")

topo_clip = crop(topo_rast, clim_rast)
rast_envTot <- c(topo_clip, clim_rast, soil_ndvi_rast)
plot(rast_envTot) ## attention long !!

rf.predict.ericar <- predict(rast_envTot, rf.ericar, type = "response")
plot(rf.predict.ericar)
writeRaster(rf.predict.ericar, file = "rf.predict.ericar.tif", overwrite=TRUE)

#rf.predict.ericar <- predict(rast_envTot, rf.ericar, type = "prob")
#plot(rf.predict.ericar$X1)
#writeRaster(rf.predict.ericar$X1, file = "rf.predict.ericar.tif", overwrite=TRUE)


                                                 

# plot large raster image
                               
library(leaflet)
library(leafem)
library(stars)

rf.predict.ericar= read_stars("rf.predict.ericar.tif", proxy = FALSE,  package="stars")
plot(rf.predict.ericar)
rf.values = raster("rf.predict.ericar.tif")
rf.values = rf.values-1
summary(rf.values[])

###
#pcol = c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00", "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000")
pcol <-c("#FF000000", "#ff9988", "#007fff")

leaflet(sites_df)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
#  addRasterImage(rf.predict.ericar, colors=pal, opacity = 0.7) %>% # trop gros raster!!
#  leafem::addGeoRaster(rf.predict.ericar, opacity = 0.8, colorOptions = colorOptions(palette = pal)) %>%
  leafem::addGeoRaster(rf.predict.ericar, opacity = 0.6, colorOptions = colorOptions(palette = pcol)) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 0.3, opacity = 0.8, color = "black") #%>%
#  addLegend(pal = pal, values = rf.predict.ericar$values,
#            title = "Suitability")

