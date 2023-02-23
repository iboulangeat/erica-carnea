
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


####################### quick SDM script
##-------------
data = dataset
name = "_all"
data = dataset_fr
name = "_fr"
data = dataset_wm
name = "_wm"

##------------
summary(data)
data_calib = na.omit(data[,which(colnames(data)%in% c("slope", "CI", "northing", "easting", "bio2", "bio3", "bio4","bio5", "bio6", "bio12", "bio15", "gdd0", 'gsl', "gsp", "scd", "sand", "ndvi", "pa"))])

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

rast_envTot <- c(topo_rast, clim_rast, soil_ndvi_rast)
names(rast_envTot)
plot(rast_envTot) ## attention long !!


rf.predict.ericar <- predict(rast_envTot, rf.ericar, type = "response") ## attention long !!
plot(rf.predict.ericar)
writeRaster(rf.predict.ericar, file = paste0("_data_prod/rf.predict.ericar_pa", name, ".tif"), overwrite=TRUE)

rf.predict.ericar.p <- predict(rast_envTot, rf.ericar, type = "prob")
plot(rf.predict.ericar.p$X1)
writeRaster(rf.predict.ericar.p$X1, file = paste0("_data_prod/rf.predict.ericar_prob", name, ".tif"), overwrite=TRUE)

rm(topo_rast, clim_rast, soil_ndvi_rast, rast_envTot, rf.predict.ericar, rf.predict.ericar.p)
# pred30m
topo_rast =  rast("_data_prod/predRast_topo_wm30.grd")
clim_rast = rast("_data_prod/predRast_clim_wm30.grd")
soil_ndvi_rast = rast("_data_prod/predRast_soil_ndvi_wm30.grd")

rast_envTot <- c(topo_rast, clim_rast, soil_ndvi_rast)
names(rast_envTot)

rf.predict.ericar <- predict(rast_envTot, rf.ericar, type = "response") ## attention long !!
plot(rf.predict.ericar)
writeRaster(rf.predict.ericar, file = paste0("_data_prod/rf.predict.ericar_pa_wm30", name, ".tif"), overwrite=TRUE)

rf.predict.ericar.p <- predict(rast_envTot, rf.ericar, type = "prob")
plot(rf.predict.ericar.p$X1)
writeRaster(rf.predict.ericar.p$X1, file = paste0("_data_prod/rf.predict.ericar_prob_wm30", name, ".tif"), overwrite=TRUE)


####################### quick SDM script //


