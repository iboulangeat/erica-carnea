############## data
#install.packages(c("Rcpp","dismo","maptools","glmnet","maxnet","raster","sp","randomForest"))

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


pts = read.table("_data/export_478_24032021_151335.txt", sep='\t', h=TRUE)
#str(pts)

sites_points = pts %>% dplyr::select(id_releve, date_releve_deb, lon_wgs84, lat_wgs84, x_l93, y_l93, id_precision)

sites_points1 = sites_points %>% filter(!is.na(lon_wgs84))
coordinates(sites_points1)= c("lon_wgs84","lat_wgs84")
proj4string(sites_points1) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#sites_points1 = spTransform(sites_points1, CRS("+init=epsg:4326"))
# bbox = unname(st_bbox(sites_points1))
sites_df <- data.frame(sites_points1)

require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  8.40), ylim = c(43.60, 46.12))

## visualisation
p <- ggmap(get_map(bbox, source = 'osm',
                         maptype ='terrain',
                         color = 'color'))
p + geom_point(data=sites_df,  aes(x=lon_wgs84, y=lat_wgs84), size=1) 

library(leaflet) 
leaflet(sites_df)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 1, opacity = 0.8, color = "red")

# Providers: 
#   OpenTopoMap
#   Thunderforest.Landscape
# Jawg.Terrain
#GeoportailFrance.plan

phyto = read.table("_data/export_releves_478_20052022_142421.txt", sep='\t', h=TRUE)
str(phyto)

sites_phyto = phyto %>% dplyr::select(id_releve, date_releve_fin, long_wgs84, lat_wgs84, x_l93, y_l93)


####################### match env. data
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))

data_env_topo = extract_env_topo(sites_points1, pathDD="/Volumes/ISA-RESEARCH/_DATA/")
saveRDS(data_env_topo, "data_env_topo.rds")
rastgrid_topo = rast(data_env_topo$predRast)
terra::writeRaster(rastgrid_topo, "_data/predRast_topo.grd", overwrite=TRUE)
terra::writeRaster(rastgrid_topo$alti, "ras_alti.tif" , overwrite=TRUE)
##-------------
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
data_env_topo = readRDS("data_env_topo.rds")
rasalti = rast('ras_alti.tif')

##------------
datasetClim = loadAndClipChelsa (data_env_topo$obs, data_env_topo$bg, path = "/Volumes/ISA-RESEARCH/_DATA/Chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/", vars = c(paste0("bio", c(1:6,12,15)),"gdd0", "gsl", "gsp", "gst", "ngd0", "npp", "scd"), rasType=rasalti, bbox)

# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# gdd0 = heat sum of all days above the 0°C temperature accumulated over 1 year
# gsl = Length of the growing season (TREELIM)
# gsp = precipitation sum accumulated on all days during the growing season )TREELIM)
# gst = Mean temperature of all growing season days (TREELIM)
# ngd0 = Number of days at which tas > 0°C
# npp = Calculated based on the ‘Miami model’
# scd = Number of days with snowcover (TREELIM)

saveRDS(datasetClim, "data_env_topo_clim.rds")
rastgrid_clim = rast(datasetClim$predRast)
terra::writeRaster(rastgrid_clim, "_data/predRast_clim.grd", overwrite=TRUE)

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
data_env_topo_clim = readRDS("data_env_topo_clim.rds")
##------------

summary(data_env_topo_clim$obs)
summary(data_env_topo_clim$bg)

data_env_topo_clim$obs$response = 1
data_env_topo_clim$bg$response = 0

dataset = rbind(data_env_topo_clim$obs@data[, -c(1:5)], data_env_topo_clim$bg@data)
dataset$lf = as.factor(dataset$lf)

####################### quick SDM
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecm.1486
library(randomForest)

set.seed(126)
dataset = na.omit(dataset[,-3])
tuneRF(x=dataset[,c(1:(ncol(dataset)-1))],y=as.factor(dataset$response))

rf.ericar<-randomForest(as.factor(response)~.,mtry=4,ntree=1000,data=dataset)

varImpPlot(rf.ericar)

rf.predict.ericar.dataset <- predict(rf.ericar, type='prob', new = dataset)[,2]


plot(rf.predict.ericar.dataset~dataset$alti)


#### use spatial RF

library(spatialRF)
dependent.variable.name <- "response"
predictor.variable.names <- colnames(dataset)[-ncol(dataset)]
dataclean = dataset[which(complete.cases(dataset)),]

xy <- data.frame(rbind(coordinates(data_env_topo_clim$obs), coordinates(data_env_topo_clim$bg))[which(complete.cases(dataset)),])
colnames(xy) = c("x", "y")
dim(xy)

distance.matrix = dist(xy, diag=T, upper=T)

# spatialRF::plot_training_df(
#   data = dataclean,
#   dependent.variable.name = dependent.variable.name,
#   predictor.variable.names = predictor.variable.names,
#   ncol = 3,
#   point.color = viridis::viridis(100, option = "F"),
#   line.color = "gray30"
# )
# 
# 
# model.5vars <- spatialRF::rf(
#   data = dataclean,
#   dependent.variable.name = dependent.variable.name,
#   predictor.variable.names = c("bio2", "bio3", "bio4", "bio12", "gsp"),
#   xy = xy, #not needed by rf, but other functions read it from the model
#   seed = 123,
#   verbose = FALSE
# )

model.non.spatial <- spatialRF::rf(
  data = dataclean,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  xy = xy, 
  seed = 126,
  verbose = TRUE
)

print_performance(model.non.spatial)
spatialRF::rf_evaluate(
  model.non.spatial,
  xy = xy,
  metrics = "auc",
  verbose = FALSE
)

spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)
spatialRF::plot_response_curves(
  model.non.spatial,
  variables = NULL,
  quantiles = 0.5,
  ncol = 3
)
spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(
    3, #same number of colors as quantiles
    option = "F", 
    end = 0.9
  ),
  ncol = 3,
  show.data = TRUE
)


model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  distance.matrix = distance.matrix,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = 123
)

comparison <- rf_compare(
  models = list(
    allvars = model.non.spatial,
    vars5 = model.5vars
  ),
  xy = xy,
  metrics = c("r.squared", "rmse"),
  n.cores = 1
)


###################### predict on stack 


library(leaflet) 
rasalti = as(rast('ras_alti.tif'), "Raster")

pal <- colorNumeric(c("#ffeda0", "#feb24c", "#f03b20"), values(rasalti),
                    na.color = "transparent")
leaflet(sites_df)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  addRasterImage(rasalti, colors = pal, opacity = 0.7) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 0.3, opacity = 0.8, color = "red")

###--------------------------------------------------
topo_rast =  rast("_data/predRast_topo.grd")
clim_rast = rast("_data/predRast_clim.grd")
topo_clip = crop(topo_rast, clim_rast)
rast_envtopo <- c(topo_clip, clim_rast)
rast_envtopo
rf.predict.ericar <- predict(rast_envtopo, rf.ericar)
writeRaster(rf.predict.ericar, file = "rf.predict.ericar.tif", overwrite=TRUE)


##################### gbif 
library(dismo)
erica.gbif <- gbif("Erica", "carnea")
saveRDS(erica.gbif, "erica.gbif.rds")
##-------------
erica.gbif = readRDS("erica.gbif.rds")
##------------

head(erica.gbif)

library(leaflet) 
leaflet(data.frame(erica.gbif))%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 1, opacity = 0.8) 

library(maptools)
files <- list.files(path=paste(system.file(package="dismo"),
                               '/ex', sep=''), pattern='grd', full.names=TRUE )
env<-stack(files[1:8])
data(wrld_simpl)
plot(env, 1)
plot(wrld_simpl,add=TRUE)
