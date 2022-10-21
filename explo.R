




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
dataset = na.omit(dataset[,-which(colnames(dataset)%in% c("gsl", "gst", "lf"))])
tuneRF(x=dataset[,c(1:(ncol(dataset)-1))],y=as.factor(dataset$response))

rf.ericar<-randomForest(as.factor(response)~.,mtry=4,ntree=1000,data=dataset)

varImpPlot(rf.ericar)

rf.predict.ericar.dataset <- predict(rf.ericar, type='prob', new = dataset)[,2]


plot(rf.predict.ericar.dataset~dataset$alti)


#### use spatial RF

library(spatialRF)
dependent.variable.name <- "response"
predictor.variable.names <- colnames(dataset)[-which(colnames(dataset)%in% c("gsl", "gst", "response"))]
dataclean = dataset[which(complete.cases(dataset)),]

xy <- data.frame(rbind(
  coordinates(spTransform(data_env_topo_clim$obs , crs(rasalti))),
  coordinates(spTransform(data_env_topo_clim$bg , crs(rasalti)))
   )[which(complete.cases(dataset)),])
colnames(xy) = c("x", "y")
dim(xy)

dist.mat = dist(xy, diag=T, upper=T)

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
#----------------------------------------------

interactions <- spatialRF::the_feature_engineer(
  data = dataclean,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = 126,
  repetitions = 100,
  verbose = TRUE
)
# Fitting and evaluating a model without interactions.
# Testing 55 candidate interactions.
# No promising interactions found. 
#----------------------------------------------

model.non.spatial <- spatialRF::rf(
  data = dataclean,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = as.matrix(dist.mat),
  distance.thresholds = c(5000, 10000),
  xy = xy, 
  seed = 126,
  verbose = TRUE
)

#----------------------------------------------

print_performance(model.non.spatial)
spatialRF::rf_evaluate(
  model.non.spatial,
  xy = xy,
  metrics = "auc",
  verbose = FALSE
)
#----------------------------------------------

spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)
model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

model.spatial.repeat <- spatialRF::rf_repeat(
  model = model.spatial, 
  repetitions = 30,
  seed = random.seed,
  verbose = FALSE
)
spatialRF::plot_importance(
  model.spatial.repeat, 
  verbose = FALSE
)

#----------------------------------------------

spatialRF::plot_response_curves(
  model.spatial,
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


#----------------------------------------------
# model.spatial <- spatialRF::rf_spatial(
#   model = model.non.spatial,
#   distance.matrix = distance.matrix,
#   method = "mem.moran.sequential", #default method
#   verbose = FALSE,
#   seed = 123
# )
# saveRDS(model.spatial, "model_spatial.rds")
#----------------------------------------------
model.spatial = readRDS("model_spatial.rds")
#----------------------------------------------


p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 

#----------------------------------------------

comparison <- spatialRF::rf_compare(
  models = list(
    `Non-spatial` = model.non.spatial,
    `Spatial` = model.spatial
  ),
  xy = xy,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = "r.squared",
  seed = 125
)


#----------------------------------------------

predicted <- stats::predict(
  object = model.non.spatial,
  data = dataclean, #+add spatial predictors
  type = "response"
)$predictions

mems <- spatialRF::mem_multithreshold(
  distance.matrix = as.matrix(dist.mat),
  distance.thresholds = c(5000, 10000)
)
head(mems)
#----------------------------------------------

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


###
pal <- colorNumeric(c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00",
                      "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000"), values(rf.predict.ericar), na.color = "transparent")
pcol = c("#7f007f", "#0000ff",  "#007fff", "#00ffff", "#00bf00", "#7fdf00",
         "#ffff00", "#ff7f00", "#ff3f00", "#ff0000", "#bf0000")
if (interactive()) {
  library(leaflet)
  library(leafem)
  library(stars)
  
  rf.predict.ericar= read_stars("rf.predict.ericar.tif", package="stars")
  
  leaflet(sites_df)%>%
    addProviderTiles("OpenStreetMap.HOT")%>%
    # addRasterImage(rf.predict.ericar, colors=pal, opacity = 0.7) %>%
    leafem::addGeoRaster(rf.predict.ericar, opacity = 0.8, colorOptions = colorOptions(palette = pal)) %>%
    setView(lng=5.5,lat=45,zoom=6) %>%
    addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date_releve_deb, radius = 0.3, opacity = 0.8, color = "red") %>%
    addLegend(pal = pal, values = values(rf.predict.ericar),
              title = "Suitability")
}
###-------------------------------------------------
topo_rast =  rast("_data/predRast_topo.grd")
clim_rast = rast("_data/predRast_clim.grd")
topo_clip = crop(topo_rast, clim_rast)
rast_envtopo <- c(topo_clip, clim_rast)
rast_envtopo_clip = crop(rast_envtopo, topo_rast$alti)
plot(rast_envtopo)

rf.predict.ericar <- predict(rast_envtopo, rf.ericar, type = "prob")
plot(rf.predict.ericar$X1)

writeRaster(rf.predict.ericar$X1, file = "rf.predict.ericar.tif", overwrite=TRUE)

predicted <- stats::predict(
  object = model.non.spatial,
  data = rast_envtopo, 
  type = "response"
)$predictions

# library(SSDM)
# mod = modelling("RF", coordinates(data_env_topo_clim$obs), stack(rast_envtopo), Xcol = "lon_wgs84", Ycol = "lat_wgs84")
# # cannot use a matrix with these dimensions
# # > 
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
