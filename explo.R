############## data
#install.packages(c("Rcpp","dismo","maptools","glmnet","maxnet","raster","sp","randomForest"))

require(sp)
require(dplyr)
require(ggplot2)
require(ggmap)
require(sf)

pts = read.table("_data/export_478_24032021_151335.txt", sep='\t', h=TRUE)
str(pts)

sites_points = pts %>% dplyr::select(id_releve, date_releve_deb, lon_wgs84, lat_wgs84, x_l93, y_l93, id_precision)

sites_points1 = sites_points %>% filter(!is.na(lon_wgs84))
coordinates(sites_points1)= c("lon_wgs84","lat_wgs84")
proj4string(sites_points1) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#sites_points1 = spTransform(sites_points1, CRS("+init=epsg:4326"))
# bbox = unname(st_bbox(sites_points1))
sites_df <- data.frame(sites_points1)

require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  8.40), ylim = c(43.60, 46.12))

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
data_env_topo = extract_env_topo(sites_points1, pathDD="/Volumes/ISA-RESEARCH/_DATA/", bbox = c(left = 5.68, bottom = 43.60, right = 8.40, top = 46.12))
saveRDS(data_env_topo, "data_env_topo.rds")
##-------------
data_env_topo = readRDS("data_env_topo.rds")
##------------

datasetClim = loadAndClipChelsa (data_env_topo$sites, data_env_topo$bg, path = "/Volumes/ISA-RESEARCH/_DATA/Chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/", vars = c("gdd0", "gsl", "gsp", "gst", "ngd0", "npp", "scd"), rasType=data_env_topo$predRast[1], bbox)

paste0("bio", 1:19)


saveRDS(datasetClim, "data_env_topo_clim.rds")
##-------------
data_env_topo_clim = readRDS("data_env_topo_clim.rds")
##------------

summary(data_env_topo_clim$obs)
summary(data_env_topo_clim$bg)

data_env_topo_clim$obs$response = 1
data_env_topo_clim$bg$response = 0

dataset = rbind(data_env_topo_clim$obs@data[, -c(1:5)], data_env_topo_clim$bg@data)
dataset = na.omit(dataset)
dataset$lf = as.factor(dataset$lf)

####################### quick model
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecm.1486
library(randomForest)

set.seed(myseed)

tuneRF(x=dataset[,1:8],y=as.factor(dataset$response))

rf.ericar<-randomForest(as.factor(response)~.,mtry=4,ntree=500,data=dataset, sampsize = samsize,)

varImpPlot(rf.ericar)

# predict on stack 
rast_envtopo <- c(data_env_topo$predRast, data_env_topo_clim$predRast)
rf.predict.ericar <- predict(rast_envtopo, rf.ericar, type='response', progress='window')


##################### gbif 
library(dismo)
erica.gbif <- gbif("Erica", "carnea")
saveRDS(erica.gbif, "erica.gbif.rds")

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
