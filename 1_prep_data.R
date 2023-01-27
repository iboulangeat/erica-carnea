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

# Points france
#================

pts_lign = read.table("_data/export_ligne_17012023_181105.txt", sep='\t', h=TRUE)
phyto = read.table("_data/export_tab_478_17012023_181229.txt", sep='\t', h=TRUE) # pas de geolocalisation
pts_phyto = read.table("_data/export_tab_rel_478_17012023_181229.txt", sep='\t', h=TRUE, quote = "", dec = ",") # que??
#str(pts)

#filtre precision
sites_points = pts_lign %>% 
  dplyr::select(id_releve, date_releve_deb, lon_wgs84, lat_wgs84, x_l93, y_l93, id_precision) %>%
  dplyr::filter(id_precision == "P")
dim(sites_points)

# sites_phyto = pts_phyto %>% 
#   dplyr::select(id_releve, date_releve_deb, x_l93, y_l93, id_precision) %>%
#   dplyr::filter(id_precision == "P")
# dim(sites_phyto)

#NB ce sont les mêmnes sites

sites_points1 = sites_points %>% filter(!is.na(lon_wgs84))
coordinates(sites_points1)= c("lon_wgs84","lat_wgs84")
proj4string(sites_points1) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#sites_points1 = spTransform(sites_points1, CRS("+init=epsg:4326"))
# bbox = unname(st_bbox(sites_points1))
sites_df <- data.frame(sites_points1)
write.csv(sites_df, "sites_df.csv", row.names = FALSE)

# Points italy et france (projet ericar)
#-=============================
pts_w_italy = read.table("_data/Erica carnea_environ variables_ita+fr.txt",  sep='\t', h=TRUE, quote = "", dec = ",")
summary(pts_w_italy)
sites_points_new = unique(pts_w_italy %>% 
  dplyr::select(id_releve, date, lon_wgs84, lat_wgs84, x_l93, y_l93))
dim(sites_points_new)
summary(sites_points_new$lat_wgs84)
summary(sites_points_new$lon_wgs84)


sites_points_new1 = sites_points_new %>% filter(!is.na(lon_wgs84))
coordinates(sites_points_new1)= c("lon_wgs84","lat_wgs84")
proj4string(sites_points_new1) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#sites_points1 = spTransform(sites_points1, CRS("+init=epsg:4326"))
# bbox = unname(st_bbox(sites_points1))
sites_df_new <- data.frame(sites_points_new1)
write.csv(sites_df_new, "sites_df_new.csv", row.names = FALSE)
plot(sites_points_new1)


# all together
#==============
colnames(sites_df)[2] = "date"
selcol = colnames(sites_df_new)[1:6]

sites_all = rbind(sites_df[, selcol], sites_df_new[selcol])
sites_all= unique(sites_all)
dim(sites_all)

write.csv(sites_all, "sites_all.csv", row.names = FALSE)


# zone d'étude (projection)
#=====================
require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  8.40), ylim = c(43.60, 46.12))
bbox_wo_merc <- tmaptools::bb(xlim = c(5.68,  8.40), ylim = c(44.60, 46.12))

# visu data
#===========================
library(leaflet) 
rasalti = raster('ras_alti.tif')
sites_df = read.csv("sites_df.csv")

pal <- colorNumeric(c("#ffeda0", "#feb24c", "#f03b20"), values(rasalti),
                    na.color = "transparent")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  addRasterImage(rasalti, colors = pal, opacity = 0.7) %>%
  setView(lng=5.5,lat=45,zoom=7) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "red")

# EXTRACT env. data
#-=========================
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))

## step -1 raster de base ##
pathDD = "/Volumes/ISA-RESEARCH/_DATA/eu_topography/EarthEnv_DEM90.tif"
ras_mnt90 = rast(pathDD)
bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(ras_mnt90)))
ras_mnt_crop = crop(ras_mnt90, extent(bbox_proj))
writeRaster(ras_mnt_crop, file = "ras_alti.tif", format = "GTIFF")

## step 0 rasters de zones de calibration
rasalti = rast('ras_alti.tif')


## step 1 topography ##
data_env_topo = extract_env_topo(sites_points1, rasalti, bbox=bbox, rasType_projection = rasalti)
saveRDS(data_env_topo[1:2], "data_env_topo.rds")
rastgrid_topo = rast(data_env_topo$predRast)
terra::writeRaster(rastgrid_topo, "_data/predRast_topo.grd", overwrite=TRUE)
terra::writeRaster(rastgrid_topo$alti, "ras_alti.tif" , overwrite=TRUE)
##-------------
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
data_env_topo = readRDS("data_env_topo.rds")
##------------

## step 2 climate ##
datasetClim = loadAndClipChelsa (data_env_topo$obs, data_env_topo$bg, path = "/Volumes/ISA-RESEARCH/_DATA/Chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/", vars = c(paste0("bio", c(2:6, 12, 15)),"gdd0", "gsl", "gsp", "gst", "scd"), rasType=rasalti, bbox)

# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# gdd0 = heat sum of all days above the 0°C temperature accumulated over 1 year
# gsl = Length of the growing season (TREELIM) !! plein de NA
# gsp = precipitation sum accumulated on all days during the growing season )TREELIM)
# gst = Mean temperature of all growing season days (TREELIM) !! plein de NA
# ngd0 = Number of days at which tas > 0°C
# npp = Calculated based on the ‘Miami model’
# scd = Number of days with snowcover (TREELIM)

saveRDS(datasetClim[1:2], "data_env_topo_clim.rds")
rastgrid_clim = rast(datasetClim$predRast)
terra::writeRaster(rastgrid_clim, "_data/predRast_clim.grd", overwrite=TRUE)

## step 3 NDVI and soil
shp_soil = readOGR("/Volumes/ISA-RESEARCH/_DATA/eu_carbon_soildatabase_ecochange/carbon_soildatabase.shp")
shp_soil_proj = spTransform(shp_soil, crs(rastgrid_clim))
shp_soil_vect = terra::vect(shp_soil_proj)
rasSoil <- terra::rasterize(shp_soil_vect, rastgrid_clim, field = "PCAREA")
plot(rasSoil)

terra::writeRaster(rasSoil, "_data/carbonSoil.tif", overwrite=TRUE)

datasetSoil = loadAndClipVar (datasetClim$obs, datasetClim$bg, path = "_data/carbonSoil.tif", varname = "soilcarbon", rasType=rasalti, bbox=bbox)

saveRDS(datasetSoil[1:2], "data_env_topo_clim_soil.rds")

## NDVI
ndvi = rast("_data/NDVI-200-2020-ericar.tif")
datasetNDVI = loadAndClipVar(datasetSoil$obs, datasetSoil$bg, path = "_data/NDVI-200-2020-ericar.tif", varname = "ndvi", rasType=rasalti, bbox=bbox)
saveRDS(datasetNDVI[1:2], "data_env_topo_clim_soil_ndvi.rds")


rast_grid_soil_ndvi = rast(list(soilcarbon = crop(datasetSoil$predRast, datasetClim$predRast$bio2), 
                               ndvi= crop(datasetNDVI$predRast, datasetClim$predRast$bio2)))

terra::writeRaster(rast_grid_soil_ndvi, "_data/predRast_soil_ndvi.grd", overwrite=TRUE)



