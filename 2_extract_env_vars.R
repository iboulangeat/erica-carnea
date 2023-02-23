# this script is meant to extract env predictors
#-=========================
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
library(rgeos)
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

# zones d'études
#=====================
require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  7.8), ylim = c(43.75, 46.12))
bbox_wo_merc <- tmaptools::bb(xlim = c(5.68,  7.7), ylim = c(44.60, 46.12))

## step 0 rasters de zones de calibration + grilles prediction
rasalti = rast('_data_prod/DEM90_sa.tif')
rasaltiwm = rast('_data_prod/DEM90_sa_wo_merc.tif')
rasalti30 = rast("_data/DEM30-ericar.tif")
bbox_proj = st_bbox(st_transform(st_as_sfc(bbox_wo_merc), crs(rasalti30)))
rasalti30wm = crop(rasalti30, extent(bbox_proj))
terra::writeRaster(rasalti30wm, "_data_prod/DEM30_sa_wo_merc.tif", overwrite=TRUE)

## set of points / observations
sites_all =  read.csv("_data_prod/sites_all.csv")
coordinates(sites_all)= c("lon_wgs84","lat_wgs84")
proj4string(sites_all) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#!test
sum(is.na(sites_all$x_l93))
#!
sites_all$x_l93 = coordinates(spTransform(sites_all, crs("EPSG:2154")))[,1]
sites_all$y_l93 = coordinates(spTransform(sites_all, crs("EPSG:2154")))[,2]

sites_fr = read.csv("_data_prod/sites_df.csv")
coordinates(sites_fr)= c("lon_wgs84","lat_wgs84")
proj4string(sites_fr) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#!test
sum(is.na(sites_fr$x_l93))
#!

## bg points
# sur zone entiere
nona = which(!is.na(rasalti[]))
bg_index= sample(nona, 10*nrow(sites_all))
bg_coords = as.data.frame(xyFromCell(rasalti, bg_index))
coordinates(bg_coords)= c("x","y")
proj4string(bg_coords) = crs(rasalti)

head(sites_all)
sites_all$pa = 1
sites_all_pa_df = sites_all_pa = rbind(sites_all@data[, c("code_releve","date", "x_l93", "y_l93","pays", "pa")], data.frame(code_releve = paste0("pa", bg_index), date = NA, x_l93 = coordinates(bg_coords)[,'x'], y_l93 = coordinates(bg_coords)[,'y'], pays = NA, pa = 0))
coordinates(sites_all_pa)= c("x_l93","y_l93")
proj4string(sites_all_pa) = CRS("EPSG:2154")
plot(sites_all_pa)


write.csv(sites_all_pa_df, "_data_prod/sites_all_pa.csv", row.names = FALSE)


# sur zone wo mercantour
rm(nona, bg_index, bg_coords)
nona = which(!is.na(rasaltiwm[]))
bg_index= sample(nona, 10*nrow(sites_all))
bg_coords = as.data.frame(xyFromCell(rasaltiwm, bg_index))
coordinates(bg_coords)= c("x","y")
proj4string(bg_coords) = crs(rasaltiwm)

sites_all_wo = sites_all[which(coordinates(sites_all)[,"lat_wgs84"] >44.6), ]
sites_all_pa_df_wm = sites_all_pa_wm = rbind(sites_all_wo@data[, c("code_releve","date", "x_l93", "y_l93", "pa")], data.frame(code_releve = paste0("pa", bg_index), date = NA, x_l93 = coordinates(bg_coords)[,'x'], y_l93 = coordinates(bg_coords)[,'y'], pa = 0))
coordinates(sites_all_pa_wm)= c("x_l93","y_l93")
proj4string(sites_all_pa_wm) = CRS("EPSG:2154")
plot(sites_all_pa_wm)


write.csv(sites_all_pa_df_wm, "_data_prod/sites_all_pa_wm.csv", row.names = FALSE)

#============================================================================
## intermediate load
sites_all_pa = read.csv("_data_prod/sites_all_pa.csv")
coordinates(sites_all_pa)= c("x_l93","y_l93")
proj4string(sites_all_pa) = CRS("EPSG:2154")
sites_all_pa_wm = read.csv("_data_prod/sites_all_pa_wm.csv")
coordinates(sites_all_pa_wm)= c("x_l93","y_l93")
proj4string(sites_all_pa_wm) = CRS("EPSG:2154")
rasalti30wm = rast("_data_prod/DEM30_sa_wo_merc.tif")
rasalti = rast('_data_prod/DEM90_sa.tif')
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
library(rgeos)
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
require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  7.8), ylim = c(43.75, 46.12))
bbox_wo_merc <- tmaptools::bb(xlim = c(5.68,  7.7), ylim = c(44.60, 46.12))
#============================================================================

## step 1 topography ## ==========================
rasalti30 = rast("_data/DEM30-ericar.tif")
# full range
data_env_topo = extract_env_topo(sites_all_pa, ras_mnt30 = rasalti30, bbox=bbox, rasType_grid = rasalti)
write.csv(data_env_topo$pts,  "_data_prod/env_topo.csv", row.names = FALSE)
names(data_env_topo$predRast) = c("alti", "slope", "CI", "northing", "easting")
#!test
ncol(rasalti) == ncol(data_env_topo$predRast)
nrow(rasalti) == nrow(data_env_topo$predRast)
#!
terra::writeRaster(data_env_topo$predRast, "_data_prod/predRast_topo.grd", overwrite=TRUE)

# wo_merc 
data_env_topo_wm = extract_env_topo(sites_all_pa_wm, ras_mnt30 = rasalti30, bbox=bbox_wo_merc, rasType_grid = rasalti30wm)
write.csv(data_env_topo_wm$pts,  "_data_prod/env_topo_wm.csv", row.names = FALSE)
names(data_env_topo_wm$predRast) = c("alti", "slope", "CI", "northing", "easting")
#!test
ncol(rasalti30wm) == ncol(data_env_topo_wm$predRast)
nrow(rasalti30wm) == nrow(data_env_topo_wm$predRast)
#!
terra::writeRaster(data_env_topo_wm$predRast, "_data_prod/predRast_topo_wm30.grd", overwrite=TRUE)

## step 2 climate ##====================

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


datasetClim = loadAndClipChelsa (sites_all_pa, path = "/Users/isabelleboulangeat/NextcloudINRAE/MyDrive/data_en_cours/", vars = c(paste0("bio", c(2:6, 12, 15)),"gdd0", "gsl", "gsp", "gst", "scd"), rasType=rasalti, bbox)
write.csv(datasetClim$pts,  "_data_prod/env_clim.csv", row.names = FALSE)
rastgrid_clim = rast(datasetClim$predRast)
#!test
ncol(rasalti) == ncol(rastgrid_clim)
nrow(rasalti) == nrow(rastgrid_clim)
#!
terra::writeRaster(rastgrid_clim, "_data_prod/predRast_clim.grd", overwrite=TRUE)

#wm
datasetClim_wm = loadAndClipChelsa (sites_all_pa_wm, path = "/Users/isabelleboulangeat/NextcloudINRAE/MyDrive/data_en_cours/", vars = c(paste0("bio", c(2:6, 12, 15)),"gdd0", "gsl", "gsp", "gst", "scd"), rasType=rasalti30wm, bbox_wo_merc)
write.csv(datasetClim_wm$pts,  "_data_prod/env_clim_wm.csv", row.names = FALSE)
rastgrid_clim_wm = rast(datasetClim_wm$predRast)
#!test
ncol(rasalti30wm) == ncol(rastgrid_clim_wm)
nrow(rasalti30wm) == nrow(rastgrid_clim_wm)
#!
terra::writeRaster(rastgrid_clim_wm, "_data_prod/predRast_clim_wm30.grd", overwrite=TRUE)

## step 3 NDVI and soil ##===========================


# raster_soil = rast("/Volumes/ISA-RESEARCH/_DATA/eu_soil/eu_STU_EU_Layers/STU_EU_T_SAND.rst")
# crs(raster_soil) = "epsg:3035"
# plot(raster_soil)
# terra::writeRaster(raster_soil, "_data/sandSoil.tif", overwrite=TRUE)
raster_soil = rast("_data/sandSoil.tif")
rasSoil <- terra::project(raster_soil, rasalti)
rasSoil = crop(rasSoil, rasalti)
plot(rasSoil)
terra::writeRaster(rasSoil, "_data_prod/sandSoil90.tif", overwrite=TRUE)
rasSoil30 <- terra::project(raster_soil, ras_alti30)
rasSoil30 = crop(rasSoil30, ras_alti30)
plot(rasSoil30)
terra::writeRaster(rasSoil30, "_data_prod/sandSoil30.tif", overwrite=TRUE)

#
datasetSoil = loadAndClipVar (sites_all_pa, path = "_data_prod/sandSoil90.tif", varname = "sand", rasType=rasalti, bbox=bbox)
write.csv(datasetSoil$pts,  "_data_prod/env_soil.csv", row.names = FALSE)

datasetSoil30 = loadAndClipVar (sites_all_pa_wm, path = "_data_prod/sandSoil30.tif", varname = "sand", rasType=rasalti30wm, bbox=bbox_wo_merc)
write.csv(datasetSoil30$pts,  "_data_prod/env_soil_wm.csv", row.names = FALSE)


## NDVI
ndvi = rast("_data/NDVI-200-2020-ericar.tif")
datasetNDVI = loadAndClipVar(sites_all_pa, path = "_data/NDVI-200-2020-ericar.tif", varname = "ndvi", rasType=rasalti, bbox=bbox)
write.csv(datasetNDVI$pts,  "_data_prod/env_ndvi.csv", row.names = FALSE)

datasetNDVI30 = loadAndClipVar (sites_all_pa_wm, path = "_data/NDVI-200-2020-ericar.tif", varname = "ndvi", rasType=rasalti30wm, bbox=bbox_wo_merc)
write.csv(datasetNDVI30$pts,  "_data_prod/env_ndvi_wm.csv", row.names = FALSE)


#!test
ncol(rasalti) == ncol(datasetSoil$predRast)
nrow(rasalti) == nrow(datasetSoil$predRast)
ncol(rasalti) == ncol(datasetNDVI$predRast)
nrow(rasalti) == nrow(datasetNDVI$predRast)
#!
rast_grid_soil_ndvi = rast(list(sand = datasetSoil$predRast, ndvi= datasetNDVI$predRast))

terra::writeRaster(rast_grid_soil_ndvi, "_data_prod/predRast_soil_ndvi.grd", overwrite=TRUE)

#!test
ncol(rasalti30wm) == ncol(datasetSoil30$predRast)
nrow(rasalti30wm) == nrow(datasetSoil30$predRast)
ncol(rasalti30wm) == ncol(datasetNDVI30$predRast)
nrow(rasalti30wm) == nrow(datasetNDVI30$predRast)
#!

rast_grid_soil_ndvi_wm30 = rast(list(sand = datasetSoil30$predRast, datasetNDVI30$predRast))

terra::writeRaster(rast_grid_soil_ndvi_wm30, "_data_prod/predRast_soil_ndvi_wm30.grd", overwrite=TRUE)


