# EXTRACT env. data
#-=========================
sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))

## step 0 rasters de zones de calibration
rasalti = rast('_data_prod/DEM90_sa.tif')

## step 1 topography ##
ras_alti30 = rast("_data/DEM30-ericar.tif")
# full range
data_env_topo = extract_env_topo(sites_points1, ras_mnt30 = ras_alti30, bbox=bbox, rasType_projection = rasalti)
write.csv(data_env_topo[1:2],  "_data_prod/env_topo.csv", row.names = FALSE)
rastgrid_topo = rast(data_env_topo$predRast)
terra::writeRaster(rastgrid_topo, "_data_prod/predRast_topo.grd", overwrite=TRUE)
# wo_merc



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



