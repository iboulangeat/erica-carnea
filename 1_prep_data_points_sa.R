
# This script prepare input data : observation points and study area

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
write.csv(sites_df, "_data_prod/sites_df.csv", row.names = FALSE)

# Points italy et france (projet ericar)
#-=============================
pts_w_italy = read.table("_data/Erica carnea_environ variables_ita+fr.txt",  sep='\t', h=TRUE, quote = "", dec = ",")
summary(pts_w_italy)
sites_points_new = unique(pts_w_italy %>% 
  dplyr::select(code_releve, pays, date, lon_wgs84, lat_wgs84, x_l93, y_l93))
dim(sites_points_new)
summary(sites_points_new$lat_wgs84)
summary(sites_points_new$lon_wgs84)


sites_points_new1 = sites_points_new %>% filter(!is.na(lon_wgs84))
coordinates(sites_points_new1)= c("lon_wgs84","lat_wgs84")
proj4string(sites_points_new1) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#sites_points1 = spTransform(sites_points1, CRS("+init=epsg:4326"))
# bbox = unname(st_bbox(sites_points1))
sites_df_new <- data.frame(sites_points_new1)
write.csv(sites_df_new, "_data_prod/sites_df_new.csv", row.names = FALSE)

# all together
#==============
colnames(sites_df)[2] = "date"
sites_df_new2 = unique(pts_w_italy %>% 
                            dplyr::select(code_releve, id_releve, pays, date, lon_wgs84, lat_wgs84, x_l93, y_l93))

sites_all = merge(sites_df[, 1:6], sites_df_new2, by = c("id_releve", "date", "lon_wgs84", "lat_wgs84", "x_l93", "y_l93"), all = TRUE)
sites_all= unique(sites_all)
dim(sites_all)
sites_all[which(is.na(sites_all$pays)), "pays"] = 'France'
sites_all[which(is.na(sites_all$code_releve)), "code_releve"] = sites_all[which(is.na(sites_all$code_releve)), "id_releve"]
sites_all = sites_all %>% filter(!is.na(lon_wgs84))

write.csv(sites_all, "_data_prod/sites_all.csv", row.names = FALSE)



# zones d'études
#=====================
require(tmaptools)
bbox <- tmaptools::bb(xlim = c(5.68,  7.8), ylim = c(43.75, 46.12))
bbox_wo_merc <- tmaptools::bb(xlim = c(5.68,  7.7), ylim = c(44.60, 46.12))

## Raster de base (grille de projection)
ras_mnt90 = rast("_data/DEM90-ericar.tif")
bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(ras_mnt90)))
ras_mnt_crop = crop(ras_mnt90, extent(bbox_proj))
writeRaster(ras_mnt_crop, filename = "_data_prod/DEM90_sa.tif", overwrite = TRUE)
# plot(ras_mnt_crop)

bbox_proj2 = st_bbox(st_transform(st_as_sfc(bbox_wo_merc), crs(ras_mnt90)))
ras_mnt_crop2 = crop(ras_mnt90, extent(bbox_proj2))
writeRaster(ras_mnt_crop2, filename = "_data_prod/DEM90_sa_wo_merc.tif", overwrite = TRUE)
# plot(ras_mnt_crop2)


# visu data
#===========================
library(leaflet) 
library(leafem)
library(stars)
sites_all = read.csv("_data_prod/sites_all.csv")
rasalti = raster('_data_prod/DEM90_sa.tif')
staralti= read_stars("_data_prod/DEM90_sa.tif", proxy = FALSE,  package="stars")

rasaltiwm = raster('_data_prod/DEM90_sa_wo_merc.tif')
staraltiwm= read_stars("_data_prod/DEM90_sa_wo_merc.tif", proxy = FALSE,  package="stars")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  leafem::addGeoRaster(staralti, opacity = 0.6, colorOptions = colorOptions(palette = "Blues")) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  leafem::addGeoRaster(staraltiwm, opacity = 0.6, colorOptions = colorOptions(palette = "Reds")) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "black") 

