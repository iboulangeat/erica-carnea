sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
require(sp)
require(dplyr)
require(ggplot2)
require(sf)
require(raster)
require(rgdal)
require(terra)
require(ade4)

topo_rast =  rast("_data_prod/predRast_topo.grd")
clim_rast = rast("_data_prod/predRast_clim.grd")
soil_ndvi_rast = rast("_data_prod/predRast_soil_ndvi.grd")
rast_envTot <- c(topo_rast, clim_rast, soil_ndvi_rast)
# nspace = niche_space(rast_envTot)

# saveRDS(nspace, "nspace.rds")

nspace = readRDS("nspace.rds")
inertia.dudi(nspace$pca, col = TRUE, row = FALSE)
s.corcircle(nspace$pca$co)
# colnames(pca$tab)=rownames(pca$co) = c( "alti", "slope", "aspect", "CI"   , "northing", "easting" , "bio4" ,  "bio5"   ,  "bio6"   , "gsl"    ,  "gsp",  "gst"     , "ngd0"   ,  "npp"   ,   "scd"   )


#### shape community matrix

sites_all = read.csv("_data_prod/sites_all.csv")
head(sites_all)
hist(sites_all$lat_wgs84)
sites_all$region = "north_fr"
sites_all$region[which(sites_all$pays=="Italie")] = "it"
sites_all$region[which(sites_all$lat_wgs84<44.5)] = "south_fr"

coordinates(sites_all)= c("lon_wgs84","lat_wgs84")
proj4string(sites_all) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

table(sites_all$region)

sites_proj = spTransform(sites_all, crs(rast_envTot))
index = terra::extract(rast_envTot, coordinates(sites_proj),  cells=TRUE)

rastSpfr = rastSpit = rastSpMerc = rast(rast_envTot$scd, nlyrs = 1, vals = 0)
rastSpMerc[index[which(sites_proj$region=="south_fr"),"cell"]] = 1
rastSpfr[index[which(sites_proj$region=="north_fr"),"cell"]] = 1
rastSpit[index[which(sites_proj$region=="it"),"cell"]] = 1


### OMI
com = as.data.frame(c(rastSpit, rastSpfr, rastSpMerc))
com = na.omit(com)
dim(com)
dim(nspace$pca$tab)
pts = as.data.frame(rast_envTot)
pts = na.omit(pts)
na.action(pts)
com = com[-na.action(pts),]
colnames(com) = c("it", "fr", "merc")

OMI <- niche(nspace$pca, com, scannf=F, nf=ncol(com))
# saveRDS(OMI, "OMI.rds")

OMI = readRDS("OMI.rds")

fac.sp = com$it
fac.sp[com$it==1] = "it"
fac.sp[com$fr==1] = "fr"
fac.sp[com$merc==1] = "merc"
fac.sp = as.factor(fac.sp)
  
s.arrow(4* OMI$co)
s.class(OMI$ls[-which(fac.sp=="0"),], fac = fac.sp[-which(fac.sp=="0")], col = c(1,2,3), add.plot=TRUE)

s.arrow(4* nspace$pca$co)
s.class(nspace$pca$li[-which(fac.sp=="0"),], fac = fac.sp[-which(fac.sp=="0")], col = c(1,2,3), add.plot=TRUE)

## create predicted obs
name = "all"
pred.values = rast(paste0("_data_prod/rf.predict.ericar_pa_", name, ".tif"))
pa = pred.values[-na.action(pts)] 
dim(pa)

### plot niche

dens_bg = densitePts2(focalPts=OMI$ls[,1:2], backgroundPts=OMI$ls[,1:2],  R=100, scale = FALSE)
dens_obs = densitePts2(focalPts=OMI$ls[which(pa$class!="0"),1:2], backgroundPts=OMI$ls[,1:2], R=100, density.bg = dens_bg, scale = TRUE)

plot_niche(pca=OMI, density.pts=dens_obs, col.select= c(1,6, 18), omi = TRUE, 
           colo.pts = c("lightyellow", colorRampPalette(c("lightblue", "violet", "darkviolet"), space="Lab")(6)),
           at.pts = c(-0.1, 0.001, 0.2, 0.4, 0.6, 0.8, 1 ),
           at.scaleLab = c(0, 0, 0.2, 0.4, 0.6, 0.8, 1), 
           at.scalePts = 0:6)



