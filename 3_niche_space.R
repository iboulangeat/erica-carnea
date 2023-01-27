sapply(list.files("R_fct"), function(x)source(paste0("R_fct/",x)))
require(sp)
require(dplyr)
require(ggplot2)
require(sf)
require(raster)
require(rgdal)
require(terra)

# topo_rast =  rast("_data/predRast_topo.grd")
# clim_rast = rast("_data/predRast_clim.grd")
# topo_clip = crop(topo_rast, clim_rast)
# rast_envtopo <- c(topo_clip, clim_rast)

# nspace = niche_space(rast_envtopo)
# inertia.dudi(nspace$pca)$tot # axe1=51%, axe2=11%
# saveRDS(nspace, "nspace.rds")

nspace = readRDS("nspace.rds")
inertia.dudi(nspace$pca, col = TRUE, row = FALSE)

# colnames(pca$tab)=rownames(pca$co) = c( "alti", "slope", "aspect", "CI"   , "northing", "easting" , "bio4" ,  "bio5"   ,  "bio6"   , "gsl"    ,  "gsp",  "gst"     , "ngd0"   ,  "npp"   ,   "scd"   )


data_env_topo_clim = readRDS("data_env_topo_clim.rds")

obs_env = as.data.frame(data_env_topo_clim$obs)
dens_obs = sampling_density(obs_env, nspace, R= 100)


plot_niche(pca=nspace$pca, density.pts=dens_obs, col.select= c(1,2,4)) 
# ax2 gdd0 et scd ; ax2 bio2 ; bio 15 ; easting
