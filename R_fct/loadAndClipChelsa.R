
loadAndClipChelsa <- function(obs, bg, path, vars, rasType, bbox){

  require(terra)
  require(rgdal)
  require(sf)
  require(sp)
  
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
  rasters = c()
  for (v in vars){
    print(v)
    ras = rast(paste0(path, "CHELSA_", v, "_1981-2010_V.2.1.tif"))
    obs$v = terra::extract(ras, st_transform(st_as_sf(obs), crs(ras)))[,2]
    colnames(obs@data)[which(colnames(obs@data)=="v")] = v
    bg$v = terra::extract(ras, st_transform(st_as_sf(bg), crs(ras)))[,2]
    colnames(bg@data)[which(colnames(bg@data)=="v")] = v
    bbox_proj_ras = st_bbox(st_transform(st_as_sfc(bbox), crs(ras)))
    ras_crop1 = crop(ras, extent(bbox_proj_ras))
    ras_proj = terra::project(ras_crop1,rasType, align = TRUE, mask = TRUE)
    ras_crop = terra::crop(ras_proj,rasType)
    rasters = c(rasters, ras_crop)
  }
  names(rasters) = vars

  return((list(obs = obs, bg = bg, predRast = rasters)))
}
