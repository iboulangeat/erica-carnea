
loadAndClipChelsa <- function(sites, bg, path, vars, rasType, bbox){

  require(terra)
  require(rgdal)
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
  rasters = c()
  for (v in vars){
    ras = rast(paste0(path, "CHELSA_", v, "_1981-2010_V.2.1.tif"))
    sites$v = extract(ras, st_transform(st_as_sf(sites), crs(ras)))[,2]
    colnames(sites@data)[which(colnames(sites@data)=="v")] = v
    bg$v = extract(ras, st_transform(st_as_sf(bg), crs(ras)))[,2]
    colnames(bg@data)[which(colnames(bg@data)=="v")] = v
    ras_proj = terra::project(ras,rasType, align = TRUE, mask = TRUE)
    ras_crop = crop(ras_proj, extent(bbox_proj))
    rasters = c(rasters, ras_crop)
  }
  names(rasters) = vars

  return((list(obs = sites, bg = bg, predRast = rasters)))
}
