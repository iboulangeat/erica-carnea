
loadAndClipVar <- function(obs, bg, path, varname = "var", rasType, bbox){
  
  require(terra)
  require(rgdal)
  require(sf)
  require(sp)
  
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
    ras = rast(path)
    obs$varname = terra::extract(ras, st_transform(st_as_sf(obs), crs(ras)))[,2]
    colnames(obs@data)[which(colnames(obs@data)=="varname")] = varname
    bg$varname = terra::extract(ras, st_transform(st_as_sf(bg), crs(ras)))[,2]
    colnames(bg@data)[which(colnames(bg@data)=="varname")] = varname
    bbox_proj_ras = st_bbox(st_transform(st_as_sfc(bbox), crs(ras)))
    ras_crop1 = terra::crop(ras, extent(bbox_proj_ras))
    ras_proj = terra::project(ras_crop1,rasType, align = TRUE, mask = TRUE)
    ras_crop = terra::crop(extend(ras_proj,rasType), rasType)

  return((list(obs = obs, bg = bg, predRast = ras_crop)))
}
