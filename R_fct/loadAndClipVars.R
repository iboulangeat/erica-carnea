
loadAndClipVar <- function(pts, path, varname = "var", rasType, bbox){
  
  require(terra)
  require(rgdal)
  require(sf)
  require(sp)
  
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
    ras = rast(path)
    pts$varname = terra::extract(ras, st_transform(st_as_sf(pts), crs(ras)))[,2]
    colnames(pts@data)[which(colnames(pts@data)=="varname")] = varname
    bbox_proj_ras = st_bbox(st_transform(st_as_sfc(bbox), crs(ras)))
    ras_crop1 = terra::crop(ras, extent(bbox_proj_ras))
    ras_proj = terra::project(ras_crop1,rasType)
    ras_crop = terra::crop(ras_proj,rasType, extend = TRUE)

  return((list(pts = pts, predRast = ras_crop)))
}
