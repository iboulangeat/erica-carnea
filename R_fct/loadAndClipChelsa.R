
loadAndClipChelsa <- function(pts, path, vars, rasType, bbox){

  require(terra)
  require(rgdal)
  require(sf)
  require(sp)
  
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
  rasters = c()
  for (v in vars){
    print(v)
    ras = rast(paste0(path, "CHELSA_", v, "_1981-2010_V.2.1.tif"))
    pts$v = terra::extract(ras, st_transform(st_as_sf(pts), crs(ras)))[,2]
    colnames(pts@data)[which(colnames(pts@data)=="v")] = v
    bbox_proj_ras = st_bbox(st_transform(st_as_sfc(bbox), crs(ras)))
    ras_crop1 = crop(ras, extent(bbox_proj_ras))
    ras_proj = terra::project(ras_crop1,rasType, align = TRUE, mask = TRUE)
    ras_crop = terra::crop(ras_proj,rasType)
    rasters = c(rasters, ras_crop)
  }
  names(rasters) = vars

  return((list(pts = pts, predRast = rasters)))
}
