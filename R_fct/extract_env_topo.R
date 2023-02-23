extract_env_topo = function(sp_Points, ras_mnt30_proj , bbox, rasType_grid=NULL){
  require(raster)
  require(terra)
  require(stars)
  require(starsExtra)
  require(shadow)
  require(rgdal)
  require(sf)

  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(ras_mnt30_proj)))
  
  print("crop rast mnt")
  
  ras_mnt_crop = crop(ras_mnt30_proj, extent(bbox_proj))

  print('calc terrain')
  
  ras_slope = terrain(ras_mnt_crop, "slope", neighbors = 8, unit= "degrees")
  
  ## need projected raster for calc
  st_mnt = st_as_stars(ras_mnt_crop)
  st_aspect = starsExtra::aspect(st_mnt)
  
  convergence = starsExtra::CI(st_aspect, 3)
  
  ras_aspect_crop = rast(as(st_aspect, "Raster"))
 
  ras_CI_crop = rast(as(convergence, "Raster"))
  
  print('extract points')
  sp_Points$alti = terra::extract(ras_mnt_crop, st_transform(st_as_sf(sp_Points), crs(ras_mnt_crop)))[,2]
  sp_Points$slope = terra::extract(ras_slope, st_transform(st_as_sf(sp_Points), crs(ras_slope)))[,2]
  sp_Points$aspect = terra::extract(ras_aspect_crop, st_transform(st_as_sf(sp_Points), crs(ras_aspect_crop)))[,2]
  sp_Points$CI = terra::extract(ras_CI_crop, st_transform(st_as_sf(sp_Points), crs(ras_CI_crop)))[,2]

  require(shadow)
  sp_Points$northing = cos(deg2rad(sp_Points$aspect))
  sp_Points$easting = sin(deg2rad(sp_Points$aspect))
  
  print("predRast")
  
  if(!is.null(rasType_grid)){
    if(crs(rasalti30wm)!=crs(rasalti30)) stop("rasters do not have the same crs")
    predRaster= rast(c(alti = ras_mnt_crop, slope =ras_slope, CI = ras_CI_crop, northing=cos(deg2rad(ras_aspect_crop)), easting = sin(deg2rad(ras_aspect_crop)) ) )
    names(predRaster) = c("alti", "slope", "CI", "northing", "easting")
    predRaster = terra::crop(predRaster,rasType_grid, extend = TRUE)
    predRaster = terra::project(predRaster, rasType_grid, align = TRUE)
  }else predRaster = NULL
   
  return(list(pts = sp_Points, predRast = predRaster ))
}
