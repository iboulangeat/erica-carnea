extract_env_topo = function(sp_Points_sites, nb_bg_points = nrow(sp_Points_sites@data), ras_mnt30 , bbox, rasType_projection){
  require(raster)
  require(terra)
  require(stars)
  require(starsExtra)
  require(shadow)
  require(rgdal)
  require(sf)

  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(ras_mnt30)))
  
  print("crop rast mnt")
  
  ras_mnt_crop = crop(ras_mnt30, extent(bbox_proj))
  ras_mnt_proj = terra::project(ras_mnt_crop, rasType_projection)

  ## crop rasters

  print('calc terrain')
  
  ras_slope_crop = terrain(ras_mnt_proj, "slope", neighbors = 8, unit= "degrees")
  
  ## need projected raster for calc
  st_mnt = st_as_stars(ras_mnt_proj)
  st_aspect = starsExtra::aspect(st_mnt)
  
  convergence = starsExtra::CI(st_aspect, 3)
  
  ras_aspect_crop = rast(as(st_aspect, "Raster"))
 
  ras_CI_crop = rast(as(convergence, "Raster"))
  
  print('obs points')
  sp_Points_sites$alti = terra::extract(ras_mnt_proj, st_transform(st_as_sf(sp_Points_sites), crs(ras_mnt_proj)))[,2]
  sp_Points_sites$slope = terra::extract(ras_slope_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_slope_crop)))[,2]
  sp_Points_sites$aspect = terra::extract(ras_aspect_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_aspect_crop)))[,2]
  sp_Points_sites$CI = terra::extract(ras_CI_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_CI_crop)))[,2]

  require(shadow)
  sp_Points_sites$northing = cos(deg2rad(sp_Points_sites$aspect))
  sp_Points_sites$easting = sin(deg2rad(sp_Points_sites$aspect))
  
  print("bg points")
  ## bg points 
  nona = which(!is.na(ras_mnt_crop[]))
  bg_index= sample(nona, nb_bg_points)
  bg_coords = as.data.frame(xyFromCell(ras_mnt_crop, bg_index))
  coordinates(bg_coords)= c("x","y")
  proj4string(bg_coords) = crs(ras_mnt_crop)
    
  bg_coords$alti = terra::extract(ras_mnt_proj, st_transform(st_as_sf(bg_coords), crs(ras_mnt_proj)))[,2]
  bg_coords$slope = terra::extract(ras_slope_crop, st_transform(st_as_sf(bg_coords), crs(ras_slope_crop)))[,2]
  bg_coords$aspect = terra::extract(ras_aspect_crop, st_transform(st_as_sf(bg_coords), crs(ras_aspect_crop)))[,2]
  bg_coords$CI = terra::extract(ras_CI_crop, st_transform(st_as_sf(bg_coords), crs(ras_CI_crop)))[,2]
  
  bg_coords$northing = cos(deg2rad(bg_coords$aspect))
  bg_coords$easting = sin(deg2rad(bg_coords$aspect))
  
   
  return(list(obs = sp_Points_sites, bg = bg_coords, predRast = c(alti = ras_mnt_proj, slope =ras_slope_crop, aspect = ras_aspect_crop,CI = ras_CI_crop, northing=cos(deg2rad(ras_aspect_crop)), easting = sin(deg2rad(ras_aspect_crop)) )))
}
