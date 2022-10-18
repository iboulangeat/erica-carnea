extract_env_topo = function(sp_Points_sites, nb_bg_points = nrow(sp_Points_sites@data), pathDD , bbox){
  require(terra)
  require(stars)
  require(starsExtra)
  require(shadow)
  require(rgdal)
  
  st_mnt25 <- read_stars(paste0(pathDD, "zaa_mnt/mntAlpes_25m.tif"), proxy = FALSE)
  ras_mnt25 = rast(as(st_mnt25, "Raster"))
  ras_lf = rast(paste0(pathDD, "zaa_landform/landform_tpi_saga_juill2020.tif"))
  ras_ray = rast(paste0(pathDD, "zaa_Rayonnement Alpes(LERFOB)/rayonalpesl93"))
  
  ras_slope = terrain(ras_mnt25, "slope", neighbors = 8, unit= "degrees")
  st_aspect = starsExtra::aspect(st_mnt25)
  
  convergence = starsExtra::CI(st_aspect, 3)
  
  ras_aspect = rast(as(st_aspect, "Raster"))
  ras_CI = rast(as(convergence, "Raster"))
  
  
  sp_Points_sites$alti = extract(ras_mnt25, st_transform(st_as_sf(sp_Points_sites), crs(ras_mnt25)))[,2]
  sp_Points_sites$slope = extract(ras_slope, st_transform(st_as_sf(sp_Points_sites), crs(ras_slope)))[,2]
  sp_Points_sites$lf = extract(ras_lf, st_transform(st_as_sf(sp_Points_sites), crs(ras_lf)))[,2]
  sp_Points_sites$aspect = extract(ras_aspect, st_transform(st_as_sf(sp_Points_sites), crs(ras_aspect)))[,2]
  sp_Points_sites$CI = extract(ras_CI, st_transform(st_as_sf(sp_Points_sites), crs(ras_CI)))[,2]
  sp_Points_sites$ray = extract(ras_ray, st_transform(st_as_sf(sp_Points_sites), crs(ras_ray)))[,2]
 
  require(shadow)
  sp_Points_sites$northing = cos(deg2rad(sp_Points_sites$aspect))
  sp_Points_sites$easting = sin(deg2rad(sp_Points_sites$aspect))
  
  ## bg points 
  nona = which(!is.na(ras_mnt25[]))
  bg_index= sample(nona, nb_bg_points)
  bg_coords = as.data.frame(xyFromCell(ras_mnt25, bg_index))
  coordinates(bg_coords)= c("x","y")
  proj4string(bg_coords) = crs(ras_mnt25)
    
  bg_coords$alti = extract(ras_mnt25, st_transform(st_as_sf(bg_coords), crs(ras_mnt25)))[,2]
  bg_coords$slope = extract(ras_slope, st_transform(st_as_sf(bg_coords), crs(ras_slope)))[,2]
  bg_coords$lf = extract(ras_lf, st_transform(st_as_sf(bg_coords), crs(ras_lf)))[,2]
  bg_coords$aspect = extract(ras_aspect, st_transform(st_as_sf(bg_coords), crs(ras_aspect)))[,2]
  bg_coords$CI = extract(ras_CI, st_transform(st_as_sf(bg_coords), crs(ras_CI)))[,2]
  bg_coords$ray = extract(ras_ray, st_transform(st_as_sf(bg_coords), crs(ras_ray)))[,2]
  
  
  bg_coords$northing = cos(deg2rad(bg_coords$aspect))
  bg_coords$easting = sin(deg2rad(bg_coords$aspect))
  
  ## crop rasters
  ras_ray_proj = terra::project(ras_ray,ras_mnt25, align = TRUE, mask = TRUE)
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(ras_mnt25)))
  ras_ray_crop = crop(ras_ray_proj, extent(bbox_proj))
  ras_mnt25_crop = crop(ras_mnt25, extent(bbox_proj))
  ras_slope_crop = crop(ras_slope, extent(bbox_proj))
  ras_aspect_crop = crop(ras_aspect, extent(bbox_proj))
  ras_CI_crop = crop(ras_CI, extent(bbox_proj))
   
  return(list(obs = sp_Points_sites, bg = bg_coords, predRast = c(alti = ras_mnt25_crop, slope =ras_slope_crop , aspect = ras_aspect_crop,CI = ras_CI_crop,ray = ras_ray_crop, northing=cos(deg2rad(ras_aspect_crop)), easting = sin(deg2rad(ras_aspect_crop)) )))
}
