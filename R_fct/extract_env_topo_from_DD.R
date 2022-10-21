extract_env_topo = function(sp_Points_sites, nb_bg_points = nrow(sp_Points_sites@data), pathDD , bbox){
  require(raster)
  require(terra)
  require(stars)
  require(starsExtra)
  require(shadow)
  require(rgdal)
  require(sf)


  print('load rast mnt')
  # "/Volumes/ISA-RESEARCH/_DATA/eu_topography"
  bbox_proj = st_bbox(st_transform(st_as_sfc(bbox), crs(rasType)))
  
  print("crop rast mnt")
  ras_mnt25 = rast(paste0(pathDD, "zaa_mnt/mntAlpes_25m.tif"))
  ras_lf = rast(paste0(pathDD, "zaa_landform/landform_tpi_saga_juill2020.tif"))
  ras_ray = rast(paste0(pathDD, "zaa_Rayonnement Alpes(LERFOB)/rayonalpesl93"))
  
  print('aggregate and crop')
  
  # ras_mnt_type = terra::project(ras_mnt25, rasType, method = "bilinear", align = TRUE)
  # ras_mnt_crop = crop(ras_mnt_type, rasType)
  ras_mnt_100m = aggregate(ras_mnt25, fac = 4)
  rasType = ras_mnt_crop = ras_mnt_100m
  ras_lf_type = terra::project(ras_mnt25, rasType, method = "near", align = TRUE)
  ras_lf_crop = crop(ras_lf_type,  rasType)

  ## crop rasters
  ras_ray_type = terra::project(ras_ray, rasType, method = "bilinear", align = TRUE, mask = TRUE)
  ras_ray_crop = crop(ras_ray_type, rasType)
  ras_ray_crop = resample(ras_ray_crop, ras_mnt_crop)
  print('calc terrain')
  
  ras_slope_crop = terrain(ras_mnt_crop, "slope", neighbors = 8, unit= "degrees")
  
  ## need projected raster for calc
  #as_mnt_100 = aggregate(ras_mnt25, fac = 4)
  st_mnt = st_as_stars(ras_mnt_100m)
  st_aspect = starsExtra::aspect(st_mnt)
  
  convergence = starsExtra::CI(st_aspect, 3)
  
  ras_aspect_type = terra::project(rast(as(st_aspect, "Raster")), rasType, method = "bilinear", align = TRUE)
  ras_aspect_crop = crop(ras_aspect_type, rasType)
  
  ras_CI_type = terra::project(rast(as(convergence, "Raster")), rasType, method = "bilinear", align = TRUE)
  ras_CI_crop = crop(ras_CI_type, rasType)
  
  print('obs points')
  sp_Points_sites$alti = extract(ras_mnt_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_mnt_crop)))[,2]
  sp_Points_sites$slope = extract(ras_slope_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_slope_crop)))[,2]
  sp_Points_sites$lf = extract(ras_lf_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_lf_crop)))[,2]
  sp_Points_sites$aspect = extract(ras_aspect_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_aspect_crop)))[,2]
  sp_Points_sites$CI = extract(ras_CI_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_CI_crop)))[,2]
  sp_Points_sites$ray = extract(ras_ray_crop, st_transform(st_as_sf(sp_Points_sites), crs(ras_ray_crop)))[,2]
 
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
    
  bg_coords$alti = extract(ras_mnt_crop, st_transform(st_as_sf(bg_coords), crs(ras_mnt_crop)))[,2]
  bg_coords$slope = extract(ras_slope_crop, st_transform(st_as_sf(bg_coords), crs(ras_slope_crop)))[,2]
  bg_coords$lf = extract(ras_lf_crop, st_transform(st_as_sf(bg_coords), crs(ras_lf_crop)))[,2]
  bg_coords$aspect = extract(ras_aspect_crop, st_transform(st_as_sf(bg_coords), crs(ras_aspect_crop)))[,2]
  bg_coords$CI = extract(ras_CI_crop, st_transform(st_as_sf(bg_coords), crs(ras_CI_crop)))[,2]
  bg_coords$ray = extract(ras_ray_crop, st_transform(st_as_sf(bg_coords), crs(ras_ray_crop)))[,2]
  
  
  bg_coords$northing = cos(deg2rad(bg_coords$aspect))
  bg_coords$easting = sin(deg2rad(bg_coords$aspect))
  
   
   
  return(list(obs = sp_Points_sites, bg = bg_coords, predRast = c(alti = ras_mnt_crop, lf = ras_lf_crop, slope =ras_slope_crop , aspect = ras_aspect_crop,CI = ras_CI_crop,ray = ras_ray_crop, northing=cos(deg2rad(ras_aspect_crop)), easting = sin(deg2rad(ras_aspect_crop)) )))
}
