
# plot results large raster image
##-----------------------------------                               
library(leaflet)
library(leafem)
library(stars)
sites_all = read.csv("_data_prod/sites_all.csv")

name = "_all"
name = "_fr"


## PA
rf.predict.ericar= read_stars(paste0("_data_prod/rf.predict.ericar_pa",name,".tif"), proxy = FALSE,  package="stars")
plot(rf.predict.ericar)
rf.values = raster(paste0("_data_prod/rf.predict.ericar_pa", name, ".tif"))
rf.values = rf.values-1
summary(rf.values[])

pcol <-c("#FF000000", "#ff9988", "#007fff")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  leafem::addGeoRaster(rf.predict.ericar, opacity = 0.6, colorOptions = colorOptions(palette = pcol)) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "black")

## proba
rf.predict.ericar.p= read_stars("_data_prod/rf.predict.ericar_prob.tif", proxy = FALSE,  package="stars")
plot(rf.predict.ericar.p)
rf.values.p = raster("_data_prod/rf.predict.ericar_prob.tif")

leaflet(sites_all)%>%
  addProviderTiles("OpenStreetMap.HOT")%>%
  leafem::addGeoRaster(rf.predict.ericar.p, opacity = 0.8, colorOptions = colorOptions(palette = "Blues")) %>%
  setView(lng=5.5,lat=45,zoom=6) %>%
  addCircleMarkers(lng = ~lon_wgs84, lat = ~lat_wgs84, popup = ~date, radius = 0.3, opacity = 0.8, color = "black") 

