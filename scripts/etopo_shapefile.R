# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
etopo <- readAll(raster("shapefiles/ETOPO1/ETOPO1_ocean.grd"))
crs(etopo) <- CRS(geo.prj)
etopo[] <- ifelse(etopo[] > -100, NA, etopo[])


eez <- st_read("shapefiles/WorldEEZ/eez_boundaries.shp")
names(eez)
str(eez)

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = CRS(geo.prj))
world_sp <- as(world_sf, "Spatial")

pdf("pdfs/test_02.pdf", width = 38, height = 20)
plot(etopo)
plot(st_geometry(eez), add = TRUE)
plot(world_sp, add = TRUE, col = "grey")
dev.off()
