library(sf)
library(terra)
library(sp)
library(raster)
library(dplyr)
library(readr)

a <- st_read("files/PacificCenterLand/PacificCenterLand.shp")
plot(st_geometry(a))

robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
rs <- terra::rast("files/dt_global_allsat_madt_fsle_1994-01.tif")
plot(rs, 1)
rs2 <- subset(rs, 1)
rs2 <- rotate(rs2)
plot(rs2)

a <- raster(rs2)
rs3 <- raster::projectRaster(a, crs = CRS("+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"), method = "ngb", over = FALSE)
plot(rs3)

rs4 <- terra::project(rs2, y = robin)
plot(rs4)

c <- terra::as.polygons(rs)
c <- st_as_sf(c)





# Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

test <- as(rs2, "SpatVector")
test <- terra::crds(rs2) %>% 
  as_tibble()

nrow(test)  
rs2[]

test <- as(rs2, "SpatialPolygonsDataFrame")
richness_robinson <- test %>% 
  st_as_sf() %>% 
  st_difference(polygon) %>% 
  st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

plot(st_geometry(richness_robinson))
