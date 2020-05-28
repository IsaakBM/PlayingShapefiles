# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
etopo <- readAll(raster("files/ETOPO1/ETOPO1_ocean.grd"))
crs(etopo) <- CRS(geo.prj)
etopo[] <- ifelse(etopo[] >= -100, NA, etopo[])

a <- sum(!is.na(etopo[]))
b <- sum(is.na(etopo[]))
c <- a+b


eez <- st_read("files/World_EEZ_v11_20191118/eez_boundaries_v11.shp")
eez_sp <- as(eez, "Spatial")

test <- as.character(unique(eez_sp@data$TERRITORY1))
length(unique(eez_sp@data$LINE_ID))

f1 <- eez %>% filter(TERRITORY1 == test[1])
f1_sp <- as(f1, "Spatial")

f3 <- extract(etopo, f1_sp, df = TRUE)
f4 <- data.frame(eez = test[1], ncells = nrow(na.omit(f3)))
f4$eez <- test[1]

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = CRS(geo.prj))
world_sp <- as(world_sf, "Spatial")

pdf("pdfs/test_02.pdf", width = 38, height = 20)
plot(etopo)
plot(st_geometry(eez), add = TRUE)
plot(world_sp, add = TRUE, col = "grey")
dev.off()

eez <- eez$Line_ID
test <- eez %>% filter(Line_name == 123)

countries <- as.character(unique(eez_sp@data$TERRITORY1))







