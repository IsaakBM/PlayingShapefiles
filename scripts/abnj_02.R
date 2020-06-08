library(sf)
library(dggridR)
library(rgdal)
library(dplyr)
library(ggplot2)
countries <- map_data("world")

test <- as.data.frame(rasterToPoints(abnj_rs2))


df    <- data.frame(lat = test$y, lon = test$x)
dggs    <- dgconstruct(area = 2500, metric = FALSE, resround = 'nearest')
df$cell <- dgGEO_to_SEQNUM(dggs,df$lon,df$lat)$seqnum
gridfilename <- dgcellstogrid(dggs,df$cell)
grid <- dgcellstogrid(dggs,df$cell, frame = TRUE, wrapcells = TRUE)

p0 <- ggplot() + 
  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
  geom_polygon(data=grid, aes(x=long, y=lat, group=group), fill="blue", alpha=0.8)    +
  geom_path   (data=grid, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  ggsave("pdfs/abnj_hexa_01.pdf", width = 40, height = 30, dpi = 300)

p0 + coord_map("ortho", orientation = c(-38.49831, -179.9223, 0))+
  xlab('')+ylab('')+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_blank()) +
  ggsave("pdfs/abnj_hexa_02.pdf", width = 40, height = 30, dpi = 300)

final <- as(grid, "SpatialPolygonsDataFrame")

xy <- grid[,c(1,2)]
final <- SpatialPointsDataFrame(coords = xy, data = group)
plot(final)
final2 <- st_as_sf(final)
plot(st_geometry(final2))

st_area(final2)

global <- dgearthgrid(dggs, frame = FALSE)
global.cell <- data.frame(cell=getSpPPolygonsIDSlots(global), row.names=getSpPPolygonsIDSlots(global))
global <- SpatialPolygonsDataFrame(global, global.cell)

for(i in 1:length(global@polygons)) {
  if(max(global@polygons[[i]]@Polygons[[1]]@coords[,1]) - 
     min(global@polygons[[i]]@Polygons[[1]]@coords[,1]) > 180) {
    global@polygons[[i]]@Polygons[[1]]@coords[,1] <- (global@polygons[[i]]@Polygons[[1]]@coords[,1] +360) %% 360
  }
}

global2 <- st_as_sf(global)
plot(st_geometry(global2))

area <- st_area(global2)/1e+06
range(area)
hist(area)

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = CRS(geo.prj))

ggplot() + 
  geom_sf(data = global2) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20")


ggplot() + 
  geom_sf(data = world_sf) +
  coord_sf(crs= "+proj=ortho +lat_0=20 +lon_0=-10")

ggplot() + 
  geom_sf(data = global2) +
  coord_sf(crs= "+proj=ortho +lat_0=20 +lon_0=-10")

