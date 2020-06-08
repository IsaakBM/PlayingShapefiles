library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
moll.prj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
azi.prj <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = CRS(geo.prj))

st_crs(world_sf)
plot(st_geometry(world_sf))

grid <- st_make_grid(x = world_sf, cellsize = c(0.5, 0.5), square = FALSE) %>% 
  st_sf() 

nrow(grid)
area <- st_area(grid)/1e+06
range(area, na.rm = TRUE)
hist(area)


library(dplyr)
library(RCzechia) # a package of Czech administrative areas
library(sf)

mesto <- kraje() %>% # All Czech NUTS3 ...
  filter(NAZ_CZNUTS3 == 'Hlavní město Praha') %>% # ... city of Prague
  st_transform(5514) # a metric CRS 

grid_spacing <- 1000  # size of squares, in units of the CRS (i.e. meters for 5514)

polygony <- st_make_grid(mesto, square = T, cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later

plot(polygony, col = 'white')
plot(st_geometry(mesto), add = T)

area <- st_area(polygony)/1e+06
range(area, na.rm = TRUE)
hist(area)



test <- st_read("files/abnj_02/abnj_02.shp") %>% 
  summarise(total_layer = sum(layer, do_union = TRUE))
st_write(test, dsn = "files/global-poly_abnj_01", driver = "ESRI Shapefile")


grid <- st_make_grid(x = test, cellsize = c(0.5, 0.5), square = FALSE) %>% 
  st_sf() 
st_write(grid, dsn = "files/abnj_hexa_01", driver = "ESRI Shapefile")

# plotting to see what we have :-)
theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.background = element_rect(fill = "white", colour = "black"),
                          plot.background = element_rect(fill = "white"),
                          panel.border = element_blank(),
                          axis.line = element_line(size = 1),
                          axis.text.x = element_text(size = rel(2), angle = 0),
                          axis.text.y = element_text(size = rel(2), angle = 0),
                          axis.ticks = element_line(size = 1.5),
                          axis.ticks.length = unit(.25, "cm"), 
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                          legend.title = element_text(colour = "black", face = "bold", size = 15),
                          legend.text = element_text(colour = "black", face = "bold", size = 10), 
                          legend.key.height = unit(1, "cm"),
                          legend.key.width = unit(0.8, "cm")))

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = CRS(geo.prj))

world_indian <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = CRS(proj_indian))

# ggplot() +
#   geom_sf(data = grid, size = 0.05) +
#   geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#   ggtitle("Planning unit region in ABNJ") +
#   theme_opts3 +
#   ggsave("pdfs/abnj_hexa_01.pdf", width = 20, height = 15, dpi = 300)


###### indian ocean for Rosa
# etopo <- readAll(raster("files/ETOPO1_05deg/ETOPO1_ocean.grd"))
# plot(etopo)
# drawExtent()

proj_indian <- "+proj=laea +lon_0=85.78 +lat_0=-1.63 +datum=WGS84 +units=m +no_defs"
box_indian <- c(xmin = 40.12496, ymin = -38.16116, xmax = 115.8954, ymax = 21.88332) %>%
  st_bbox()
indian <- st_read("files/global-poly_abnj_01/global-poly_abnj_01.shp") %>% 
  st_crop(box_indian) %>% 
  st_transform(crs = CRS(proj_indian))

grid_spacing <- 50000  # size of squares, in units of the CRS (i.e. meters for 5514)
pus_indian <- st_make_grid(indian, square = F, cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later
plot(st_geometry(pus_indian))
# nrow(pus_indian) # 12838 polygons... aprox 13ks area of 2.2km2

pus_indian2 <- pus_indian %>% 
  st_transform(crs = CRS(geo.prj))

pus_area <- round(st_area(pus_indian)/1e+06)
hist(pus_area)

ggplot() +
  geom_sf(data = pus_indian2, size = 0.05) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region Indian Ocean ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_indian_latlon_01.pdf", width = 20, height = 15, dpi = 300)

ggplot() +
  geom_sf(data = pus_indian, size = 0.05) +
  geom_sf(data = world_indian, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region Indian Ocean ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_indian_laea_01.pdf", width = 20, height = 15, dpi = 300)

