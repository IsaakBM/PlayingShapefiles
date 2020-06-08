library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

##### Create a global polygon for ABNJ 
# global <- st_read("files/abnj_02/abnj_02.shp") %>% 
#   summarise(total_layer = sum(layer, do_union = TRUE))
# st_write(global, dsn = "files/global-poly_abnj_01", driver = "ESRI Shapefile")

##### indian ocean for Rosa
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
st_write(pus_indian, dsn = "files/indian-poly_abnj_01", driver = "ESRI Shapefile")

pus_indian2 <- pus_indian %>% 
  st_transform(crs = CRS(geo.prj))

pus_area <- round(st_area(pus_indian)/1e+06)
range(pus_area)
hist(pus_area)



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

