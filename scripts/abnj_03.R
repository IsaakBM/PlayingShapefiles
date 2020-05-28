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


ggplot() +
  geom_sf(data = grid, size = 0.05) +
  ggsave("pdfs/grid_test_01.pdf", width = 20, height = 15, dpi = 300)


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
