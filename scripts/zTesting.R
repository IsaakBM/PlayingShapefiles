library(sf)
library(terra)
library(sp)
library(raster)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyterra)

a <- st_read("files/PacificCenterLand/PacificCenterLand.shp")
plot(st_geometry(a))

robin <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rs <- terra::rast("files/dt_global_allsat_madt_fsle_1994-01.tif")
plot(rs, 1)
rs2 <- subset(rs, 1)
# rs2 <- rotate(rs2)
rs4 <- terra::project(rs2, y = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(rs4)

p1 <- ggplot() +
  geom_spatraster(data = rs5) +
  geom_sf(data = a, size = 0.05, fill = "grey20") +
  scale_fill_distiller(palette = "RdYlBu",
                       direction = -1,
                       oob = scales::squish,
                       guide = guide_colourbar(title.position = "top", title = "FSLE"), 
                       na.value = "white") +
  theme_opts3 +
  theme(legend.position = "none")

p2 <- ggplot() +
  geom_spatraster(data = rs5) +
  geom_sf(data = a, size = 0.05, fill = "grey20") +
  scale_fill_distiller(palette = "RdYlBu",
                       direction = -1,
                       limits = c(0, 0.5),
                       oob = scales::squish,
                       guide = guide_colourbar(title.position = "top", title = "FSLE"), 
                       na.value = "white") +
  theme_opts3 +
  theme(legend.position = "none")

  ggsave("pdfs/FSLE_global_all_v01.png", plot = p1, width = 20, height = 15, dpi = 600, limitsize = FALSE)
  ggsave("pdfs/FSLE_global_cutoff_v02.png", plot = p2, width = 20, height = 15, dpi = 600, limitsize = FALSE)

  # theme(plot.title = element_text(face = "plain", size = 20, hjust = 0.5),
  #       plot.tag = element_text(colour = "black", face = "bold", size = 23), 
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_text(size = rel(1.5), angle = 0),
  #       axis.text.x = element_text(size = rel(2), angle = 0),
  #       axis.text.y = element_text(size = rel(2), angle = 0),
  #       legend.title = element_text(colour = "black", face = "bold", size = 15),
  #       legend.text = element_text(colour = "black", face = "bold", size = 13),
  #       legend.key.height = unit(1.5, "cm"),
  #       legend.key.width = unit(1.5, "cm"))


theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.background = element_blank(),
                          plot.background = element_rect(fill = "white"),
                          panel.border = element_blank(),
                          axis.line = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(.25, "cm"),
                          axis.title.x = element_blank(),
                          axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                          plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                          legend.title = element_text(colour = "black", face = "bold", size = 25),
                          legend.text = element_text(colour = "black", face = "bold", size = 20),
                          legend.key.height = unit(2.5, "cm"),
                          legend.key.width = unit(1.4, "cm"),
                          plot.tag = element_text(size = 32, face = "bold")))

rs5 <- rs4
rs5 <- rs5*-1
rs5 <- log10(rs5+1)
rs5[] <- ifelse(rs5[] == 0, NA, rs5[])


c <- terra::as.polygons(rs4)
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
test <- terra::crds(rs2, df = TRUE, na.rm = FALSE) %>% 
  as_tibble() %>% 
  dplyr::mutate(var1 = rs2[]) %>% 
  as.data.frame()
rs_richness <- rasterFromXYZ(test)
plot(rs_richness)
rs_richness_final <- resample(rs_richness, rs, resample = "ngb")
rs1_richness <- as(rs_richness, "SpatialPolygonsDataFrame")

test2 <- as(test, "SpatVector")
test2 <- as(test, "SpatialPolygonsDataFrame")
richness_robinson <- test %>% 
  st_as_sf() %>% 
  st_difference(polygon) %>% 
  st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

plot(st_geometry(richness_robinson))



