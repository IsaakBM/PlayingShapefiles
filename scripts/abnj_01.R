library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

# using land mask for nature earth package
land <- st_read("files/ne_10m_land/ne_10m_land.shp")
land_sp <- as(land, "Spatial") # to an sp object
# creating a empty raster at 0.5° resolution
rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
crs(rs) <- CRS(geo.prj)
# rasterize the land sp object  
land_rs <- rasterize(land_sp, rs)
land_rs[] <- ifelse(is.na(land_rs[]), 1, NA) # considering only ocean cells
land_rs <- setValues(raster(land_rs), land_rs[])

# Reading EEZ
eez <- st_read("files/World_EEZ_v11_20191118/eez_v11.shp") %>% 
  filter(SOVEREIGN1 != "Antarctica")
# head(eez)
eez_sp <- as(eez, "Spatial")
# Creating the final raster
abnj_rs <- mask(land_rs, eez_sp, inverse = TRUE)
plot(abnj_rs)


abnj_clump <- clump(abnj_rs, directions = 8) 
# get frequency table    
df_clump <- freq(abnj_clump) %>% 
  as.data.frame()
  # which rows of the data.frame are only represented by clumps under 9 pixels?
  str(which(df_clump$count <= 9))
  # which values do these correspond to?
  str(df_clump$value[which(df_clump$count <= 9)])
# put these into a vector of clump ID's to be removed
excludeID <- df_clump$value[which(df_clump$count <= 9)]
# make a new raster to be sieved
abnj_rs2 <- abnj_clump
# assign NA to all clumps whose IDs are found in excludeID
abnj_rs2[abnj_rs2 %in% excludeID] <- NA
plot(abnj_rs2)
writeRaster(abnj_rs2, "files/abnj.tif")


# 
abnj_pol <- as(abnj_rs2,  "SpatialPolygonsDataFrame")
abnj_pol$layer <- seq(1, length(abnj_pol))
abnj_pol <- spTransform(abnj_pol, CRS(geo.prj))

abnj_pol_sf <- st_as_sf(abnj_pol) %>% select(layer)
st_write(abnj_pol_sf, dsn = "files/abnj_02", driver = "ESRI Shapefile")


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

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(crs = CRS(geo.prj))

ggplot() +
  geom_sf(data = abnj_pol_sf, size = 0.05) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region in ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_02.pdf", width = 20, height = 15, dpi = 300)





