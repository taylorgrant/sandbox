##%######################################################%##
#                                                          #
####   Mapping Rivers and Water in Washington State     ####
#                                                          #
##%######################################################%##

pacman::p_load(tidyverse, janitor, here, glue, httr, sf)

#
# url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
#   
# res <- GET(url,
#            write_disk("na_rivers.zip"),
#            progress())
# unzip("na_rivers.zip")
filenames <- list.files("HydroRIVERS_v10_na_shp", pattern="*.shp", full.names=T)
riv_list <- lapply(filenames, st_read)

# state boundaries 
url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
res <- GET(url,
           write_disk("state_boundaries.zip"),
           progress())
unzip("state_boundaries.zip")
state_sf <- st_read("~/Downloads/cb_2021_53_bg_500k/cb_2021_53_bg_500k.shp")


na_riv <- riv_list[[1]] %>% 
  st_cast("MULTILINESTRING") %>%
  mutate(
    width = as.numeric(ORD_FLOW),
    width = case_when(
      width == 3 ~ 1.1,
      width == 4 ~ 0.9,
      width == 5 ~ 0.7,
      width == 6 ~ 0.5,
      width == 7 ~ 0.3,
      width == 8 ~ 0.3,
      width == 9 ~ 0.2,
      width == 10 ~ 0.1,
      TRUE ~ 0
    )
  ) %>%
  st_as_sf()

na_riv$geometry <- na_riv$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

# bounding box coords - https://observablehq.com/@rdmurphy/u-s-state-bounding-boxes
bbox_new <- st_bbox(na_riv)
bbox_new[1] <- -124.733643
bbox_new[2] <- 45.543831
bbox_new[3] <- -116.916161
bbox_new[4] <- 49.002405
bbox_new <- bbox_new %>% 
  st_as_sfc()

# 
# remotes::install_github("hrbrmstr/albersusa")
library(albersusa)

# 
pacman::p_load("elevatr", "terra", "giscoR", "marmap")
# crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

country_sf <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "01", # can change resolution as necessary
  country = c("United States","Canada"))


#
# country_elevation <- get_elev_raster(
#   locations = country_transformed, 
#   z = 9, 
#   clip = "locations",
#   override_size_check = TRUE) 

# country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
#   na.omit()
# colnames(country_elevation_df)[3] <- "elevation"
# head(country_elevation_df)

# altitudes 
library(raster)
alt.raster.w <- getData("SRTM", lat = 47.75, lon = -122.74, download = TRUE)
alt.raster.e <- getData("SRTM", lat = 47.75, lon = -116.74, download = TRUE)
alt.raster.nw <- getData("SRTM", lat = 49 ,lon = -124.5, download = TRUE)

# crop # (not using the crop)
# alt.raster.w <- crop(alt.raster.w, as(bbox_new, 'Spatial'), snap='near')
# alt.raster.e <- crop(alt.raster.e, as(bbox_new, 'Spatial'), snap='near')
# alt.raster.nw <- crop(alt.raster.nw, as(bbox_new, 'Spatial'), snap='near')
# alt.raster.sw <- crop(alt.raster.sw, as(bbox_new, 'Spatial'), snap='near')

# if want to keep terrain colors
alt.m.w  <-  rasterToPoints(alt.raster.w)
alt.m.e  <-  rasterToPoints(alt.raster.e)
alt.df.w <- data.frame(alt.m.w)
alt.df.e <- data.frame(alt.m.e)
colnames(alt.df.w) = c("lon", "lat", "alt")
colnames(alt.df.e) = c("lon", "lat", "alt")

# sets to hill slope 
slope.raster.w <- terrain(alt.raster.w, opt='slope')
slope.raster.e <- terrain(alt.raster.e, opt='slope')
slope.raster.nw <- terrain(alt.raster.nw, opt='slope')
#
aspect.raster.w <- terrain(alt.raster.w, opt='aspect')
aspect.raster.e <- terrain(alt.raster.e, opt='aspect')
aspect.raster.nw <- terrain(alt.raster.nw, opt='aspect')
#
hill.raster.w <- hillShade(slope.raster.w, aspect.raster.w, 40, 270)
hill.raster.e <- hillShade(slope.raster.e, aspect.raster.e, 40, 270)
hill.raster.nw <- hillShade(slope.raster.nw, aspect.raster.nw, 40, 270)
#
hill.m.w <- rasterToPoints(hill.raster.w)
hill.m.e <- rasterToPoints(hill.raster.e)
hill.m.nw <- rasterToPoints(hill.raster.nw)
#
hill.df.w <-  data.frame(hill.m.w)
hill.df.e <-  data.frame(hill.m.e)
hill.df.nw <-  data.frame(hill.m.nw)
#
colnames(hill.df.w) <- c("lon", "lat", "hill")
colnames(hill.df.e) <- c("lon", "lat", "hill")
colnames(hill.df.nw) <- c("lon", "lat", "hill")

# lakes 
library(osmdata)
osm_lakes.sf <- 
  opq(bbox = st_bbox(bbox_new)) %>%
  add_osm_feature(key = 'water', value = 'lake') %>%
  osmdata_sf()
osm_lakes.sf <- osm_lakes.sf$osm_multipolygons

# set altitude
p <- ggplot() +
  geom_raster(data = hill.df.w, aes(lon, lat, fill = hill), alpha = .95) +
  geom_raster(data = hill.df.e, aes(lon, lat, fill = hill), alpha = .95) +
  scale_fill_gradientn(colours = grey.colors(100)) 

# pb <- ggplot() +
#   geom_raster(data = alt.df.w, aes(lon, lat, fill = alt), alpha = .35) +
#   geom_raster(data = alt.df.e, aes(lon, lat, fill = alt), alpha = .35) +
#   scale_fill_gradientn(colours = terrain.colors(30))

# rivers & lakes
p2 <- p + 
  geom_sf(data=na_riv, aes(color = factor(ORD_FLOW), size=width)) +
  geom_sf(data = osm_lakes.sf, fill = '#9ecae1', colour = NA) +
  scale_color_manual(
    name = "",
    values = c("#08306b", "#08519c", "#2171b5", "#4292c6", 
               "#6baed6", "#9ecae1", 
               "#c6dbef", "#deebf7", "#f7fbff")) +
  scale_size(range=c(0, .3)) +
  scale_alpha_manual(values=c("3" = 1, "4" = 1, "5" = .7, "6" = .6, 
                              "7" = .4, "8" = .3, "9" = .2, "10" = .1))
# boundaries
p3 <- p2 +
  geom_sf(data = country_sf, fill = NA, size = .3, color = "#66774d") +
  geom_sf(data = usa_sf(), fill = NA, size = .3, color = "#66774d") +
  coord_sf(
           xlim = st_coordinates(bbox_new)[c(1,2), 1],
           ylim = st_coordinates(bbox_new)[c(2,4), 2]) +
  # theme_void() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines")
        ) +
  labs(title = "Rivers of Washington")

ggsave(p3, 
       filename = "WA_river_map.png",
       scale = 1, 
       width = 18, 
       height = 12, 
       units = "in",
       dpi = 500)



