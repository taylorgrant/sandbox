##%######################################################%##
#                                                          #
####       Mapping San Francisco Building Heights       ####
#                                                          #
##%######################################################%##

pacman::p_load(tidyverse, janitor, here, glue, sf, viridis)


# get data from DataSF ----------------------------------------------------

# building footprint and heights 
df <- read_csv("https://data.sfgov.org/api/views/ynuv-fyni/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# convert heights to feet and block into factor
df <- df %>% 
  mutate(hgt_max = hgt_maxcm*0.0328084,
         hgt_maxd = case_when(hgt_max < 100 ~ "<100",
                              hgt_max >=100 & hgt_max < 200 ~ "200",
                              hgt_max >=200 & hgt_max < 300 ~ "300",
                              hgt_max >=300 & hgt_max < 400 ~ "400",
                              hgt_max >=400 & hgt_max < 500 ~ "500",
                              hgt_max >=500 & hgt_max < 600 ~ "600",
                              hgt_max >=600 & hgt_max < 700 ~ "700",
                              hgt_max >=700 & hgt_max < 800 ~ "800",
                              hgt_max >=800 & hgt_max < 900 ~ ">900",
                              TRUE ~ ">900"),
         hgt_maxd = factor(hgt_maxd, 
                           levels = c("<100", "200","300","400",'500',
                                      "600","700","800",">900")))

# converting csv to sf object
sf_buildings <- df %>% 
  select(hgt_maxd, geometry = shape) %>% 
  st_as_sf(wkt = "geometry")

# CRS was missing, so set and transform
# if only plotting buildings this isn't an issue, but if add roads, need it
sf_buildings <- sf_buildings %>% st_set_crs(4326) %>% st_transform(crs=4326)
SF_SF <- SF_SF %>% st_transform(crs=4326)


# get roads shapefile from us census --------------------------------------
# 1. Create a temp directory and download ZIP file from US Census
temp_download <- tempfile()
download.file("https://www2.census.gov/geo/tiger/TIGER2017//ROADS/tl_2017_06075_roads.zip", temp_download)
# 2. Create a temp directory to unzip the file into
temp_unzip <- tempfile()
unzip(temp_download, exdir = temp_unzip)
# 3. Read the unzipped shapefile from the temporary filepath
sf_roads <- read_sf(paste0(file.path(temp_unzip), "/tl_2017_06075_roads.shp"))


# changing coordinates of bounding box ------------------------------------
# expanding on the upper y-axis
bbox_new <- st_bbox(sf_buildings) # current bounding box
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange)
bbox_new[4] <- bbox_new[4] + (0.25 * yrange)

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon


# plotting the map --------------------------------------------------------

# setting fonts for 
library(showtext)
font_add_google("Roboto", "roboto",
                regular.wt = 100)
font_add_google("Seaweed Script", "seaweed")
showtext_auto()

# plot
plot1 <- ggplot() + 
  # adding building heights
  geom_sf(data = sf_buildings, aes(fill = hgt_maxd, color = hgt_maxd),
          size = .05) +
  # color and fill are the same
  scale_fill_viridis(option = "A",
                     begin = .4,
                     end = 1,
                     guide = guide_legend(reverse = TRUE),
                     name = "Roof\nHeight (ft)",
                     discrete = TRUE) +
  scale_color_viridis(option = "A",
                     begin = .4,
                     end = 1,
                     guide = guide_legend(reverse = TRUE),
                     name = "Roof\nHeight (ft)",
                     discrete = TRUE) +
  # adding in roads of SF
  geom_sf(data = sf_roads, col = "white", size = .04) +
  # setting new coordinates with new bounding box
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) + # min & max of y values
  theme_void() + 
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = '#483248', color = NA),
        panel.background = element_rect(fill = '#483248', color = NA),
        legend.position = c(.92,.5),
        # fixing lineheight on break
        legend.title=element_text(size=65,
                                  lineheight = .16,
                                  family = "roboto",
                                  face = "bold",
                                  color = "#F7FFBF"),
        legend.text = element_text(size = 65,
                                   family = "roboto",
                                   color = "#F7FFBF"),
        legend.spacing.x = unit(0.3, 'cm'),
        # signature line gets script font
        plot.caption = element_text(size = 70,
                                    family = "seaweed",
                                    color = "#F7FFBF",
                                    vjust = -10,
                                    hjust = .97)
        ) + 
  guides(colour=guide_legend(title.vjust = -9),
         fill=guide_legend(title.vjust = -9)) +
  annotate("text", x = -122.46, y = 37.855, label = "Building Footprints & Heights",
           size = 65, 
           color = "#F7FFBF",
           family = "roboto",
           fontface = "bold") +
  annotate("text", x = -122.46, y = 37.8632, label = "San Francisco",
           size = 140, 
           color = "#F7FFBF",
           family = "roboto") +
  labs(caption = "Taylor Grant")

# save with large size and high dpi for print quality
ggsave(plot1, 
       filename = "SF_building_map.png",
       scale = 1, 
       width = 12, 
       height = 18, 
       units = "in",
       dpi = 500)


