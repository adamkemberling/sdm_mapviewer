# Land Mask Prep

# Goal: make a coastline shapefile for the relevant study area:


####  Packages  ####
library(tidyverse)
library(leaflet)
library(sf)
library(gmRi)
library(rnaturalearth)

# Support Functions:
source(here::here("R/app_support_functions.R"))

# 
# # 1. Loading land mask
# bbox <- c(xmin = -78.5, ymin = 34.5, xmax = -54.5, ymax = 48.25)
# land_sf <- sf::st_read(paste0(Res_Data_path, "ne_50m_land.shp")) %>% 
#   sf::st_crop(bbox)
# land_sf <- sf::st_transform(land_sf, crs = 32619)

# 2. Land Mask with country Borders
# Load countries with borders
# canada_sf <- ne_states("canada", returnclass = "sf")
# us_sf <- ne_states("united states of america", returnclass = "sf")
# north_america <- bind_rows(list(mutate(canada_sf, country = "Canada"),
#                                 mutate(us_sf, country = "United States")))
# 
# na_bbox <- c(xmin = -79, ymin = 33.5, xmax = -54, ymax = 49)
# north_cropped <- st_crop(north_america, na_bbox)
# north_projected <-  sf::st_transform(north_cropped, crs = 32619)
# ggplot(north_projected) + geom_sf(fill = "gray60", color = "white", size = 0.25) +
#   coord_sf(xlim = c(-182000, 1570000), ylim = c(3870000, 5385000), expand = F, crs = 32619) +
#   theme_map()
# 
# # Save it
# sf::write_sf(north_cropped, here::here("processed_data/spatial", "nw_atlantic_countries_crs4326.geojson"))
# sf::write_sf(north_projected, here::here("processed_data/spatial", "nw_atlantic_countries_crs32619.geojson"))
