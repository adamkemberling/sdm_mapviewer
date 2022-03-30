# Data Pre-Processing: sdm web app data


# Goal:
# Map out the visuals step-by-step using one dataset as an example:


####  Packages  ####
library(tidyverse)
library(sf)
library(rnaturalearth)
library(zoo)

# Support Functions:
source(here::here("R/app_support_functions.R"))




####  Paths  ####


# # Path to shiny app on Box
# sdmShiny_path <- gmRi::cs_path(box_group = "Mills lab", 
#                                subfolder = "Projects/sdm_workflow/targets_output/shiny")
# 
# # Path to Res_Data folder on Box
# Res_Data_path <- gmRi::cs_path(box_group = "Res_Data",
#                                subfolder = "Shapefiles/ne_50m_land/")






####  Data  ####



# Read the cropped land coverage:
land_sf <- read_sf(here::here("Data/spatial", "nw_atlantic_countries_crs32619.geojson"))
land_wgs <- read_sf(here::here("Data/spatial", "nw_atlantic_countries_crs4326.geojson"))




#### 1. Baseline Data  ####

# Read in baselines

# the .csv files
baseline_dataList <- fetch_boxdata("baseline_data") %>% 
  map(~data.table::fread(.x, stringsAsFactors = FALSE)) 


# Pull one
baseline_dat <- baseline_dataList$American_lobster_ST_SSP5_85_mean_baseline.csv

# check it
glimpse(baseline_dat)


# Run prep function
meters_bsquare <- baseline_prep(baseline_dat)


# Map one of them:
ggplot() +
  geom_sf(data = meters_bsquare, aes(fill = Log_Biomass, color = Log_Biomass)) +
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "viridis", na.value = "transparent") +  #, limits = plot_lims()) +
  scale_color_viridis_c(option = "viridis", na.value = "transparent") +  #, limits = plot_lims()) +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
  labs(title = "LobsterSSP585 Mean Baseline",
       subtitle = "bquare - geom_sf", fill = "Log( Biomass )", color = "Log( Biomass )") +
  theme_map()



# Export List
baseline_prepped <- map(baseline_dataList, baseline_prep)



# check it
iwalk(baseline_prepped, export_app_data, folder = "baseline_data", fextension = ".geojson", write = FALSE)


# Save them
iwalk(baseline_prepped, export_app_data, folder = "baseline_data", fextension = ".geojson", write = TRUE)




####________####

#### 2. Projected Data  ####
dataList <- fetch_boxdata("projected_data") %>% 
  map(~data.table::fread(.x, stringsAsFactors = FALSE))

# Pull one as an example:
proj_dat <- dataList$American_lobster_ST_SSP5_85_mean_projected.csv


# check it
glimpse(proj_dat)



# Run prep function
proj_dat <- projection_prep(proj_dat = proj_dat)


# Set inputs to follow user selections:
input <- list(species = "American Lobster",
              decade = 2050, 
              season = "Spring",
              ssp = "SSP585")


# Some Filtering Would Occur:
proj_dat_f <- proj_dat %>% 
  filter(Decade == input$decade,
         Season == input$season)


# Then we can map it
ggplot() +
  geom_sf(data = proj_dat_f, aes(fill = Log_Biomass, color = Log_Biomass)) +
  geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "viridis", na.value = "transparent") +  #, limits = plot_lims()) +
  scale_color_viridis_c(option = "viridis", na.value = "transparent") +  #, limits = plot_lims()) +
  coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
  labs(title = paste(input$species, " : ", input$ssp, " ", input$season, " ", input$decade, "'s", sep = ""), 
       fill = "Log( Biomass )", 
       color = "Log( Biomass )") +
  theme_map()




# Export List
projected_prepped <- map(dataList, projection_prep)



# check it
iwalk(projected_prepped, export_app_data, folder = "projected_data", fextension = ".geojson", write = FALSE)


# Save them
iwalk(projected_prepped, export_app_data, folder = "projected_data", fextension = ".geojson", write = TRUE)











####________####
#### 3. Annual Summaries  ####
annual_dataList <- fetch_boxdata("spatial_summary")%>% 
  map(~ data.table::fread(.x, stringsAsFactors = FALSE))


# prep for display
spat_summ_df <- annual_dataList$American_lobster_ST_SSP5_85_mean_Biomass_Index.csv


# Check it out:
glimpse(spat_summ_df)



#run prep function
spat_summ_df <- spat_summ_prep(spat_summ_df = spat_summ_df)


#Input filtering:
spat_summ_df <- spat_summ_df %>% 
  filter(Season == input$season)



# Plot config:
plot_regions <- unique(spat_summ_df$Region)
color_pal <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
colors_use <- color_pal[1:length(plot_regions)]



# Works, but will probably want to filter to seasons
ggplot(spat_summ_df) +
  geom_point(aes(x = Year, y = Log_Biomass, color = Region), alpha = 0.25) +
  geom_line(data = spat_summ_df, aes(Year, y = mean_5yr, color = Region, linetype = "5 Year Average"), size = 0.8) +
  scale_color_manual(values = colors_use, na.translate = F) +
  labs(title = paste(input$species, input$ssp, "Biomass", sep = " "),
       x = NULL,
       y = "Log( Biomass ) (kg)",
       linetype = "Trendline") + 
  theme_bw() +
  facet_wrap(~Season, ncol = 1) +
  theme_plot()





# Export List
annual_prepped <- map(annual_dataList, spat_summ_prep)



# check it
iwalk(annual_prepped, export_app_data, folder = "spatial_summary", fextension = ".csv", write = FALSE)


# Save them
iwalk(annual_prepped, export_app_data, folder = "spatial_summary", fextension = ".csv", write = TRUE)








####________####
####  4. Center of Biomass  ####

# Center of biomass data
center_dataList <- fetch_boxdata("center_biomass") %>%  
  map(~data.table::fread(.x, stringsAsFactors = FALSE))


# Pull one
center_bio <- center_dataList$American_lobster_ST_SSP5_85_mean_cog.csv


# Check it
glimpse(center_bio)


# Input filters:
center_bio <- center_bio %>% 
  filter(Season == input$season) %>% 
  mutate(Year = as.numeric(Year),
         decade = floor_decade(year_vector = Year))

# Plot it - Lat
ggplot(center_bio) +
  geom_line(aes(Year, Lat)) +
  labs(x = NULL,
       y = "Center of Biomass (Latitude)",
       title = str_c(input$species, " Center of Biomass")) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


# Plot it - Lat
ggplot(center_bio) +
  geom_line(aes(Year, Lon)) +
  labs(x = NULL,
       y = "Center of Biomass (Longitude)",
       title = str_c(input$species, " Center of Biomass")) + 
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 


# Play with the map:
center_bio %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
   #geom_sf(aes(color = decade)) +
   scale_alpha_continuous(trans = "reverse") +
   geom_sf(data = land_wgs, fill = "gray50", color = "white", size = 0.15) +
   ggforce::geom_mark_ellipse(data = center_bio, aes(Lon, Lat, color = decade)) +
   coord_sf(xlim = range(center_bio$Lon)+ c(-2,2), ylim = range(center_bio$Lat) + c(-1,1), expand = T, crs = 4326) +
   # coord_sf(xlim = c(-150500, 1450000), ylim = c(4355000, 5200000), expand = F, crs = 32619) +
   theme_map() +
   labs(title = "American Lobster Center of Biomass") +
   guides(
     alpha = guide_legend(
       title.position = "top",
       title.hjust = 0.5,
       direction = "horizontal",
       frame.colour = "black",
       ticks.colour = "black"))

  

# Idea: make it in plotly to show/hide decades


# # Animate it?
# library(gganimate)
# ggplot(center_bio, aes(x = Lon, y = Lat)) +
#   geom_point() +
#   gganimate::transition_reveal(Year)


# # Animate sf? - no.
# p <- center_bio %>% 
#   st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>% 
#   ggplot() +
#   #geom_line(aes(Lon, Lat, alpha = Year)) +
#   geom_point(aes(Lon, Lat, alpha = Year)) +
#   scale_alpha_continuous(trans = "reverse") +
#   geom_sf(data = land_wgs, fill = "gray50", color = "white", size = 0.15) +
#   coord_sf(xlim = c(-76, -56), ylim = c(35.5, 47.5), expand = FALSE, crs = 4326) +
#   theme_map() +
#   guides(
#     alpha = guide_legend(
#       title.position = "top",
#       title.hjust = 0.5, 
#       direction = "horizontal",
#       frame.colour = "black", 
#       ticks.colour = "black")) +
#   labs(title = str_c(input$species, " : Center of Biomass")) +
#   gganimate::transition_reveal(Year) +
#   shadow_wake(wake_length = 0.1,
#               alpha = 0.25) 
#   
# 
# animate(p, fps = 2)





# Export List

# check it
iwalk(center_dataList, export_app_data, folder = "center_biomass", fextension = ".csv", write = FALSE)


# Save them
iwalk(center_dataList, export_app_data, folder = "center_biomass", fextension = ".csv", write = TRUE)



####_________####



####  Mapping Scratch  ####




# # Using geom_tile in meter crs
# ggplot() +
#   geom_tile(data = meters_data, 
#             aes(Lon, Lat, fill = Log_Biomass), color = "transparent" ,  
#             width = 25000, height = 25000) +
#   scale_fill_viridis_c(option = "viridis", na.value = "transparent") +  #, limits = plot_lims()) +
#   geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
#   labs(title = "lat/lon in meters - geom_tile", fill = "Log( Biomass )") +
#   theme_map()



# # Plotting Lat/Lon data
# 
# # pull a table that is in lat/lon
# decdeg_dat <- baseline_dataList$American_lobster_SSP585_mean_baseline.csv
# base_sf <- st_as_sf(decdeg_dat, coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) 
# 
# ggplot(base_sf) + 
#   geom_sf(aes(color=Log_Biomass, geometry=geometry), fill = "transparent", shape = 15) +
#   scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent") + #, limits = plot_lims()) +
#   geom_sf(data = st_transform(land_sf, crs = 4326), fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-76, -56), ylim = c(35.5, 47.5), expand = FALSE, crs = 4326) +
#   labs(title = "Baseline",) +
#   theme_map()
# 
# 
# # Plotting in projected coords as sf:
# base_sf %>% st_transform(crs = 32619) %>% 
#   ggplot() + 
#   geom_sf(aes(color=Log_Biomass, geometry=geometry), fill = "transparent", shape = 15) +
#   scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent") + #, limits = plot_lims()) +
#   geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
#   labs(title = "Baseline",) +
#   theme_map()
# 
# 
# 
# 
# # Working to tile from lat/lon in degrees:
# base_sf %>% st_transform(crs = 32619) %>% 
#   mutate(x = sf::st_coordinates(.)[,1],
#          y = sf::st_coordinates(.)[,2]) %>% 
#   #bind_cols(base_sf) %>% 
#   ggplot() +
#   geom_tile(aes(x, y, fill = Log_Biomass), color = "transparent" ,  width = 25000, height = 25000) +
#   scale_fill_viridis_c(option = "viridis", na.value = "transparent") + 
#   geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
#   labs(title = "Baseline - geom_tile", fill = "Log( Biomass )") +
#   theme_map()
# 
# 
# 
# 
# 
# 
# 
# #Loading UTM geojson
# utm_test <- baseline_sfList$American_lobster_SSP85_mean_baseline_utm.geojson
# ggplot(utm_test) +
#   geom_sf(aes(color=Log_Biomass, geometry=geometry), fill = "transparent", shape = 15) +
#   scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent") + 
#   geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
#   labs(title = "Baseline",) +
#   theme_map()
# 
# 
# #Loading UTM geojson
# wgs_test <- baseline_sfList$American_lobster_SSP85_mean_baseline_wgs.geojson
# ggplot(wgs_test) +
#   geom_sf(aes(color=Log_Biomass, geometry=geometry), fill = "transparent", shape = 15) +
#   scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent") + 
#   geom_sf(data = st_transform(land_sf, crs = 4326), fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
#   coord_sf(xlim = c(-76, -56), ylim = c(35.5, 47.5), expand = FALSE, crs = 4326) +
#   labs(title = "Baseline") +
#   theme_map()
