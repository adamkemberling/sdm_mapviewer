# SDM App Support Functions




####  Themes  ####

# Plotting map theme
theme_map <- function(...){
  list(
    # Theme options, with ellipse to add more
    theme(
      
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      
      # Grids and Axes
      panel.background = element_blank(), 
      panel.border = element_rect(color = "black", fill = "transparent"), 
      panel.grid.major = element_line(color = "gray80"),
      #axis.text.x=element_blank(), 
      #axis.text.y=element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks=element_blank(),
      plot.margin = margin(t = 10, r = 2, b = 0.1, l = 2, unit = "pt"),
      legend.position = c(.725, .125), 
      legend.background = element_rect(color = "transparent", fill = "white", 
                                       size = 0.25),
          
          # Use ellipses for tweaks on the fly:
          ...),
    
    # Guide design
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5, 
        barwidth = unit(2, "in"),
        barheight = unit(0.6, "cm"), 
        direction = "horizontal",
        frame.colour = "black", 
        ticks.colour = "black"),
      color = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(2, "in"),
        barheight = unit(0.6, "cm"),
        direction = "horizontal",
        frame.colour = "black",
        ticks.colour = "black"))
  )
}




# Plot theme for timeseries
theme_plot <- function(...){
  list(
    theme(
      # Titles
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9),
      
      # Axes
      rect = element_rect(fill = "transparent", color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.ticks.y = element_line(), 
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(), 
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      # Panel/Grid Setup
      panel.grid = element_line(colour = NULL, linetype = 3, color = "gray80"), 
      panel.grid.major = element_line(colour = "black"), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(), 
      # Facets
      strip.text = element_text(color = "white", 
                                face = "bold",
                                size = 11),
      strip.background = element_rect(
        color = "#00736D", 
        fill = "#00736D", 
        size = 1, 
        linetype="solid"),
      # Legend
      legend.position = "bottom"
      
      ) # close theme()
    
  )
  
  
}


####________####
####  Data Prep  ####

# Convert points in sf polygon for plotting

bSquare <- function(x, a, coords = c("x", "y")) {
  a <- sqrt(a)/2
  x <- sf::st_as_sf(x, coords = coords, crs = 32619, remove = FALSE) %>% drop_na(Log_Biomass)
  x <- mutate(x, geometry = sf::st_buffer(geometry, 
                                          dist = a, 
                                          nQuadSegs=1, 
                                          endCapStyle = "SQUARE"))
  return(x)
}



# Get Data that is staged in the cloud based on its folder organization:
fetch_boxdata <- function(data_resource = NULL){
  
  # Path to box where Andrew is putting outputs
  project_path <- gmRi::cs_path("Mills Lab", 
                                subfolder = "Projects/sdm_workflow/DFOSDM_app")
  
  # Folder for the specific resource
  resource_folder <- str_c(project_path, data_resource)
  
  # List the files and name them
  resource_files <- list.files(resource_folder, full.names = TRUE) %>% 
    setNames(list.files(resource_folder, full.names = FALSE))
  
  # Return the files
  return(resource_files)
  
}





# Baseline data prep:
baseline_prep <- function(baseline_dat){
  baseline_prepped <- baseline_dat %>% 
    mutate(Season = factor(Season, levels = c("Spring", "Summer","Fall"))) %>% 
    bSquare(25000*25000, coords = c("Lon", "Lat")) # 
  
  # Drop columns we don't need
  baseline_prepped <- baseline_prepped%>% 
    select(Lon, Lat, Season, Climate_Scenario, Species, Log_Biomass, geometry)
}

# Projected Data prep:
projection_prep <- function(proj_dat){
  proj_prepped <- proj_dat %>% 
    mutate(Season = factor(Season, levels = c("Spring", "Summer","Fall"))) %>% # Do bSquare
    bSquare(25000*25000, coords = c("Lon", "Lat")) 
  
  # Drop columns we don't need
  proj_prepped <- proj_prepped  %>% 
    select(Lon, Lat, Season, Climate_Scenario, Species, Log_Biomass, geometry)
  
  return(proj_prepped)
}


# Biomass Difference prep:
difference_prep <- function(){}


# Annual Summary prep:
spat_summ_prep <- function(spat_summ_df){
  spat_summ_df <- spat_summ_df %>% 
    filter(Region %in% c("SNE_and_MAB", "GoM", "DFO")) %>% 
    mutate(
      Region = case_when(
        Region == "DFO" ~ "DFO Survey Area",
        Region == "GoM" ~ "Gulf of Maine",
        Region == "SNE_and_MAB" ~ "Southern New England & Mid-Atlantic Bight"),
      Region = factor(Region, levels = c("DFO Survey Area", "Gulf of Maine", "Southern New England & Mid-Atlantic Bight")),
      Season = factor(Season, levels = c("All", "Spring", "Summer", "Fall")))
  
  
  # Rolling Average:
  spat_summ_avgs <- spat_summ_df %>% 
    group_by(Region, Season) %>% 
    group_split() %>% 
    map_dfr(function(group_dat){
      group_dat %>% 
        arrange(Year) %>% 
        mutate(mean_5yr = rollapply(Log_Biomass, 
                                    width = 5, 
                                    FUN = mean,
                                    align = 'center',
                                    fill=NA)) 
      }) 
  
  
  # Drop columns we don't need
  spat_summ_avgs <- spat_summ_avgs %>% 
    select(Year, Log_Biomass, Region, Season, mean_5yr)
  
  # Return the data with rolling averages
  return(spat_summ_avgs)
  
}




# Center of biomass prep:
center_bio_prep <- function(){}



# Exporting to relative app paths:

# function to export data
export_app_data <- function(df, fname, folder, fextension = ".geojson", write = FALSE){
  
  save_name <- str_replace(fname, "[.]csv", ".geojson")
  out_path <- str_c("./Data/", folder, "/", save_name)
  
  # If write == FALSE then print what they will be
  if(write == FALSE){
    print(str_c("Save location for: ", out_path))
    print(str_c("Class of: ", class(df)[1]))
  }
  
  # If print == TRUE save the files using the appropriate functions
  if(write == TRUE){
    # Spatial
    if(fextension == ".geojson"){
      write_sf(df, out_path)  
    }
    # Flat file
    if(fextension == ".csv"){
      write_csv(df, out_path)
    }
  }
}


####________####

####  Loading Processed Data  ####


# Load data that is prepared for deployment:
# Data is saved following the preparation steps to minimize processing times
fetch_appdata <- function(data_resource = NULL){
  
  # Path to box where Andrew is putting outputs
  #project_path <- "./Data"
  project_path <- here::here("Data")
  
  # Folder for the specific resource
  resource_folder <- str_c(project_path, data_resource, sep = "/")
  
  # List the files and name them
  resource_files <- list.files(resource_folder, full.names = TRUE) %>% 
    setNames(list.files(resource_folder, full.names = FALSE))
  
  
  # Read them all
  fextension <- switch(
    EXPR = data_resource,
    "baseline_data"   = ".geojson",
    "projected_data"  = ".geojson",
    "spatial_summary" = ".csv",
    "center_biomass"  = ".csv"
  )
  
  # Read spatial files
  if(fextension == ".geojson"){
    resource_files <- map(resource_files, read_sf)
  }
  
  # Read spatial files
  if(fextension == ".csv"){
    resource_files <- map(resource_files, read_csv, col_types = cols())
  }
  
  
  # Return the files
  return(resource_files)
  
}






####________####
####  Plotting Functions  ####




####__  SDM Output Maps  
sdmPlotFun <- function(dataInput, land_sf, hague_sf, plot_lims, mapTitle, diff_limits = FALSE){

  # Map the sdm outputs:  
  sdm_map <- ggplot() +
    geom_sf(data = dataInput(), 
            aes(fill = Log_Biomass, color = Log_Biomass)) +
    geom_sf(data = land_sf, fill = "gray50", color = "white", size = 0.15) 
  
  
  # Change Limits for difference plot
  if(diff_limits == TRUE){
    
    # get max absolute value for difference colors:
    diff_range <- quantile(abs(dataInput()$Log_Biomass), probs = c(0.75)) * c(-1,1)
    
    sdm_map <- sdm_map +
      scale_fill_distiller(palette = "PRGn", na.value = "transparent", direction = 1, limits = diff_range, oob = scales::oob_squish) +
      scale_color_distiller(palette = "PRGn", na.value = "transparent", direction = 1, limits = diff_range, oob = scales::oob_squish)+
      labs(title = mapTitle(), fill = "Change in Log( Biomass kg )", color = "Change in Log( Biomass kg )") 
  } else{
    
    # Use the same scale for baseline and for the projection
    sdm_map <- sdm_map +
      scale_fill_viridis_c(option = "viridis", na.value = "transparent", limits = plot_lims()) +
      scale_color_viridis_c(option = "viridis", na.value = "transparent", limits = plot_lims()) +
      labs(title = mapTitle(), fill = "Log( Biomass kg )", color = "Log( Biomass kg )") 
  }
  
  # finish plot
  sdm_map <- sdm_map +
    geom_sf(data = hague_sf, color = "black", size = 1) +
    coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
    theme_map()
  
  return(sdm_map)
  
}





####__  Annual Biomass Plot


# Updated annual plot function:
annualPlotFun <- function(dataInput, plot_title){
  
  spat_summ_df <- dataInput()
  
  # Plot config:
  plot_regions <- unique(spat_summ_df$Region)
  color_pal <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
  colors_use <- color_pal[1:length(plot_regions)]
  
  
  
  # Works, but will probably want to filter to seasons
  plot_out <- ggplot(spat_summ_df) +
    geom_point(aes(x = Year, y = Log_Biomass, color = Region), alpha = 0.25) +
    geom_line(aes(Year, y = mean_5yr, color = Region, linetype = "5 Year Average"), size = 0.8) +
    scale_x_continuous(expand = expansion(add = c(1,1))) +
    scale_color_manual(values = colors_use, na.translate = F) +
    labs(
      # title = paste(input$species, input$ssp, "Biomass", sep = " "),
      title = plot_title(),
      x = NULL,
      y = "Log( Biomass ) (kg)",
      linetype = "Trendline") + 
    facet_wrap(~Season, ncol = 1) +
    theme_bw() +
    theme_plot()
  
  return(plot_out)
  
}


####__  Center of Biomass  


# Center of Biomass plotting function
centerBioPlotFun <- function(dataInput, plot_title, land_wgs, hague_sf){
  
    # rename for convenience
    center_bio <- dataInput()
  
    # MAke COG data into sf obj
    cog_dat <- center_bio  %>% 
      st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)
      
      
    plot_out <- ggplot() +
      geom_sf(data = cog_dat, aes(alpha = Year)) +
      geom_sf(data = land_wgs, fill = "gray50", color = "white", size = 0.15) +
      geom_sf(data = hague_sf, color = "#EA4F12", size = 1) +
      scale_alpha_continuous(trans = "reverse") +
      coord_sf(xlim = range(center_bio$Lon)+ c(-2,2), ylim = range(center_bio$Lat) + c(-1,1), expand = T, crs = 4326) +
      # coord_sf(xlim = c(-182500, 1550000), ylim = c(3875000, 5370000), expand = F, crs = 32619) +
      theme_map() +
      labs(title = plot_title(), alpha = "Decade") +
      guides(
        alpha = guide_legend(
          title.position = "top",
          title.hjust = 0.5, 
          direction = "horizontal",
          frame.colour = "black", 
          ticks.colour = "black"))
  
  return(plot_out)
  
}
