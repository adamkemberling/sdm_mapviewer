# App Support Functions



####  Data Prep  ####

# Convert points in sf polygon for plotting

bSquare <- function(x, a) {
  a <- sqrt(a)/2
  x <- sf::st_as_sf(x, coords = c("x", "y"), crs = 32619, remove = FALSE) %>% drop_na(Log_Biomass)
  x <- mutate(x, geometry = sf::st_buffer(geometry, 
                                          dist = a, 
                                          nQuadSegs=1, 
                                          endCapStyle = "SQUARE"))
  return(x)
}


####  Themes  ####

# Plotting map theme
theme_map <- function(x){
  theme(panel.background = element_blank(), 
       panel.border = element_blank(), 
       axis.text.x=element_blank(), 
       axis.text.y=element_blank(), 
       axis.ticks=element_blank(),  
       plot.margin = margin(t = 10, r = 0.05, b = 0.05, l = 0.05, unit = "pt"),
       legend.position = c(.85, .25))}



####  Plotting Functions  ####


# Center of Biomass plotting function
centerBioPlotFun <- function(dataInput, plot_title){
  plot_out <- dataInput() %>% 
    ggplot() +
    geom_line(aes(year, latitude)) +
    xlab("Year") +
    ylab("Center of Biomass (latitude)") +
    ggtitle(plot_title()) + 
    theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  return(plot_out)
  
}


# Annual_summary plotting function
annualPlotFun <- function(dataInput, plot_title){
  
  spat_summ_df <- dataInput()
  
  plot_regions <- unique(spat_summ_df$Region)
  
  color_pal <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
  colors_use <- color_pal[1:length(plot_regions)]
  
  # Date stuffs
  date_col_class<- class(spat_summ_df$Date)
  if(is.character(date_col_class)){
    spat_summ_df$Date<- as.Date(spat_summ_df$Date)
  }
  date_breaks<- seq.Date(from = as.Date(min(spat_summ_df$Date)), to = as.Date(max(spat_summ_df$Date)), by = "5 years")
  
  plot_out <- ggplot() +
    geom_point(data = spat_summ_df, aes_string(x = "Date", y = "meanBio", color = "Region")) +
    geom_line(data = spat_summ_df, aes_string(x = "Date", y = "meanBio", color = "Region")) +
    scale_color_manual(values = colors_use) +
    scale_x_date(breaks = date_breaks, date_labels = "%Y") +
    xlab("Year") +
    ylab("Mean log biomass (kg)") +
    ggtitle(plot_title()) + 
    theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  return(plot_out)
  
}



# SDM plotting function
sdmPlotFun <- function(dataInput, land_sf, plot_lims, mapTitle){
  ggplot(dataInput()) + 
    geom_sf(aes(fill=Log_Biomass, color=Log_Biomass, geometry=geometry)) +
    scale_fill_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims()) +
    scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims()) +
    geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
    coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
    labs(title = mapTitle()) +
    theme_map()
}


