####  Species Distribution Forecasting App

# Author: Matt Dzaugis & Adam Kemberling
# About:
# Display species distribution shifts as
# forecaster by VAST models



####  Packages  ####
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(gmRi)
library(zoo)
library(sf)



# Support Functions:
source(here::here("R/app_support_functions.R"))


####  Data Prep  ####


# Path to shiny app on Box
sdmShiny_path <- gmRi::cs_path(box_group = "Mills lab", 
                               subfolder = "Projects/sdm_workflow/targets_output/shiny")

# Path to Res_Data folder on Box
Res_Data_path <- gmRi::cs_path(box_group = "Res_Data",
                               subfolder = "Shapefiles/ne_50m_land/")


# Read the cropped land coverage:
land_sf <- read_sf("./Data/spatial/nw_atlantic_countries_crs32619.geojson")
land_wgs <- read_sf("./Data/spatial/nw_atlantic_countries_crs4326.geojson")



#### User Selection Choices  ####


# List of the available species
speciesList <- sort(
  c("American lobster", #"Black sea bass", 
    "Atlantic cod", 
    "Atlantic halibut", 
    "Atlantic herring", 
    "Haddock", 
    "Sea scallop", 
    "Yellowtail flounder"))


# List of the available SSP scenarios
sspList <- c("SSP5 8.5")




#### File Loading  ####


#### 1. Baseline Data  ####

# # Read in baselines
# baseline_dataList <- fetch_boxdata("baseline_data") %>% 
#   map(~data.table::fread(.x, stringsAsFactors = FALSE))
# 
# # Baseline Prep
# baseline_dataList <- map(baseline_dataList, baseline_prep)


# Load Pre-Prepped App Data::
baseline_datList <- fetch_appdata(data_resource = "baseline_data")



#### 2. Projected Data  ####

# # Read in the SSP projections:
# dataList <- fetch_boxdata("projected_data") %>% 
#   map(function(x){data.table::fread(x, stringsAsFactors = FALSE)})
# 
# 
# # Projected Prep
# dataList <- map(dataList, projection_prep)


# Load Pre-Prepped App Data::
dataList <- fetch_appdata(data_resource = "spatial_summary")




#### 3. Annual Spatial Summaries  ####

# # Read in the Spatial Summaries:
# annual_dataList <- fetch_boxdata("spatial_summary") %>% 
#   map(~ data.table::fread(.x, stringsAsFactors = FALSE)) 
# 
# 
# # Annual Summary prep
# annual_dataList <- map(annual_dataList, spat_summ_prep)



# Load Pre-Prepped App Data::
annual_dataList <- fetch_appdata(data_resource = "spatial_summary")


####  4.  Summaries  

# # Center of biomass data
# center_dataList <- fetch_boxdata("center_biomass") %>% 
#   map(~data.table::fread(.x, stringsAsFactors = FALSE))


# Load Pre-Prepped App Data::
center_dataList <- fetch_appdata(data_resource = "center_biomass")


####________####
#### Shiny UI modules  ####

# Sidebar user selection inputs
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Select Species
    selectInput(
      inputId = ns("species"),
      label = "Species name", 
      choices = unique(speciesList),
      selected = unique(speciesList)[1]
    ),
    
    # Select CMIP SSP Scenario
    selectInput(
      inputId = ns("ssp"),
      label = "SSP scenario", 
      choices = unique(sspList),
      selected = unique(sspList)[1]
    ),
    
    # Select the sruvey season
    selectInput(
      inputId = ns("season"),
      label = "Season", 
      choices = c("Spring", "Summer", "Fall"),
      selected = "Spring"
    ),
    # sliderInput(
    #   inputId = ns("year"),
    #   label = "Year", 
    #   value = 2020,
    #   min=1985,
    #   max=2100,
    #   sep=""
    # )
    sliderInput(
      inputId = ns("decade"),
      label = "Decade", 
      value = 2020,
      min = 2020,
      max = 2090, 
      step = 10,
      sep="", 
      post = "'s"
    )
    
  )
  
}

# Map output(s)
mapUI <- function(id) {
  ns <- NS(id)
  tagList(plotOutput(ns("plot")))
}


# Download button output
downloadUI <- function(id) {
  ns <- NS(id)
  tagList(downloadButton(ns('downloadPlot'), 'Download Plot'),
          downloadButton(ns('downloadData'), 'Download Data'))
}



####________####
#### Shiny server modules  ####

# Testing the input  filters:
# input <- list("species" = "American lobster", "ssp" = "SSP5 8.5", "decade" = 2020, season = "Spring")



# Filter the data list based on species and ssp
sideFilter <- function(id, datalist, baseline = FALSE){
  moduleServer(
    id,
    function(input, output, session){
      
       # Get dataset based on user inputs:
       # subsets from dataList or baseline_dataList
       df <- reactive({
        
         # adjust input string from species
         name_ <- str_replace_all(input$species, "[:space:]", "_")
         
         # Adjust string input from ssp
         ssp_ <- str_replace_all(input$ssp, c("[:space:]" = "_", "\\." = ""))
         ssp_ <- str_c("ST_", ssp_)
        
        # Toggle for Baseline Data
        if(isTRUE(baseline)){
          dfName <- str_subset(names(datalist), paste(name_, sep="_"))
        }
        
        
        # Toggle for Projections
        if(isFALSE(baseline)){
          dfName <- str_subset(names(datalist), paste(name_, ssp_, sep="_"))
        }
        
        
        # Pull Projection from dataList (or baseline_dataList)
        df <- datalist[[dfName]]
        
        return(df)
      })
    }
  )
}



# Testing dataList and baseline_dataList Filtering:

# name_test <- str_replace_all(input$species, "[:space:]", "_")
# ssp_test <- str_replace_all(input$ssp, c("[:space:]" = "5_", "\\." = ""))
# ssp_test <- str_c("ST_", ssp_test)

# baseline name matching:
# str_subset(names(baseline_dataList),  paste(name_test, sep="_")) 

# projection list name matching:
# str_subset(names(dataList), paste(name_test, ssp_test, sep="_"))
# dfName <- str_subset(names(dataList), paste(name_test, ssp_test, sep="_"))




# Filter projection data by selected species, ssp by decade and season
sideServer <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Take projection data and subset by user selections
      df_r <- reactive({
        
        df <- df() %>% 
          dplyr::filter(
            #year == input$year,
            Decade == input$decade,                           
            Season == input$season)

        return(df)
      })
      return(df_r)
    })
}


# Testing the filters of sideServer:

# df <- dataList[[dfName]]
# df %>% filter(Decade == input$decade, Season == input$season)




# Create the dynamic plot limits - unnecessary 
plot_limsServer <- function(id, df){
  moduleServer(
    id,
    function(input, output, session) {
      
      plotLims <- reactive({
        plotlims <- c(0,
                      #min(tibble(df())[, "Log_Biomass"], na.rm = TRUE), 
                      max(tibble(df())[, "Log_Biomass"], na.rm = TRUE))
      })
      return(plotLims)
    })
}



# Testing pllotLims
# min(tibble(df)[, "Log_Biomass"], na.rm = TRUE)
# max(tibble(df)[, "Log_Biomass"], na.rm = TRUE)



# Create map names/titles
mapName <- function(id, plotType){
  moduleServer(
    id,
    
    function(input, output, session){
      mapName <- reactive({
        
        # Baseline Plot Title
        if(plotType == "baseline"){
          titleName <- paste(input$species, "Baseline Period 2015-2019: log(Biomass)", sep = " | ")
        }
        
        if(plotType == "sdmPlot"){
          titleName <- paste(input$species, " | ", input$ssp, " Projected Biomass: ", input$season, " ", input$decade, "'s", sep = "")
        }
        
        if(plotType == "difference"){
          titleName <- paste(input$species, " | Baseline Change to the ", input$decade, "'s",sep = "")
        }
        
        if(plotType == "annualPlot"){
          titleName <- paste(input$species, "|", input$ssp, "Projected Annual Biomass", sep = " ")
        }
        
        if(plotType == "centerPlot"){
          titleName <- paste(input$species, "|", input$ssp, "Projected Center of Biomass", sep = " ")
        }
        
        return(titleName)
      })
    }
  )
}


# SDM draw plot
mapServer <- function(id, dataInput, land_sf, plot_lims, mapTitle, diff_limits = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        
        sdmPlotFun(dataInput, land_sf, plot_lims, mapTitle, diff_limits)
        
      })
    })
}


# Difference Data Server Calculation
differenceServer <- function(id, baselineInput, projectionInput, mapTitle){
  moduleServer(
    id,
    function(input, output, session){
      df_r <- reactive({
        df <- sf::st_join(baselineInput(), projectionInput(), # don't think by is relevant for these...
                          by = c("Lon", "Lat", "Species", "Season", "Climate_Scenario", "geometry")) %>% 
          mutate(Log_Biomass = Log_Biomass.y - Log_Biomass.x,
                 Biomass = Biomass.y - Biomass.x) %>% 
          dplyr::select(-Log_Biomass.x, -Log_Biomass.y, -Biomass.x, -Biomass.y)
        return(df)
      })
      
      return(df_r)
    }
  )
}


# Annual summary draw plot
tsPlotServer <- function(id, dataInput, plotType, plotTitle){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "annualPlot"){
        output$plot <- renderPlot({
          annualPlotFun(dataInput = dataInput, plot_title = plotTitle)
        })
      }

      if(plotType == "centerPlot"){
        output$plot <- renderPlot({
          centerBioPlotFun(dataInput = dataInput, plot_title = plotTitle, land_wgs = land_wgs)
        })
      }
      
    }
  )
}

# draw and download plot
downloadServer <- function(id, plotType, dataInput, land_sf, plot_lims, mapTitle, diff_limits = FALSE){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "sdmPlot"){
        plot1 <- reactive({
          sdmPlotFun(dataInput, land_sf, plot_lims, mapTitle, diff_limits)
        })
      }
      
      if(plotType == "annualPlot"){
        plot1 <- reactive({
          annualPlotFun(dataInput = dataInput, plot_title = mapTitle)
        })
      }
      
      if(plotType == "centerPlot"){
        plot1 <- reactive({
          centerBioPlotFun(dataInput = dataInput, plot_title = mapTitle, land_wgs = land_wgs)
        })
      }
      
      output$downloadPlot <- downloadHandler(
        filename = function() {paste0(mapTitle(), ".png")},
        content = function(file) {
          ggsave(file, plot = plot1(), width = 5, height = 5)
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function() {paste0(mapTitle(), ".csv")},
        content = function(file) {
          write_csv(dataInput(), file)
        }
      )
    }
  )
}



####________####
####  User Interface  ####


# Build ui & server and then run
ui <- dashboardPage(
  
  ####_  Header  ####
  dashboardHeader(title = "Mapping Marine Species Distribution Under CMIP6",
                  titleWidth = 500),
  
  
  ####_  Sidebar  ####
  dashboardSidebar(h3("Project Information"), # make a new tab with project information
                   h3("Selection Panel"),
                   sideUI("side1")),
  
  
  
  ####_  Dashboard Body  ####
  dashboardBody(
    
    
    # First Row three Panels: Baseline, Projection, Difference
    fluidRow(
      
      
      # Baseline Period Map
      box(width = 4,
          title = "Baseline Species Distribution",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("baseline"),
          downloadUI("baseline")),
             
      # SSP Seasonal Projection
      box(width = 4,
          title = "Projected Species Distribution",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("map1"),
          downloadUI("map1")),
      
      # Difference in Biomass
      box(width = 4,
          title = "Forecasted Change in Biomass",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("difference"),
          downloadUI("difference"))),
    
    
    # Second Row 2 Panels: Annual Biomass
    fluidRow(
      
      # Annual Biomass Summary
      box(width = 6,
          title = "Annual Biomass Summary",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("annualPlot1"),
          downloadUI("annualPlot1")),
      
      # Center of Biomass Summary
      box(width = 6,
          title = "Projected Center of Biomass",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("centerPlot"),
          downloadUI("centerPlot"))
    
      ) # Close Second fluidRow
    
    ) # Close DashboardBody
  
  ) # Close DashboardPage




####________####
####  Server Interface  ####

server <- function(input, output, session) {

  ####_  Projected Map  ####
  
  # sdm plot - Projection Data
  
  # Filter the data, use dataList
  df1 <- sideFilter("side1", datalist = dataList)
  
  # Get plot limits for color scale
  plotLims1 <- plot_limsServer("side1", df1)
  
  # build map title
  mapTitle1 <- mapName("side1", plotType = "sdmPlot")
  
  # Pull the data 
  data_input1 <- sideServer("side1", df1) # season year/decade data
  mapServer("map1", 
            dataInput = data_input1, 
            land_sf = land_sf, 
            plot_lims = plotLims1, 
            mapTitle = mapTitle1,
            diff_limits = FALSE)
  downloadServer("map1", 
                 plotType = "sdmPlot", 
                 dataInput = data_input1, 
                 land_sf = land_sf, 
                 plot_lims = plotLims1, 
                 mapTitle = mapTitle1,
                 diff_limits = FALSE)

  
  ####_ Baseline Map  ####
  baselineDF <- sideFilter("side1", datalist = baseline_dataList, baseline = TRUE)
  baseline_mapTitle <- mapName("side1", plotType = "baseline")  
  mapServer("baseline", 
            dataInput = baselineDF, 
            land_sf = land_sf, 
            plot_lims = plotLims1, 
            mapTitle = baseline_mapTitle,
            diff_limits = FALSE)
  downloadServer("baseline", 
                 plotType = "sdmPlot", 
                 dataInput = data_input1, 
                 land_sf = land_sf, 
                 plot_lims = plotLims1, 
                 mapTitle = baseline_mapTitle,
                 diff_limits = FALSE)
  
  
  ####_ Difference Map    ####
  diff_mapTitle <- mapName("side1", plotType = "difference") 
  diffDF <- differenceServer("side1", baselineInput = baselineDF, projectionInput = data_input1, mapTitle = differenceTitle)
  plotLims2 <- plot_limsServer("side1", diffDF)
  mapServer("difference", 
            dataInput = diffDF, 
            land_sf = land_sf, 
            plot_lims = plotLims2, 
            mapTitle = diff_mapTitle,
            diff_limits = TRUE)
  downloadServer("difference", 
                 plotType = "sdmPlot", 
                 dataInput = diffDF, 
                 land_sf = land_sf, 
                 plot_lims = plotLims2, 
                 mapTitle = diff_mapTitle,
                 diff_limits = TRUE)
  
  
  ####_ Annual plot  ####
  annual_df1 <- sideFilter("side1", datalist = annual_dataList, baseline = FALSE)
  # Add filtering for the season? : Something like this:
  # ann_seas_DF <- annualServer("side1",  datalist = baseline_dataList, projectionInput = data_input1)
  
  ann_mapTitle1 <- mapName("side1", plotType = "annualPlot")
  tsPlotServer("annualPlot1", 
               dataInput = annual_df1, 
               plotType = "annualPlot", 
               plotTitle = ann_mapTitle1)
  downloadServer("annualPlot1", 
                 plotType = "annualPlot", 
                 dataInput = annual_df1, 
                 land_sf = land_sf, 
                 plot_lims = plotLims1, 
                 mapTitle = ann_mapTitle1)
  
  
  ####_ Center of Biomass  ####
  center_df1 <- sideFilter("side1", datalist = center_dataList, baseline = FALSE)
  center_mapTitle1 <- mapName("side1", plotType = "centerPlot")
  tsPlotServer("centerPlot", 
               dataInput = center_df1, 
               plotType = "centerPlot", 
               plotTitle = center_mapTitle1)
  downloadServer("centerPlot", 
                 plotType = "centerPlot", 
                 dataInput = center_df1, 
                 land_sf = land_sf, 
                 plot_lims = plotLims1, 
                 mapTitle = center_mapTitle1)

}

shinyApp(ui, server)
