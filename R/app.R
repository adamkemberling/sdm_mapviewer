####  Species Distribution Forecasting App

# Author: Matt Dzaugis & Adam Kemberling
# About:
# Display species distribution shifts as
# forecaster by VAST models

####  Setup:  ####

####  Packages  ####
library(tidyverse)
library(shiny)
library(shinydashboard)
#library(leaflet)
library(here)
#library(zoo)
library(sf)
library(shinyWidgets)
library(shinyalert)
library(waiter)



# Support Functions:
source(here::here("R/app_support_functions.R"))


####  Data Prep  ####



# # Read the cropped land coverage:

# Read the cropped land coverage:
land_sf <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs32619.geojson"))
land_wgs <- read_sf(here::here("./Data/spatial/nw_atlantic_countries_crs4326.geojson"))

# Load the Hague Lines
hague_sf <- read_sf(here::here("Data/spatial", "hagueline_crs32619.geojson"))
#hague_wgs <- here::here("Data/spatial", "hagueline_crs4326.geojson")


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


####_________________________####

#### File Loading:  ####


#### 1. Baseline Data  ####

# # Read in baselines:

# Load Pre-Prepped App Data::
baseline_dataList <- fetch_appdata(data_resource = "baseline_data")



#### 2. Projected Data  ####

# # Read in the SSP projections:

# Load Pre-Prepped App Data::
dataList <- fetch_appdata(data_resource = "projected_data")




#### 3. Annual Spatial Summaries  ####

# # Read in the Spatial Summaries:

# Load Pre-Prepped App Data::
annual_dataList <- fetch_appdata(data_resource = "spatial_summary") %>% 
  map(function(x){
    x %>% 
      mutate(
        Region = factor(Region, levels = c("DFO Survey Area", "Gulf of Maine", "Southern New England & Mid-Atlantic Bight")),
        Season = factor(Season, levels = c("All", "Spring", "Summer", "Fall")))
      })


####  4.  Summaries  

# # Center of biomass data

# Load Pre-Prepped App Data::
center_dataList <- fetch_appdata(data_resource = "center_biomass")


####__________________________####
####  Modules:  ####


#### UI modules  ####

# Sidebar user selection inputs
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Select Species
    selectInput(
      inputId = ns("species"),
      label = "Species Name", 
      choices = unique(speciesList),
      selected = unique(speciesList)[1]
    ),
    
    # Select CMIP SSP Scenario
    selectInput(
      inputId = ns("ssp"),
      label = "SSP Climate Scenario", 
      choices = unique(sspList),
      selected = unique(sspList)[1]
    ),
    
    # Select the survey season
    selectInput(
      inputId = ns("season"),
      label = "Survey Season", 
      choices = c("Spring", "Summer", "Fall"),
      selected = "Spring"
    ),
    
    # Select the forecast decade
    sliderInput(
      inputId = ns("decade"),
      label = "Decade to Display", 
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
#### Server modules  ####

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
mapServer <- function(id, dataInput, land_sf, hague_sf, plot_lims, mapTitle, diff_limits = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        
        sdmPlotFun(dataInput, land_sf, hague_sf, plot_lims, mapTitle, diff_limits)
        
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
tsPlotServer <- function(id, dataInput, hague_sf,  plotType, plotTitle){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "annualPlot"){
        output$plot <- renderPlot({
          annualPlotFun(dataInput = dataInput, 
                        plot_title = plotTitle)
        })
      }

      if(plotType == "centerPlot"){
        output$plot <- renderPlot({
          centerBioPlotFun(dataInput = dataInput, 
                           plot_title = plotTitle, 
                           land_wgs = land_wgs,
                           hague_sf = hague_sf)
        })
      }
      
    }
  )
}

# draw and download plot
downloadServer <- function(id, plotType, dataInput, land_sf, hague_sf, plot_lims, mapTitle, diff_limits = FALSE){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "sdmPlot"){
        plot1 <- reactive({
          sdmPlotFun(dataInput, 
                     land_sf, 
                     hague_sf, 
                     plot_lims, 
                     mapTitle, 
                     diff_limits)
        })
      }
      
      if(plotType == "annualPlot"){
        plot1 <- reactive({
          annualPlotFun(dataInput = dataInput, 
                        plot_title = mapTitle)
        })
      }
      
      if(plotType == "centerPlot"){
        plot1 <- reactive({
          centerBioPlotFun(dataInput = dataInput, 
                           plot_title = mapTitle, 
                           hague_sf = hague_sf,
                           land_wgs = land_wgs)
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



####__________________________####

#### App Design: ####


####  User Interface  ####


# Build ui & server and then run
ui <- dashboardPage(
  
  
  ####_  Header  ####
  dashboardHeader(title = "Mapping Marine Species Distributions Under CMIP6 Projections",
                  titleWidth = 500), 
  
  
  
  ####_  Sidebar  ####
  
  # Original Sidebar:
  dashboardSidebar(
    sidebarMenu(
      id = "nav",
      # make the text in sidebar not flow into body
      # Fix padding for text
      style = "white-space: normal; ",
      # Display Controls
      menuItem(
        "Display Options", 
        tabName = "Displays",
        icon = icon("layer-group")), # source: https://fontawesome.com/icons/map-location-dot?s=solid
        
      
      conditionalPanel(
        condition = "input.nav === 'Displays'",
        style = "
        overflow-wrap: anywhere;
        h1, h2, h3, p{
          padding-left: 10px;
          padding-right: 10px;
        }",
        h3("Display Controls"),
        br(),
        p("This visualization tool was buit to show how we might 
        expect species to move based on climate model predictions."),
        br(),
        p("Changing the following options will update the displays to
           show how our models predict species will move based on
           our climate model ensemble data."),
        sideUI("side1"),
        startExpanded = TRUE),
        
      # ------------ _About Flood CamML -----------
      menuItem("Project Information", 
               tabName = "About", 
               icon = icon("info-circle"))
      
      
      )   # Close sidebar menu
    ),  # Close dashboardSidebar
    
  
  
  

  
  ####_  Dashboard Body  ####
  dashboardBody(
    
    
    
    ####__ Spinners  ####
    # Loading Spinners for Initial page load:
    use_waiter(),
    waiter::waiter_preloader(
      html = tagList(
        h2("Loading FishShift VisTool"),
        spin_wave()), 
      color = "#222d32"),
    
    ####__ Pop Up Modal  ####
    useShinyalert(),
    
    
    
    ####__ Styling  ####
    
    
    # Make app-wide css adjustments manually:
    tags$head(
      tags$style(HTML('

      '))),
    
    
    
    
    
    ####__ Tab 1. Displays  ####
    tabItems(
    
      
      
      # Tab 1. for the model displays:
      tabItem(
        tabName = "Displays",
      
    
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
    
    ), # Close tabItem "Displays"
    
    
    
    
    ####__ Tab 2. About the Project  ####
    tabItem(
      tabName = "About",
      
      # Set up page for the project background:
      fluidRow(
        includeMarkdown(here::here("text", "about.md"))
            
        ) # Close about page fluidrow
      
      ) # Close tabItem "About"
    
    ) # Close tabItems
    
    ) # Close DashboardBody
  
  ) # Close DashboardPage




####________####
####  Server Interface  ####

server <- function(input, output, session) {

  ####_ Disclaimer  Pop Up  ####
  # Popup on load to display info
  shinyalert(title = "",
             html = T,
             text = includeMarkdown(here::here("text/landing_text.md")),
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonText = "OK",
             imageWidth = 50,
             imageHeight = 50,
             type = "info",
             animation=F,
             size = "s",
             inputId = "splash_page", 
             closeOnEsc = T)
  
  
  
  # ####_  Loading Spinners  ####
  # 
  # # Spinners on input changes, not working...
  # observeEvent(input$decade, {
  # 
  # 
  #   # Spinner for SDM Forecasted Distribution
  #   show_waiter(
  #     spin_3(),
  #     id = "plot.map1", # specify id of plot to overlay
  #     color = "#00608A"
  #   )
  # 
  #   # Something that needs to load should go here, but its not working with modules...
  # 
  # 
  # 
  #   # hide waiters
  #   hide_waiter(id = "plot.map1")
  # 
  # 
  #   # Spinner for Difference Map
  #   show_waiter(
  #     spin_3(),
  #     id = "plot.difference", # specify id of plot to overlay
  #     color = "#00608A"
  #   )
  # 
  # 
  #   # Something that needs to load should go here, but its not working with modules...
  # 
  # 
  #   # Hide Waiter
  #   hide_waiter(id = "plot.difference")
  # 
  # })

  
  
  
  ####_ Baseline Map  ####
  baselineDF <- sideFilter("side1", datalist = baseline_dataList, baseline = TRUE)
  baseline_mapTitle <- mapName("side1", plotType = "baseline")  
  mapServer("baseline", 
            dataInput = baselineDF, 
            land_sf = land_sf, 
            hague_sf = hague_sf,
            plot_lims = plotLims1, 
            mapTitle = baseline_mapTitle,
            diff_limits = FALSE)
  downloadServer("baseline", 
                 plotType = "sdmPlot", 
                 dataInput = data_input1, 
                 land_sf = land_sf, 
                 hague_sf = hague_sf,
                 plot_lims = plotLims1, 
                 mapTitle = baseline_mapTitle,
                 diff_limits = FALSE)
  
  
  
  
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
            hague_sf = hague_sf,
            plot_lims = plotLims1,
            mapTitle = mapTitle1,
            diff_limits = FALSE)
  downloadServer("map1",
                 plotType = "sdmPlot",
                 dataInput = data_input1,
                 land_sf = land_sf,
                 hague_sf = hague_sf,
                 plot_lims = plotLims1,
                 mapTitle = mapTitle1,
                 diff_limits = FALSE)

  
 
  
  
  ####_ Difference Map    ####
  diff_mapTitle <- mapName("side1", plotType = "difference")
  diffDF <- differenceServer("side1", baselineInput = baselineDF, projectionInput = data_input1, mapTitle = differenceTitle)
  plotLims2 <- plot_limsServer("side1", diffDF)
  mapServer("difference",
            dataInput = diffDF,
            land_sf = land_sf,
            hague_sf = hague_sf,
            plot_lims = plotLims2,
            mapTitle = diff_mapTitle,
            diff_limits = TRUE)
  downloadServer("difference", 
                 plotType = "sdmPlot", 
                 dataInput = diffDF, 
                 land_sf = land_sf, 
                 hague_sf = hague_sf,
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
                 mapTitle = ann_mapTitle1)
  
  
  ####_ Center of Biomass  ####
  center_df1 <- sideFilter("side1", datalist = center_dataList, baseline = FALSE)
  center_mapTitle1 <- mapName("side1", plotType = "centerPlot")
  tsPlotServer("centerPlot", 
               dataInput = center_df1, 
               hague_sf = hague_sf,
               plotType = "centerPlot", 
               plotTitle = center_mapTitle1)
  downloadServer("centerPlot", 
                 plotType = "centerPlot", 
                 hague_sf = hague_sf,
                 dataInput = center_df1, 
                 mapTitle = center_mapTitle1)

}

shinyApp(ui, server)
