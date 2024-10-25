# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(DT) 
library(scales)
library(bs4Dash)
library(bslib)
library(fresh)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(readr)
library(tidyverse)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bootswatch = 'minty',
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  
  navbar_fg = "#ffffff")

# Sourcing the modules
source("modules/data_processing.R")
source("modules/medicalleadsmetrics.R")
source("modules/motorleadsmetrics.R")
source("modules/drysalesmotor.R")
source("modules/drysalesmedical.R")
source("modules/claims.R")


# Here's a simplified example of how you might set up the dashboard UI to display these metrics:
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("Customer Engagement Dashboard", class = "header-title")
    ),
  titleWidth = 400,
  controlbarIcon = NULL,
  sidebarIcon = NULL,
  fixed = TRUE,
  tags$div(class = "control-bar", actionButton("toggleControlbar", "Filters", class = "btn btn-primary control-button"))
  ),
  dashboardSidebar(
    fixed = TRUE,
    collapsed = FALSE,
    minified = FALSE,
    width = 150,
    dashboardSidebar(
      tags$div(
        class = "leads-container",
        tags$h3("Leads Report", class = "leads-title"),
        sidebarMenu(
          menuItem("Motor", tabName = "motormetrics", icon = icon("car-side")),
          menuItem("Medical", tabName = "medicalmetrics", icon = icon("briefcase-medical"))
        )
      ),
      tags$div(style = "margin-top: 50px;"),  
      tags$div(
        class = "leads-container",
        tags$h3("Dry Sales Report", class = "leads-title"),
        sidebarMenu(
          menuItem("Motor", tabName = "drysalesmotor", icon = icon("chart-line")),
          menuItem("Medical", tabName = "drysalesmedical", icon = icon("tools"))
        )
      ),
      tags$div(style = "margin-top: 50px;"),  
      tags$div(
        class = "leads-container",
        tags$h3("Claims Report", class = "leads-title"),
        sidebarMenu(
          menuItem("Claims", tabName = "claims", icon = icon("file-alt"))
        )
      )
    ),
    div(class = "sidebar-footer",
        img(src = "images/kenbright2.png")
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet")
    ),
    tabItems(
      tabItem(tabName = "motormetrics", leadsmotorUI("motormetricsMod")),
      tabItem(tabName = "medicalmetrics", leadsmedicalUI("medicalmetricsMod")),
      tabItem(tabName = "drysalesmotor", drysalesmotorUI("drysalesmotorMod")),
      tabItem(tabName = "drysalesmedical", drysalesmedicalUI("drysalesmedicalMod")),
      tabItem(tabName = "claims", claimsUI("claimsMod"))
    ),
    div(class = "body-footer", "© 2024 Customer Engagement Dashboard - Kenbright") 
  ),
  title = "Customer Engagement Dashboard",
  skin = "blue",
  controlbar = dashboardControlbar(  
    skin = "light",
    title = "Filter Settings", 
    id = "dashboardControlbar",
    width = 300,
    bs4Card(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = HTML('<i class="fas fa-car-side"></i> Motor Leads Filters'),
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "leads_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "leads_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "leads_year", label = "Select Year", choices = NULL)
    ),
    bs4Card(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = HTML('<i class="fas fa-briefcase-medical"></i> Medical Leads Filters'),
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "leads_medical_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "leads_medical_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "leads_medical_year", label = "Select Year", choices = NULL)
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("dashboardControlbar")
  })
  
  # Load and process data

  #load and process motor data
  leads_motor <- read_and_process_data_motor("data/Motor Leads.xlsx")

  #load and process medical data
  leads_medical <- read_and_process_data_medical("data/Medical Leads.xlsx")

    # Reactive expression to load and process data
  drysalesmotor <- reactive({
    # Assuming you have a function `read_and_process_data_drysalesmotor` to load and process your data
    read_and_process_data_drysalesmotor("data/Dry Shifts Data.xlsx")
  })



  # Reactive expression to load and process data
  drysalesmedical <- reactive({
    # Assuming you have a function `read_and_process_data_drysalesmotor` to load and process your data
    read_and_process_data_drysalesmedical("data/Dry Shifts Data.xlsx")
  })


  #Data_medical <- read_and_process_data("data/Medical Leads.xlsx")
  
  #Medical
  #1. MOTOR LEADS ---------------------------------------------------------------------------------------------------------------------------------------
  observe({
    # Ensure Month is two digits for date parsing
    leads_motor <- leads_motor %>%
      mutate(
        Month = as.character(Month),
        Month = trimws(Month),
        Year = as.numeric(Year),
        Quarter = as.character(Quarter))
    
    month_choices <- leads_motor$Month[!is.na(leads_motor$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- leads_motor$Quarter[!is.na(leads_motor$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- leads_motor$Year[!is.na(leads_motor$Year)] %>% unique()
    
    updateSelectInput(session, "leads_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "leads_quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "leads_year", choices = year_choices, selected = format(Sys.Date(), "%Y"))
  })
  
  # Reactive expression to filter the data based on selected month, quarter, and year
  filtered_data__leads_motor <- reactive({
    req(leads_motor)  
    if (input$leads_month == "All" && input$leads_quarter == "All") {
      leads_motor %>%
        filter(Year == as.numeric(input$leads_year))
    } else if (input$leads_quarter == "All") {
      leads_motor %>%
        filter(Month == input$leads_month, Year == as.numeric(input$leads_year))
    } else if (input$leads_month == "All") {
      leads_motor %>%
        filter(Quarter == as.character(input$leads_quarter), Year == as.numeric(input$leads_year))
    } else {
      leads_motor %>%
        filter(Month == input$leads_month, Quarter == as.character(input$leads_quarter), Year == as.numeric(input$leads_year))
    }
  })


  # Call the motor leads metrics module
  leadsmotorServer("motormetricsMod", filtered_data__leads_motor)


#2. MEDICAL LEADS --------------------------------------------------------------------------------------------------------------------------------------

  observe({
    # Ensure Month is two digits for date parsing
    leads_medical <- leads_medical %>%
      mutate(
        Month = as.character(Month),
        Month = trimws(Month),
        Year = as.numeric(Year),
        Quarter = as.character(Quarter))
    
    month_choices <- leads_medical$Month[!is.na(leads_medical$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- leads_medical$Quarter[!is.na(leads_medical$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- leads_medical$Year[!is.na(leads_medical$Year)] %>% unique()
    
    updateSelectInput(session, "leads_medical_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "leads_medical_quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "leads_medical_year", choices = year_choices, selected = format(Sys.Date(), "%Y"))
  })
  
  
  # Reactive expression to filter the data based on selected month, quarter, and year
  filtered_data__leads_medical <- reactive({
    req(leads_medical)  
    if (input$leads_medical_month == "All" && input$leads_medical_quarter == "All") {
      leads_medical %>%
        filter(Year == as.numeric(input$leads_medical_year))
    } else if (input$leads_medical_quarter == "All") {
      leads_medical %>%
        filter(Month == input$leads_medical_month, Year == as.numeric(input$leads_medical_year))
    } else if (input$leads_medical_month == "All") {
      leads_medical %>%
        filter(Quarter == as.character(input$leads_medical_quarter), Year == as.numeric(input$leads_medical_year))
    } else {
      leads_medical %>%
        filter(Month == input$leads_medical_month, Quarter == as.character(input$leads_medical_quarter), Year == as.numeric(input$leads_medical_year))
    }
  })


  # Call the medical leads metrics module
  leadsmedicalServer("medicalmetricsMod", filtered_data__leads_medical)


#3. DRY SALES MOTOR -----------------------------------------------------------------------------------------------------------------------------------

  # Call the dry sales motor metrics module
  drysalesmotorServer("drysalesmotorMod", drysalesmotor)
  

#4. DRY SALES MEDICAL ---------------------------------------------------------------------------------------------------------------------------------

  # Call the dry sales medical metrics module
  drysalesmedicalServer("drysalesmedicalMod", drysalesmedical)

#5. CLAIMS -------------------------------------------------------------------------------------------------------------------------------------------
  # Reactive expression to load and process data
  claimsData <- reactive({
    # Assuming you have a function `read_and_process_data_drysalesmotor` to load and process your data
    read_and_process_data_claims("data/Claims Data.csv")
  })
  
  claimsServer("claimsMod", claimsData)


}

shinyApp(ui, server)
