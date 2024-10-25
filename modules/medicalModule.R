# modules/medicalModule.R

source("modules/data_processing.R")  # Ensures the data processing functions are available

medicalUI <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements specific to the Medical module
    selectInput(ns("leads_medical_month"), "Select Month", choices = NULL),
    selectInput(ns("leads_medical_quarter"), "Select Quarter", choices = NULL),
    selectInput(ns("leads_medical_year"), "Select Year", choices = NULL),
    # Possibly other UI elements like plots or tables could go here
  )
}

medicalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load and process data
    leads_medical <- reactive({ read_and_process_data_medical("data/Medical Leads.xlsx") })
  
    observe({
      medical_data <- req(leads_medical())
      medical_data <- medical_data %>%
        mutate(
          Month = format(as.Date(Month), "%m"),
          Year = as.numeric(format(as.Date(Year), "%Y"))
        )
      
      month_choices <- unique(medical_data$Month)
      quarter_choices <- c("Q1", "Q2", "Q3", "Q4")
      year_choices <- unique(medical_data$Year)
      
      updateSelectInput(session, ns("leads_medical_month"), choices = month_choices, selected = month_choices[1])
      updateSelectInput(session, ns("leads_medical_quarter"), choices = quarter_choices, selected = quarter_choices[1])
      updateSelectInput(session, ns("leads_medical_year"), choices = year_choices, selected = year_choices[1])
    })
    


  })
}
