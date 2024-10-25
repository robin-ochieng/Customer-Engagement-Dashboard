# modules/motorModule.R

source("modules/data_processing.R")  # Ensures the data processing functions are available

motorUI <- function(id) {
  ns <- NS(id)
  tagList(
    # UI elements specific to the Motor module
    selectInput(ns("leads_month"), "Select Month", choices = NULL),
    selectInput(ns("leads_quarter"), "Select Quarter", choices = NULL),
    selectInput(ns("leads_year"), "Select Year", choices = NULL),
    # Possibly other UI elements like plots or tables could go here
  )
}

motorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load and process data
    leads_motor <- reactive({ read_and_process_data_motor("data/Motor Leads.xlsx") })
  
    observe({
      motor_data <- req(leads_motor())
      motor_data <- motor_data %>%
        mutate(
          Month = format(as.Date(Month), "%m"),
          Year = as.numeric(format(as.Date(Year), "%Y"))
        )
      
      month_choices <- unique(motor_data$Month)
      quarter_choices <- c("Q1", "Q2", "Q3", "Q4")
      year_choices <- unique(motor_data$Year)
      
      updateSelectInput(session, ns("leads_month"), choices = month_choices, selected = month_choices[1])
      updateSelectInput(session, ns("leads_quarter"), choices = quarter_choices, selected = quarter_choices[1])
      updateSelectInput(session, ns("leads_year"), choices = year_choices, selected = year_choices[1])
    })
    


    # Output or further processing here
  })
}
