# UI for displaying metrics
drysalesmotorUI <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    valueBoxOutput(ns("uniqueDryShiftDays"), width = 4),
    valueBoxOutput(ns("percentDryShiftDays"), width = 4),
    valueBoxOutput(ns("staffPerformanceDryShifts"), width = 4)
  ),
  fluidRow(
    box(title = "Dry Sales Distribution by Day of the Week", status = "white", solidHeader = TRUE, 
          plotOutput(ns("drysalesbydayofweek")) %>% withSpinner(type = 6)),
    box(title = "Dry Sales Distribution by Day", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("drysalesbyday")) %>% withSpinner(type = 6)),
    box(title = "Dry Shifts by Time of Day", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("dryShiftFrequencyByDayType")) %>% withSpinner(type = 6)),
    box(title = "Total Dry Sales Leads by Day of the Week", status = "white", solidHeader = TRUE,
          plotOutput(ns("totalLeadsByDayOfWeek")) %>% withSpinner(type = 6)),


  )
 )
}



# Server logic for attendance metrics
drysalesmotorServer <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {

 # Unique Dry Shift Days
    output$uniqueDryShiftDays <- renderValueBox({
      data <- reactiveData()
      dry_shifts <- data %>%
        filter(Closed == 0) %>%
        summarise(DryDays = n_distinct(Date))
      valueBox(
        value = dry_shifts$DryDays,
        subtitle = "Unique Dry Sales Days",
        icon = icon("calendar-times"),
        color = "white" 
      )
    })

    # Percentage of Dry Shift Days
    output$percentDryShiftDays <- renderValueBox({
      data <- reactiveData()
      start_date <- as.Date("2024-01-01")
      last_date_in_data <- max(as.Date(data$Date))
      total_days_in_year <- as.integer(last_date_in_data - start_date + 1) 
      dry_days <- data %>%
        filter(Closed == 0) %>%
        summarise(DryDays = n_distinct(Date)) %>%
        .$DryDays
      percent_dry_days <- dry_days / total_days_in_year * 100
      valueBox(
        value = sprintf("%.2f%%", percent_dry_days),
        subtitle = "Percentage of Dry Sales Days",
        icon = icon("percent"),
        color = "white"  
      )
    })


    # Staff Performance on Dry Shifts
    output$staffPerformanceDryShifts <- renderValueBox({
      data <- reactiveData()
      zero_sales <- data %>% filter(Closed == 0)
      staff_performance <- zero_sales %>%
        group_by(`Staff on Duty`) %>%
        summarise(TotalZeroShifts = n()) %>%
        arrange(desc(TotalZeroShifts))
      top_staff <- head(staff_performance, 1)
      valueBox(
        value = paste(top_staff$`Staff on Duty`, ":", top_staff$TotalZeroShifts, "Days"),
        subtitle = "Most Dry Sales by Staff",
        icon = icon("user-times"),
        color = "white"  # Adjust color as needed
      )
    })



output$drysalesbyday <- renderPlotly({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    group_by(Date) %>%
    summarise(TotalDryShifts = n(), .groups = "drop") %>%  # Aggregate dry shifts for both shifts
    arrange(Date)

  plot_ly(data, x = ~Date, y = ~TotalDryShifts, type = 'scatter', mode = 'lines+markers',
          line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd'),
          hoverinfo = 'text',
          text = ~paste('Date:', Date, '<br>Total Dry Shifts:', TotalDryShifts)) %>% 
    layout(
      title = "Total Dry Shifts by Day",
      xaxis = list(title = "Date", tickformat = "%b %d, %Y"),
      yaxis = list(title = "Total Number of Dry Shifts"),
      font = list(family = "Mulish", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = 'closest'
    )
})

output$drysalesbydayofweek <- renderPlot({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) %>%
    group_by(Weekday) %>%
    summarise(TotalDryShifts = n(), .groups = "drop") %>%
    arrange(Weekday)  # Ensure days are sorted from Monday to Sunday

  ggplot(data, aes(x = Weekday, y = TotalDryShifts)) +
    geom_bar(stat = "identity", fill = "#4682B4") +
    geom_text(aes(label = scales::comma(TotalDryShifts)), vjust = -0.3, size = 3.5) +
    labs(title = "Total Dry Shifts by Day of the Week", x = "Day of the Week", y = "Total Number of Dry Shifts") +
    theme_minimal() +
    theme(text = element_text(family = "Mulish"),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14))
})




output$dryShiftsByTimeOfDay <- renderPlotly({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    group_by(`Time of Day`) %>%
    summarise(DryShifts = n(), .groups = "drop") %>%
    mutate(Percentage = DryShifts / sum(DryShifts) * 100) %>%
    arrange(desc(DryShifts))

  plot_ly(data, x = ~`Time of Day`, y = ~DryShifts, type = 'bar',
          text = ~paste(sprintf("%.2f%%", Percentage)), # Adding percentage on the bar
          hoverinfo = 'text',
          hovertext = ~paste('Time of Day:', `Time of Day`, '<br>Dry Shifts:', DryShifts, '<br>Percentage:', sprintf("%.2f%%", Percentage)),
          marker = list(color = c('Day' = '#1E90FF', 'Night' = '#6495ED'))) %>%
    layout(
      title = "Distribution of Dry Shifts by Time of Day",
      xaxis = list(title = "Time of Day"),
      yaxis = list(title = "Number of Dry Shifts"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})

output$dryShiftFrequencyByDayType <- renderPlotly({
  # Prepare the data
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    group_by(`Time of Day`) %>%
    summarise(DryShifts = n(), .groups = "drop") %>%
    mutate(Percentage = DryShifts / sum(DryShifts) * 100) %>%
    arrange(`Time of Day`)

  # Determine the base, increases, and text for each step
  text_labels <- paste(data$`Time of Day`, ":", data$DryShifts, " shifts (", sprintf("%.2f%%", data$Percentage), ")", sep = "")

  plot_ly(type = "waterfall",
          measure = c("relative", "relative"),
          x = data$`Time of Day`,
          text = text_labels,
          y = data$DryShifts,
          connector = list(line = list(color = "rgb(63, 63, 63)")),
          decreasing = list(marker = list(color = "rgb(10, 132, 255)", line = list(color = "rgb(10, 132, 255)", width = 3))),
          increasing = list(marker = list(color = "rgb(10, 132, 255)", line = list(color = "rgb(10, 132, 255)", width = 3))),
          totals = list(marker = list(color = "rgb(0, 123, 255)", line = list(color = "rgb(0, 123, 255)", width = 3)))
  ) %>%
    layout(title = "Dry Shifts by Time of Day",
           xaxis = list(title = "Time of Day"),
           yaxis = list(title = "Number of Dry Shifts", autorange = TRUE),
           font = list(family = "Arial", color = "black"),
           showlegend = FALSE,
           annotations = list(
             text = "Percentage",
             x = data$`Time of Day`,
             y = data$DryShifts,
             showarrow = TRUE,
             font = list(family = "Arial", size = 14, color = "black"),
             align = "center",
             xanchor = "center",
             yanchor = "bottom",
             yshift = 10
           ),
           plot_bgcolor = "white",
           paper_bgcolor = "white")
})


output$totalLeadsByDayOfWeek <- renderPlot({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) %>%
    group_by(Weekday) %>%
    summarise(TotalLeads = sum(`Total Leads`, na.rm = TRUE), .groups = "drop") %>%
    arrange(Weekday)  # Ensure days are sorted from Monday to Sunday

  ggplot(data, aes(x = Weekday, y = TotalLeads)) +
    geom_bar(stat = "identity", fill = "#4682B4") +
    geom_text(aes(label = scales::comma(TotalLeads)), vjust = -0.3, size = 3.5) +
    labs(title = "Total Dry Sales Leads by Day of the Week", x = "Day of the Week", y = "Sum of Total Leads") +
    theme_minimal() +
    theme(text = element_text(family = "Mulish"),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14))
})

})

}
