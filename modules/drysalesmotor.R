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
    box(title = "Day vs. Night Dry Shift Comparison", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("dayNightComparison")) %>% withSpinner(type = 6)),
    box(title = "Staff Performance on Dry Shifts", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("staffPerformance")) %>% withSpinner(type = 6)),
    box(title = "Prospect to Lead Conversion Rate", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("prospectToLeadConversion")) %>% withSpinner(type = 6)),
    box(title = "Dry Shift Frequency by Day Type", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("dryShiftFrequencyByDayType")) %>% withSpinner(type = 6)),
    box(title = "Correlation Heatmap", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("correlationHeatmap")) %>% withSpinner(type = 6)),

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


output$dayNightComparison <- renderPlotly({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    group_by(Date, `Time of Day`) %>%
    summarise(DryShifts = n(), .groups = "drop") %>%
    arrange(Date)

  plot_ly(data, x = ~Date, y = ~DryShifts, type = 'bar', color = ~`Time of Day`, colors = c("Day" = "#FFD700", "Night" = "#1E90FF"),
          hoverinfo = 'text',
          text = ~paste('Date:', Date, '<br>Time of Day:', `Time of Day`, '<br>Dry Shifts:', DryShifts)) %>%
    layout(
      barmode = 'stack',
      title = "Day vs. Night Dry Shift Comparison",
      xaxis = list(title = "Date", tickformat = "%b %d, %Y"),
      yaxis = list(title = "Number of Dry Shifts"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})

output$staffPerformance <- renderPlotly({
  data <- reactiveData()%>%
    filter(Closed == 0) %>%
    group_by(`Staff on Duty`, `Time of Day`) %>%
    summarise(DryShifts = n(), .groups = "drop") %>%
    arrange(desc(DryShifts))

  plot_ly(data, x = ~`Staff on Duty`, y = ~DryShifts, type = 'bar', color = ~`Time of Day`, colors = c("Day" = "#FF6347", "Night" = "#4682B4"),
          hoverinfo = 'text',
          text = ~paste('Staff:', `Staff on Duty`, '<br>Time of Day:', `Time of Day`, '<br>Dry Shifts:', DryShifts)) %>%
    layout(
      barmode = 'group',
      title = "Staff Performance on Dry Shifts",
      xaxis = list(title = "Staff on Duty"),
      yaxis = list(title = "Number of Dry Shifts"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})

output$prospectToLeadConversion <- renderPlotly({
  data <- reactiveData() %>%
    mutate(ConversionRate = ifelse(Prospect > 0, Closed / Prospect * 100, 0)) %>%
    group_by(Date) %>%
    summarise(AverageConversionRate = mean(ConversionRate, na.rm = TRUE), .groups = "drop") %>%
    arrange(Date)

  plot_ly(data, x = ~Date, y = ~AverageConversionRate, type = 'scatter', mode = 'lines+markers',
          line = list(color = '#32CD32'), marker = list(color = '#006400'),
          hoverinfo = 'text',
          text = ~paste('Date:', Date, '<br>Conversion Rate:', sprintf("%.2f%%", AverageConversionRate))) %>%
    layout(
      title = "Prospect to Lead Conversion Rate Over Time",
      xaxis = list(title = "Date", tickformat = "%b %d, %Y"),
      yaxis = list(title = "Average Conversion Rate (%)"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = 'closest'
    )
})

output$dryShiftFrequencyByDayType <- renderPlotly({
  data <- reactiveData() %>%
    filter(Closed == 0) %>%
    group_by(`Day Type`) %>%
    summarise(TotalDryShifts = n(), .groups = "drop")

  plot_ly(data, x = ~`Day Type`, y = ~TotalDryShifts, type = 'bar',
          marker = list(color = c('Weekday' = '#1E90FF', 'Weekend' = '#FFA07A')),
          hoverinfo = 'text',
          text = ~paste('Day Type:', `Day Type`, '<br>Total Dry Shifts:', TotalDryShifts)) %>%
    layout(
      title = "Dry Shift Frequency by Day Type",
      xaxis = list(title = "Day Type"),
      yaxis = list(title = "Number of Dry Shifts"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})

output$correlationHeatmap <- renderPlotly({
  data <- reactiveData() %>%
    select(Prospect, Closed, `Total Leads`) %>%
    cor(use = "complete.obs")  # Calculating the correlation matrix

  plot_ly(x = colnames(data), y = colnames(data), z = as.matrix(data),
          type = 'heatmap', colorscale = 'Viridis',
          hoverinfo = 'text',
          text = ~paste('Correlation:', formatC(data, format = 'e', digits = 2))) %>%
    layout(
      title = "Correlation Heatmap",
      xaxis = list(title = "Metrics"),
      yaxis = list(title = "Metrics"),
      font = list(family = "Arial", color = "black"),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
})



})

}
