# UI for displaying metrics
leadsmotorUI <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    valueBoxOutput(ns("totalleads"), width = 4),
    valueBoxOutput(ns("permissionGranted"), width = 4),
    valueBoxOutput(ns("miniappSource"), width = 4)
  ),
  fluidRow(
      box(title = "Distribution of Leads by Source", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("LeadsBySource")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Cover Type", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("LeadsByCoverType")) %>% withSpinner(type = 6)),
      box(title = "Leads by Day of the Week", status = "white", solidHeader = TRUE,
          plotOutput(ns("leadsbydayofweek")) %>% withSpinner(type = 6)),
      box(title = "Leads by Day", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("leadsbyday")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Time of Day", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("leadsbyTimeofDay")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Permission to Call", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("leadsbyPermissiontoCall")) %>% withSpinner(type = 6)),
  )
 )
}



# Server logic for attendance metrics
leadsmotorServer <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {


output$totalleads <- renderValueBox({
    valueBox(
      value = format(nrow(reactiveData()), big.mark = ","),  # Format with commas
      subtitle = "Total Leads", 
      icon = icon("users"),  # Using a more representative icon
      color = "white"
    )
})

output$permissionGranted <- renderValueBox({
    # Prepare the data by filtering out blanks and calculating percentages
    data <- reactiveData() %>%
        filter(Permission != "") %>%
        mutate(PermissionYes = ifelse(Permission == "Yes", 1, 0))
    # Calculate percentage of 'Yes' permissions
    permission_percentage <- sum(data$PermissionYes) / nrow(data) * 100
    # Format as percentage
    formatted_percentage <- sprintf("%.2f%%", permission_percentage)
    valueBox(
      value = formatted_percentage,
      subtitle = "Permission to Call Granted",
      icon = icon("check-circle"),
      color = "white"
    )
})

output$miniappSource <- renderValueBox({
    # Prepare the data by filtering out blanks in the "SOURCE" column
    data <- reactiveData() %>%
        filter(SOURCE != "") %>%
        mutate(MiniappSource = ifelse(SOURCE == "Mini App", 1, 0))  # Add a new column for miniapp source
    # Calculate percentage of clients from miniapp
    miniapp_percentage <- sum(data$MiniappSource) / nrow(data) * 100
    # Format as percentage with two decimal points
    formatted_percentage <- sprintf("%.2f%%", miniapp_percentage)
    valueBox(
      value = formatted_percentage,
      subtitle = "Leads from Miniapp",
      icon = icon("mobile-alt"),  # Icon representing mobile apps
      color = "white"
    )
})




custom_colors_cover_tod <- c("#B0E0E6", "#4682B4", "#191970", "#2ca02c", "#2ca02c", "#F28E2B", "#d62728")
output$leadsbyTimeofDay <- renderPlotly({ 
    data <- reactiveData()  # Ensure data is loaded and contains the right columns
    # Filter out rows where 'Time of Day' might be NA
    data <- data[!is.na(data$`Time of Day`), ]
    # Group data by 'Time of Day' and count the occurrences
    count_by_time <- data %>%
      group_by(`Time of Day`) %>%
      summarise(Count = n(), .groups = 'drop')
    # Generate a qualitative color palette
    num_categories <- length(unique(count_by_time$`Time of Day`))
    # Create the donut chart
    p <- plot_ly(count_by_time, labels = ~`Time of Day`, values = ~Count, type = 'pie', hole = 0.4,
                 textposition = 'outside', 
                 texttemplate = "%{label}<br>%{value:,}<br>(%{percent})",    
                 insidetextorientation = 'radial',  # Corrected the typo from 'tangetial' to 'radial'
                 marker = list(colors = custom_colors_cover_tod),
                 textfont = list(color = 'black', family = "Mulish", size = 10))
    # Add title and display the plot
    p <- p %>% layout(title = "Leads by Time of Day",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
})


output$LeadsBySource <- renderPlotly({
    data <- reactiveData() %>%
      filter(!is.na(SOURCE), SOURCE != "") %>%
      group_by(SOURCE) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))

    # Creating a lollipop plot with explicit mode and adjusted text position
    plot_ly(data, y = ~reorder(SOURCE, Count), x = ~Count, type = 'scatter', mode = 'markers+lines',
            marker = list(color = '#0d6efd', size = 10),  # Professional blue for markers
            line = list(color = '#0d6efd', width = 2)) %>%
      layout(
        title = "Distribution of Leads by Source",
        yaxis = list(title = "Source"),
        xaxis = list(title = "Number of Leads", zeroline = FALSE, showline = TRUE),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        showlegend = FALSE,
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish"),
        margin = list(r = 50)
      ) %>%
      add_text(text = ~Count, x = ~Count + ifelse(Count > 500, -100, 10), y = ~reorder(SOURCE, Count), 
               textposition = ~ifelse(Count > 500, 'inside', 'right')) # Adjust text for long bars
})


custom_colors_cover_ptc <- c("#007B7F",  "#76D7EA", "#002D62", "#2ca02c", "#2ca02c", "#F28E2B", "#d62728")
output$leadsbyPermissiontoCall <- renderPlotly({ 
    data <- reactiveData()  # Ensure data is loaded and contains the right columns
    # Filter out rows where 'Time of Day' might be NA
    data <- data[!is.na(data$Permission), ]
    # Group data by 'Time of Day' and count the occurrences
    count_by_permission <- data %>%
      group_by(Permission) %>%
      summarise(Count = n(), .groups = 'drop')
    # Generate a qualitative color palette
    num_categories <- length(unique(count_by_permission$Permission))
    # Create the donut chart
    p <- plot_ly(count_by_permission, labels = ~Permission, values = ~Count, type = 'pie', hole = 0.4,
                 textposition = 'outside', 
                 texttemplate = "%{label}<br>%{value:,}<br>(%{percent})",   
                 insidetextorientation = 'radial',  # Corrected the typo from 'tangetial' to 'radial'
                 marker = list(colors = custom_colors_cover_ptc),
                 textfont = list(color = 'black', family = "Mulish", size = 10))
    # Add title and display the plot
    p <- p %>% layout(title = "Leads by Permission to Call",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
})


output$leadsbyday <- renderPlotly({
    data <- reactiveData() %>%
      group_by(Date) %>%
      summarise(Leads = n(), .groups = "drop") %>%
      arrange(Date)  # Ensure the dates are in chronological order
  
    # Plotting
    plot_ly(data, x = ~Date, y = ~Leads, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd'),
            text = ~paste('Date:', Date, '<br>Leads:', Leads)) %>%  # Custom hover text
      layout(
        title = "Trend of Leads by Day",
        xaxis = list(title = "Date", tickformat = "%b %Y"),  # Formatting the date display
        yaxis = list(title = "Number of Leads", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        hovermode = 'closest'  # Ensures hover effects are well defined for close data points
      )
})


output$leadsbydayofweek <- renderPlot({
    data <- reactiveData() %>%
      mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) %>%  # Extract weekday name
      group_by(Weekday) %>%
      summarise(Leads = n(), .groups = "drop") %>%
      arrange(Weekday)  # Sort by day of the week, Monday to Sunday

    # Plotting using ggplot2
    ggplot(data, aes(x = Weekday, y = Leads)) +
      geom_bar(stat = "identity", fill = "#4682B4") +
      geom_text(aes(label = scales::comma(Leads)), vjust = -0.3, size = 3.5) +
      labs(title = "Leads by Day of the Week", x = "Day of the Week", y = "Number of Leads") +
      theme_minimal() +
      theme(text = element_text(family = "Mulish"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 14))
})



output$LeadsByCoverType <- renderPlotly({
    data <- reactiveData() %>%
      filter(`Cover type` != "", !is.na(`Cover type`)) %>%
      group_by(`Cover type`) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Calculate cumulative values for the waterfall chart
    data <- data %>%
      mutate(Cumulative = cumsum(Count))
    
    plot_ly(data, 
            x = ~`Cover type`, 
            y = ~Count, 
            type = 'waterfall', 
            texttemplate = ~paste(formatC(Count, format = "f", big.mark = ",", digits = 0), 
                                  " (", round((Count / sum(data$Count)) * 100, 2), "%)", sep = ""),
            textposition = "outside", 
            measure = rep("relative", nrow(data)),  # Set measure to relative for each cover type
            connector = list(line = list(color = "blue", width = 2)),
            increasing = list(marker = list(color = '#0d6efd')),  # Set a professional blue color for increasing values
            decreasing = list(marker = list(color = '#0d6efd')),  # Set the same color for decreasing values
            totals = list(marker = list(color = '#0d6efd'))  # Set the same color for totals if needed
            ) %>%
      layout(
        title = "Distribution of Leads by Cover Type",
        yaxis = list(title = "Number of Leads"),
        xaxis = list(title = "Cover Type"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish")
      )
})



  })
}
