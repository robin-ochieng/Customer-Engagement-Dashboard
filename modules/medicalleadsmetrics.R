# UI for displaying metrics
leadsmedicalUI <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    valueBoxOutput(ns("totalLeadsMedical"), width = 4),
    valueBoxOutput(ns("permissionGranted"), width = 4),
    valueBoxOutput(ns("miniappSource"), width = 4)

  ),
  fluidRow(
      box(title = "Distribution of Leads by Budget", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("LeadsByBudget")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Principal's Age", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("PrincipalAgeDistribution")) %>% withSpinner(type = 6)),
      box(title = "Leads by Day of the Week", status = "white", solidHeader = TRUE,
          plotOutput(ns("leadsbydayofweek")) %>% withSpinner(type = 6)),
      box(title = "Leads by Day", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("leadsbyday")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Cover Type", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("LeadsByHealthCategory")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Source", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("LeadsBySource")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Time of Day", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("leadsbyTimeofDay")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Leads by Permission to Call", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("leadsbyPermissiontoCall")) %>% withSpinner(type = 6)),
  )
 )
}



# Server logic for attendance metrics
leadsmedicalServer <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {


output$totalLeadsMedical <- renderValueBox({
    valueBox(
      value = format(nrow(reactiveData()), big.mark = ","),  
      subtitle = "Total Leads", 
      icon = icon("users"),  
      color = "white"
    )
})

output$permissionGranted <- renderValueBox({
    # Prepare the data by filtering out blanks and calculating percentages
    data <- reactiveData() %>%
        filter(`Permission to Call` != "") %>%
        mutate(PermissionYes = ifelse(`Permission to Call` == "Yes", 1, 0))
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
        filter(Source != "") %>%
        mutate(MiniappSource = ifelse(Source == "Mini App", 1, 0))  # Add a new column for miniapp source
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



output$leadsbyday <- renderPlotly({
    data <- reactiveData() %>%
      group_by(Date) %>%
      summarise(Leads = n(), .groups = "drop") %>%
      arrange(Date)  # Ensure the dates are in chronological order
  
    # Plotting
    plot_ly(data, x = ~Date, y = ~Leads, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd'),
            hoverinfo = 'text',
            text = ~paste('Date:', Date, '<br>Leads:', Leads)) %>%  # Custom hover text
      layout(
        title = "Trend of Leads by Day",
        xaxis = list(title = "Date", tickformat = "%b %Y"),  # Formatting the date display
        yaxis = list(title = "Number of Leads", tickfont = list(size = 10, color = "black")),
        font = list(family = "Mulish", color = "black"),
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
      filter(!is.na(Source), Source != "") %>%
      group_by(Source) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create funnel chart in Plotly
    plot_ly(data, 
            y = ~reorder(Source, -Count), 
            x = ~Count, 
            type = 'funnel', 
            textinfo = "value",
            marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Distribution of Leads by Source",
        yaxis = list(title = "Source"),
        xaxis = list(title = "Number of Leads"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish")
      )
})


custom_colors_cover_ptc <- c("#007B7F",  "#76D7EA", "#002D62", "#2ca02c", "#2ca02c", "#F28E2B", "#d62728")
output$leadsbyPermissiontoCall <- renderPlotly({ 
    data <- reactiveData()  # Ensure data is loaded and contains the right columns
    # Filter out rows where 'Time of Day' might be NA
    data <- data[!is.na(data$`Permission to Call`), ]
    # Group data by 'Time of Day' and count the occurrences
    count_by_permission <- data %>%
      group_by(`Permission to Call`) %>%
      summarise(Count = n(), .groups = 'drop')
    # Generate a qualitative color palette
    num_categories <- length(unique(count_by_permission$`Permission to Call`))
    # Create the donut chart
    p <- plot_ly(count_by_permission, labels = ~`Permission to Call`, values = ~Count, type = 'pie', hole = 0.4,
                 textposition = 'outside', 
                 texttemplate = "%{label}<br>%{value:,}<br>(%{percent})",   
                 insidetextorientation = 'radial',  # Corrected the typo from 'tangetial' to 'radial'
                 marker = list(colors = custom_colors_cover_ptc),
                 textfont = list(color = 'balck', family = "Mulish", size = 10))
    # Add title and display the plot
    p <- p %>% layout(title = "Leads by Permission to Call",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
})


output$LeadsByHealthCategory <- renderPlotly({
    data <- reactiveData() %>%
      filter(`Health Category` != "", !is.na(`Health Category`)) %>%  # Filter out blanks and NA values
      group_by(`Health Category`) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))  # Arrange by count for visual preference

    if(nrow(data) == 0 || is.null(data$Count) || all(is.na(data$Count))) {
      # Handle the case where there is no data or only NA values in Count
      return(NULL)  # Optionally, return a Plotly message plot here indicating no data
    }

    # Calculate an appropriate x-axis range
    max_count <- max(data$Count, na.rm = TRUE)  
    x_axis_limit <- max_count * 1.2  

    # Plotting using Plotly, now as a horizontal bar plot
    plot_ly(data, y = ~`Health Category`, x = ~Count, type = 'bar', orientation = 'h',
            marker = list(color = '#0d6efd')) %>%  # Set a professional blue color
      layout(
        title = "Distribution of Leads by Health Category",
        yaxis = list(title = "Health Category"),
        xaxis = list(title = "Number of Leads", zeroline = FALSE, showline = TRUE, range = c(0, x_axis_limit)),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),  # Customize hover label colors
        showlegend = FALSE, 
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish"),
        margin = list(r = 150)
      ) %>%
      add_text(text = ~Count, textposition = 'outside right')  
})

output$LeadsByBudget <- renderPlotly({
    data <- reactiveData() %>%
      filter(!is.na(Budget), Budget != "") %>%
      group_by(Budget) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create funnel chart in Plotly
    plot_ly(data, 
            y = ~reorder(Budget, -Count), 
            x = ~Count, 
            type = 'funnel', 
            textinfo = "value+percent",
            marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Distribution of Leads by Budget",
        yaxis = list(title = "Budget"),
        xaxis = list(title = "Number of Leads"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish")
      )
})


output$PrincipalAgeDistribution <- renderPlotly({
    data <- reactiveData() %>%
      filter(!is.na(Principal))  # Ensure no missing values in Principal column

    # Create a histogram
    plot_ly(data, x = ~Principal, type = 'histogram',
            marker = list(color = '#0d6efd'),  # Professional blue color
            nbinsx = 30) %>%  # You can adjust the number of bins if needed
      layout(
        title = "Distribution of Principal's Age",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Frequency"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish"),
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
})




  })
}
