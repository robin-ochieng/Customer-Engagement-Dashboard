# UI for displaying claims metrics
claimsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("totalClaimsProcessed"), width = 4),
      valueBoxOutput(ns("claimsAwaitingAction"), width = 4),  
      valueBoxOutput(ns("avgSettlementTime"), width = 4)
    ), 
  fluidRow(
      box(title = "Distribution of Claims by Insurer", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("ClaimsByInsurer")) %>% withSpinner(type = 7)),
      box(title = "Distribution of Claims by Type of Claim", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("typeofClaim")) %>% withSpinner(type = 8)),
      box(title = "Distribution of Claims by Status", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("ClaimsByStatus")) %>% withSpinner(type = 8)),
      box(title = "Claims Reporting Timeline", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("claimsbyday")) %>% withSpinner(type = 8)),
   )
  )
}                      

# Server logic for claims metrics
claimsServer <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {
    output$totalClaimsProcessed <- renderValueBox({
      data <- reactiveData()
      total_claims <- nrow(data)
      valueBox(
        value = total_claims,
        subtitle = "Total Claims Processed",
        icon = icon("clipboard-check"),
        color = "white"
      )
    })

    output$claimsAwaitingAction <- renderValueBox({
      data <- reactiveData()
      awaiting_action <- sum(data$`Claim Status` %in% c("Awaiting settlement", "Undergoing Repairs", "Awaiting Assessment Report", "Awaiting refund", "Not Documented", "Awaiting supporting documents"))
      valueBox(
        value = awaiting_action,
        subtitle = "Claims Awaiting Action",
        icon = icon("clock"),
        color = "white"
      )
    })

    output$avgSettlementTime <- renderValueBox({
        data <- reactiveData()
        data$`Date of Loss` <- as.Date(data$`Date of Loss`)
        data$`Reported On` <- as.Date(data$`Reported On`)
        avg_days <- mean(data$`Reported On` - data$`Date of Loss`, na.rm = TRUE)
        valueBox(
            value = round(avg_days, 2),
            subtitle = "Average Claim Settlement Time (days)",
            icon = icon("hourglass-half"),
            color = "white"
        )
    })



    output$ClaimsByInsurer <- renderPlotly({
        data <- reactiveData() %>%
            filter(!is.na(Insurer), Insurer != "") %>%
            group_by(Insurer) %>%
            summarize(Count = n(), .groups = 'drop') %>%
            arrange(desc(Count))
        # Reverse the order for better visual display on the y-axis
        data$Insurer <- factor(data$Insurer, levels = rev(data$Insurer))
        # Creating a lollipop plot with explicit mode and adjusted text position
        plot_ly(data, y = ~Insurer, x = ~Count, type = 'scatter', mode = 'markers+text',
                marker = list(color = '#0d6efd', size = 10),  # Professional blue for markers
                text = ~paste(Count),  # Add the count as text next to each marker
                textposition = 'right',  # Position the text to the right of the markers
                hoverinfo = 'text',  # Show only text when hovering
                line = list(color = '#0d6efd', width = 2)) %>%
            layout(
            title = "Distribution of Claims by Insurer",
            yaxis = list(title = "Insurer"),
            xaxis = list(title = "Number of Claims", zeroline = FALSE, showline = TRUE, range = c(0, max(data$Count) + 5)),
            hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
            showlegend = FALSE,
            plot_bgcolor = 'white',
            paper_bgcolor = 'white',
            font = list(family = "Mulish"),
            margin = list(r = 50, l = 150)  # Adjust left margin to prevent label cutoff
            )
    })


 output$typeofClaim <- renderPlotly({
    data <- reactiveData() %>%
      filter(!is.na(`Type of Claim`), `Type of Claim` != "") %>%
      group_by(`Type of Claim`) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Create funnel chart in Plotly
    plot_ly(data, 
            y = ~reorder(`Type of Claim`, -Count), 
            x = ~Count, 
            type = 'funnel', 
            textinfo = "value+percent",
            marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Distribution of Claims by Type of Claim",
        yaxis = list(title = "Type of Claim"),
        xaxis = list(title = "Number of Leads"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish")
      )
})   

output$ClaimsByStatus <- renderPlotly({
    data <- reactiveData() %>%
        filter(!is.na(`Claim Status`), `Claim Status` != "") %>%
        group_by(`Claim Status`) %>%
        summarize(Count = n(), .groups = 'drop') %>%
        arrange(desc(Count))

    # Adjust the order for a better visual display
    data$Claim_Status <- factor(data$`Claim Status`, levels = unique(data$`Claim Status`))

    # Creating a horizontal bar plot with counts displayed next to the bars
    plot_ly(data, x = ~Count, y = ~Claim_Status, type = 'bar', orientation = 'h',
            marker = list(color = '#17becf'),  # Cyan color for bars
            text = ~Count,  # Adding count as text
            textposition = 'outside',  # Positioning text outside the bars
            hoverinfo = 'text+x') %>%  # Hover shows text and x value
        layout(
            title = "Distribution of Claims by Status",
            xaxis = list(title = "Number of Claims",
                         zeroline = FALSE,
                         showline = TRUE,
                         side = 'bottom',
                         range = c(0, max(data$Count) * 1.2)),  # Extend the x-axis range by 20%
            yaxis = list(title = "Claim Status", automargin = TRUE),  # Ensure y-axis labels fit
            hoverlabel = list(bgcolor = '#17becf', font = list(color = 'white')),
            showlegend = FALSE,
            plot_bgcolor = 'white',
            paper_bgcolor = 'white',
            font = list(family = "Mulish"),
            margin = list(r = 100, l = 150, t = 50, b = 50)  # Increase right margin to accommodate larger numbers
        )
})



output$claimsbyday <- renderPlotly({
    data <- reactiveData() %>%
      group_by(`Reported On`) %>%
      summarise(Claims = n(), .groups = "drop") %>%
      arrange(`Reported On`)  # Ensure the dates are in chronological order
  
    # Plotting using Plotly
    plot_ly(data, x = ~`Reported On`, y = ~Claims, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd'),
            text = ~paste('Date:', `Reported On`, '<br>Leads:', Claims)) %>%  # Custom hover text
      layout(
        title = "Trend of Claims Reported Over Time",
        xaxis = list(title = "Date", tickformat = "%b %d, %Y"),  # Formatting the date display for more granularity
        yaxis = list(title = "Number of Reported Claims"),
        font = list(family = "Mulish", color = "#333333"),  # Adjusting font style and color
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        hovermode = 'closest'  # Ensures hover effects are well defined for close data points
      )
})


  })
}
