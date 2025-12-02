library(shiny)
library(dplyr)
library(DT)
library(plotly)

server <- function(input, output, session) {
  # Filtering ATUS data based on user's inputs
  filtered_atus <- reactive({
    data <- activity_2020_24
    # Filtering by age range
    if (input$age_choice != "All") {
      data <- data |>
        filter(age_group == input$age_choice)
    }
    # Filtering by time slot
    if (input$time_choice != "All") {
      data <- data |>
        filter(time_slot == input$time_choice)
    }
    # Filtering by day options
    if (input$days_choice != "All") {
      data <- data |> filter(wkday_wkend == input$days_choice)
    }
    # Filtering by location
    if (input$location_choice != "All") {
      data <- data |> filter(location_detail == input$location_choice)
    }
    data
  })
  # visualization: treemap
  treemap <- reactive({
    req(nrow(filtered_atus()) > 0)
    
    df <- filtered_atus()
    
    df <- df |> 
      filter(!is.na(first_four_digits_classifications) & !is.na(activity_detail))
    
    parents <- df |> 
      group_by(first_four_digits_classifications) |> 
      summarize(
        value = n(),
        avg_duration = mean(duration_hours, na.rm = T)
      ) |> 
      mutate(
        ids = first_four_digits_classifications,
        labels = first_four_digits_classifications,
        parents = "",
        category_name = first_four_digits_classifications,
        color_group = first_four_digits_classifications
      )
    
    children <- df |> 
      group_by(first_four_digits_classifications, activity_detail) |> 
      summarize(
        value = n(),
        avg_duration = mean(duration_hours, na.rm = T),
        .groups = "drop"
      ) |> 
      mutate(
        ids = paste(first_four_digits_classifications, activity_detail, sep = " - "),
        labels = activity_detail,
        parents = first_four_digits_classifications,
        category_name = first_four_digits_classifications,
        color_group = first_four_digits_classifications
      )
    
    bind_rows(parents, children)
  })
  
  output$treemapPlot <- renderPlotly({
    plot_data <- treemap()
    
    plot_ly(
      data = plot_data,
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~value,
      type = "treemap",
      branchvalues = "total",
      marker = list(colorscale = "Viridis"),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "count: %{value}<br>",
        "average duration: %{customdata:.1f}hours<extra></extra>"
      ),
      customdata = ~avg_duration
    ) |> 
      layout(
        title = "Activities Treemap",
        margin = list(t=50, l=0, r=0, b=0)
      )
  })
  
  output$scatterPlot <- renderPlotly({
    req(nrow(filtered_atus()) > 0)
    scatter_data <- filtered_atus() |>
      filter(!is.na(activity_detail)) |> 
      group_by(activity_detail, first_four_digits_classifications) |> 
      summarise(
        mean_duration = mean(duration_hours, na.rm = TRUE),
        frequency = n(),
        .groups = "drop"
      ) |> 
      filter(frequency > 100)
    plot_ly(
      data = scatter_data,
      x = ~mean_duration,
      y = ~frequency,
      type = 'scatter',
      mode = 'markers',
      size = ~frequency, 
      color = ~first_four_digits_classifications,
      colors = "Set2",
      text = ~paste(
        "<b>", activity_detail, "</b><br>",
        "broad category:", first_four_digits_classifications, "<br>",
        "average duration:", round(mean_duration, 1), "hours<br>",
        "count:", frequency
      ),
      hoverinfo = "text"
    ) |>
      layout(
        title = "Do you want short or long activities?",
        xaxis = list(title = "Average Duration (hours)"),
        yaxis = list(title = "Number of People Who Enjoy the Activity"),
        legend = list(title = list(text = "Category"))
      )
    })
  
  
  # Meetup events (IN PROGRESS)
  meetup_to_show <- reactive({
    if (is.null(meetup_events) || nrow(meetup_events) == 0) {
      return(NULL)
    }
    # Prototype only has art meetup data
    if (!is.null(input$activity_type) &&
        input$activity_type == "Arts and Entertainment") {
      meetup_events
    } else {
      meetup_events[0, ]
    }
  })
  # Message for meetup output (IN PROGRESS)
  output$meetup_message <- renderText({
    data <- meetup_to_show()
    if (is.null(data) || nrow(data) == 0) {
      "No matching Meetup events in this prototype, but here is your ATUS-based activity suggestion."
    } else {
      "Here are some local art-related Meetup events that might interest you:"
    }
  })
  # Meetup events table (IN PROGRESS)
  output$meetup_table <- renderDT({
    data <- meetup_to_show()
    req(!is.null(data), nrow(data) > 0, any(!is.na(data$event_name)))    
    data <- data %>%
      dplyr::select(event_name, group, date, link) %>%
      mutate(
        link = ifelse(
          !is.na(link) & link != "",
          paste0("<a href='", link, "' target='_blank'>Event link</a>"),
          "No link available right now."
        )
      )
    datatable(
      data,
      escape = FALSE,
      options = list(pageLength = 5, lengthChange = FALSE)
    )
  })
}
