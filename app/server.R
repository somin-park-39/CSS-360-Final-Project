library(shiny)
library(dplyr)
library(DT)
library(plotly)

server <- function(input, output, session) {
  # Tab 1: Treemap
  # Filtering ATUS data based on user's inputs
  filtered_atus <- reactive({
    data <- ATUS_data
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
    # parents (larger category)
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
        color_group = first_four_digits_classifications,
        box_text = paste(
          "<b>", labels, "</b><br>",
          "count: ", value, "<br>",
          "average duration: ", round(avg_duration, 1), "hours")
      )
    # children (subcategories)
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
        color_group = first_four_digits_classifications,
        box_text = paste(
          "count: ", value, "<br>",
          "average duration: ", round(avg_duration, 1), "hours")
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
      text= ~box_text,
      hovertemplate = paste(
        "<b>%{label}<br><extra></extra>"
      ),
      customdata = ~category_name, source = "treemapSource"
    ) |> 
      layout(
        title = "Exploring Activities",
        margin = list(t=50, l=0, r=0, b=0)
      )
  })
  
  # Switch treemap to meetup when clicking these categories (not all categories)
  atus_to_meetup <- c("Socializing and Communicating" = "Socializing",
                      "Participating in Sports, Exercise, and Recreation" = "Sports, Exercise and Leisure",
                      "Eating and Drinking" = "Health and Wellbeing", 
                      "Travel related to household activities" = "Parents and Family",
                      "Traveling" = "Traveling and Outdoors",
                      "Attending or Hosting Social Events" = "Events, Hobbies, and Passions")
  
  observeEvent(event_data("plotly_click", source = "treemapSource"), {
    click_data <- event_data("plotly_click", source = "treemapSource")
    req(click_data)
    
    clicked_category <- click_data$customdata
    if(is.list(clicked_category)) {
      clicked_category <- click_category[[1]]
    }
    
    meetup_category <- atus_to_meetup[[clicked_category]]
    if (!is.null(meetup_category)) {
      updateSelectInput(session, "activity_category", selected = meetup_category)
      
      updateTabsetPanel(session, "nav_tabs", selected = "NYC meetup.com Events")
    }
  })
  
  # Tab 2: Butterfly chart & Pie chart
  filtered_atus_bar <- reactive({
    data <- ATUS_data
    # time slot
    if (input$time_choice_bar != "All") {
      data <- data |> filter(time_slot == input$time_choice_bar)
    }
    
    # non-work time slider
    req(input$nonwork_slider)
    data <- data |> 
      filter(nonwork_time_hours >= input$nonwork_slider[1],
             nonwork_time_hours <= input$nonwork_slider[2])
    data
  })
  # butterfly bar chart
  output$top10Bar <- renderPlotly({
    df <- filtered_atus_bar()
    req(nrow(df) > 0)
    
   student_stats <- df |> 
      filter(student_status == "Student") |> 
      filter(!is.na(activity_detail))
    
    top_student_activities <- student_stats |> 
      count(activity_detail, name = "count") |> 
      mutate(prop = count / nrow(student_stats)) |> 
      slice_max(prop, n = 10) |> 
      arrange(prop)
    
    activity_order <- top_student_activities$activity_detail
    # based on the top 10 activities for students, made comparison between students and non-students
    non_student_stats <- df |> 
      filter(student_status == "Non-student") |> 
      filter(!is.na(activity_detail))
    
    top_nonstudent_activities <- non_student_stats |>
      filter(activity_detail %in% activity_order) |> 
      count(activity_detail, name = "count") |> 
      mutate(prop = count / nrow(non_student_stats))
    
    plot_data <- top_student_activities |> 
      select(activity_detail, prop_student = prop, count_student = count) |> 
      left_join(top_nonstudent_activities |> select(activity_detail, prop_non = prop, count_non = count), by = "activity_detail") |> 
      mutate(prop_non = ifelse(is.na(prop_non), 0, prop_non),
             count_non = ifelse(is.na(count_non), 0, count_non))
      
      plot_ly(
      data = plot_data, source = "butterflySource") |> 
        add_bars(
          x = ~ -prop_student,
          y = ~factor(activity_detail, levels = activity_order),
          name = "Student",
          orientation = 'h',
          marker = list(color = '#FF8296'),
          text = ~paste0(round(prop_student * 100, 1), "%"),
          textposition = "auto",
          customdata = ~count_student,
          hovertemplate = "<b>Count:</b> %{customdata}<extra></extra>") |> 
        add_bars(
          x = ~prop_non,
          y = ~factor(activity_detail, levels = activity_order),
          name = "Non-student",
          orientation = 'h',
          marker = list(color = '#75CDD8'), 
          text = ~paste0(round(prop_non * 100, 1), "%"),
          textposition = "auto",
          customdata = ~count_non,
          hovertemplate = "<b>Count:</b> %{customdata}<extra></extra>"
        ) |> 
        layout(
          barmode = 'overlay',
          xaxis = list(
            title = "Group",
            tickmode = "array",
            tickvals = c(-0.4, -0.2, 0, 0.2, 0.4), 
            ticktext = c("40%", "20%", "0%", "20%", "40%"),
            range = c(-0.5, 0.5)
          ),
          yaxis = list(title = ""),
          legend = list(x = 0.9, y = 0),
          margin = list(l = 150)
        )
      }
   )
      
  
  # pie chart when clicking bars
  selected_activity_data <- reactive({
    click_data <- event_data("plotly_click", source = "butterflySource")
    
    if (is.null(click_data)) return(NULL)
    selected_activity <- click_data$y
    df_subset <- filtered_atus_bar() |> 
      filter(activity_detail == selected_activity)
    list(
      name = selected_activity,
      data = df_subset
    )
  })
  
  output$selected_activity_name <- renderText({
    selected <- selected_activity_data()
    if (is.null(selected)) return("Click a bar above to see duration details")
    paste("Average Duration for:", selected$name)
  })
  
  output$activityPie <- renderPlotly({
    selected <- selected_activity_data()
    req(selected)
    
    df <- selected$data
    req(nrow(df) > 0)
    mean_duration <- mean(df$duration_hours, na.rm = T)
    mean_nonwork <- mean(df$nonwork_time_hours, na.rm = T)
    remaining_time <- max(0, mean_nonwork - mean_duration)
    
    pie_df <- data.frame(
      category = c("Activity Duration", "Remaining Time"),
      hours = c(mean_duration, remaining_time)
    )
    
    plot_ly(
      data = pie_df,
      labels = ~category,
      values = ~hours,
      type = 'pie',
      textinfo = 'label+percent',
      marker = list(colors = c("#673AB7", "#C8D1DB")),
      hovertemplate = "Average Duration Hours: %{value:.1f} hours<extra></extra>"
    ) |>
      layout(
        showlegend = T
      )
  })
  
  # Tab 3: Meetup Events
  meetup_to_show <- reactive({
    if (!exists("meetup_events")) return(NULL)
    data <- meetup_events
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
        if (is.null(input$activity_category)) {
      return(data)
    }
    
    if ("category" %in% names(data)) {
      data |> filter(category == input$activity_category)
    } else {
      data
    }
  })
  
  output$meetup_message <- renderText({
    data <- meetup_to_show()
    
    if (is.null(data) || nrow(data) == 0) {
      paste("No events found for category:", input$activity_category)
    } else {
      paste("Showing NYC meetup.com", input$activity_category, "events:")
    }
  })
  
  output$meetup_table <- renderDT({
    data <- meetup_to_show()
    req(!is.null(data), nrow(data) > 0)    
    
    cols_to_keep <- c("event_name", "group", "date", "category")
    data <- data %>% select(any_of(cols_to_keep))
    
    datatable(
      data,
      options = list(pageLength = 10, lengthChange = FALSE)
    )
  })
}