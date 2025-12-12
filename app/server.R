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
      clicked_category <- clicked_category[[1]]
    }
    
    if(!is.null(clicked_category) && (clicked_category %in% names(atus_to_meetup))) {
    
    meetup_category <- atus_to_meetup[[clicked_category]]
    if (!is.null(meetup_category)) {
      updateSelectInput(session, "activity_category", selected = meetup_category)
      
      updateTabsetPanel(session, "nav_tabs", selected = "NYC meetup.com Events")}
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
  # Tab 4: Heatmap
  extract_hour_from_timeslot <- function(ts) {
    if (is.na(ts) || ts == "") return(NA_integer_)
    
    if (is.numeric(ts)) {
      hh <- as.integer(ts)
      if (!is.na(hh) && hh >= 0 && hh <= 23) return(hh)
    }
    
    s <- as.character(ts)
    m <- str_match(s, "\\b(\\d{1,2})\\b")
    
    if (!is.na(m[1, 2])) {
      hh <- as.integer(m[1, 2])
      if (!is.na(hh) && hh >= 0 && hh <= 23) return(hh)
    }
    
    m2 <- str_match(s, "(\\d{1,2}):(\\d{2})")
    if (!is.na(m2[1, 2])) {
      hh <- as.integer(m2[1, 2])
      if (!is.na(hh) && hh >= 0 && hh <= 23) return(hh)
    }
    
    NA_integer_
  }
  
  extract_hour_from_meetup_date <- function(dt_string) {
    if (is.na(dt_string) || dt_string == "") return(NA_integer_)
    
    s <- as.character(dt_string)
    s_norm <- str_replace_all(s, "[\u2022\u00B7·•]", " | ")
    s_norm <- str_replace_all(s_norm, "\\s+\\|\\s+", " | ")
    
    parts <- unlist(str_split(s_norm, "\\s*\\|\\s*"))
    parts2 <- unlist(str_split(parts[length(parts)], "\\s+on\\s+"))
    candidate <- tail(parts2, 1)
    candidate <- str_trim(candidate)
    
    matches <- str_match_all(
      candidate,
      "(?i)(\\b\\d{1,2}(?::\\d{2})?\\s*(?:AM|PM)\\b)"
    )[[1]]
    
    if (nrow(matches) > 0) {
      time_str <- matches[nrow(matches), 2]
      
      if (!str_detect(time_str, ":")) {
        time_str <- str_replace(
          time_str,
          "(?i)(\\d{1,2})\\s*(AM|PM)",
          "\\1:00 \\2"
        )
      }
      
      parsed <- NA
      try({
        parsed <- parse_date_time(
          time_str,
          orders = c("I:M p", "I p"),
          tz = "UTC"
        )
      }, silent = TRUE)
      
      if (!is.na(parsed)) return(hour(parsed))
      
      m2 <- str_match(
        time_str,
        "(?i)(\\d{1,2})(?::(\\d{2}))?\\s*(AM|PM)"
      )
      
      if (!is.na(m2[1, 2])) {
        hh <- as.integer(m2[1, 2])
        ampm <- toupper(m2[1, 3])
        
        if (!is.na(hh) && ampm %in% c("AM", "PM")) {
          if (ampm == "AM") {
            if (hh == 12) hh <- 0
          } else {
            if (hh != 12) hh <- hh + 12
          }
          if (hh >= 0 && hh <= 23) return(hh)
        }
      }
    }
    
    matches24 <- str_match_all(candidate, "(\\b\\d{1,2}:\\d{2}\\b)")[[1]]
    if (nrow(matches24) > 0) {
      last24 <- matches24[nrow(matches24), 2]
      hh <- as.integer(str_split(last24, ":")[[1]][1])
      if (!is.na(hh) && hh >= 0 && hh <= 23) return(hh)
    }
    
    NA_integer_
  }
  
  heatmap_data <- reactive({
    
    atus_df <- filtered_atus()
    
    if (!is.null(input$heat_days_choice) && input$heat_days_choice != "All") {
      atus_df <- atus_df |> filter(wkday_wkend == input$heat_days_choice)
    }
    
    if ("start_hour" %in% names(atus_df)) {
      atus_df <- atus_df |> mutate(.hour = as.integer(start_hour))
    } else if ("time_slot" %in% names(atus_df)) {
      atus_df <- atus_df |> mutate(.hour = vapply(time_slot, extract_hour_from_timeslot, integer(1)))
    } else {
      atus_df <- atus_df |> mutate(.hour = NA_integer_)
    }
    
    activity_col <- input$heat_activity_level
    if (!(activity_col %in% names(atus_df))) activity_col <- names(atus_df)[1]
    
    atus_summary <- atus_df |>
      filter(!is.na(.hour)) |>
      group_by(activity = .data[[activity_col]], hour = .hour) |>
      summarise(
        n = n(),
        avg_minutes = mean(duration_hours, na.rm = TRUE) * 60,
        .groups = "drop"
      )
    
    activity_totals <- atus_df |>
      filter(!is.na(.data[[activity_col]])) |>
      group_by(activity = .data[[activity_col]]) |>
      summarise(total = n(), .groups = "drop")
    
    atus_summary <- atus_summary |>
      left_join(activity_totals, by = "activity") |>
      mutate(pct = ifelse(is.na(total) | total == 0, 0, n / total * 100))
    
    top_acts <- atus_df |>
      filter(!is.na(.data[[activity_col]])) |>
      count(activity = .data[[activity_col]], name = "freq") |>
      arrange(desc(freq)) |>
      slice_head(n = input$heat_top_n) |>
      pull(activity)
    
    atus_summary <- atus_summary |> filter(activity %in% top_acts)
    
    atus_matrix <- atus_summary |>
      tidyr::complete(activity, hour = 0:23, fill = list(n = 0, avg_minutes = 0, pct = 0)) |>
      arrange(activity, hour)
    
    meetup_df <- NULL
    meetup_summary <- NULL
    
    if (exists("meetup_events")) {
      meetup_df <- meetup_events |>
        mutate(.hour = vapply(date, extract_hour_from_meetup_date, integer(1)))
      
      meetup_summary <- meetup_df |>
        filter(!is.na(.hour)) |>
        group_by(hour = .hour) |>
        summarise(event_count = n(), .groups = "drop")
    }
    
    list(
      atus = atus_matrix,
      meetup = meetup_summary,
      top_activities = top_acts,
      raw_atus = atus_df,
      raw_meetup = meetup_df
    )
  })
  
  output$heatmapPlot <- renderPlotly({
    
    hd <- heatmap_data()
    view <- input$heat_view
    traces <- list()
    
    if (view %in% c("atus", "both")) {
      atus_df <- hd$atus
      req(nrow(atus_df) > 0)
      
      mat_df <- atus_df |>
        select(activity, hour, pct) |>
        tidyr::pivot_wider(names_from = hour, values_from = pct)
      
      hours <- as.character(0:23)
      missing_hours <- setdiff(hours, colnames(mat_df))
      if (length(missing_hours) > 0) {
        for (mh in missing_hours) mat_df[[mh]] <- 0
      }
      
      mat_df <- mat_df |> select(activity, all_of(hours))
      mat <- as.matrix(mat_df[, hours])
      rownames(mat) <- mat_df$activity
      
      p_atus <- plot_ly(
        x = 0:23,
        y = rownames(mat),
        z = mat,
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = paste(
          "<b>%{y}</b><br>",
          "Hour: %{x}<br>",
          "Percent: %{z:.1f}%<extra></extra>"
        ),
        showscale = TRUE
      )
      
      traces <- c(traces, list(p_atus))
    }
    
    if (view %in% c("meetup", "both")) {
      meetup_summary <- hd$meetup
      
      if (!is.null(meetup_summary) && nrow(meetup_summary) > 0) {
        counts <- rep(0, 24)
        counts[meetup_summary$hour + 1] <- meetup_summary$event_count
        mat_meetup <- matrix(counts, nrow = 1)
        
        p_meetup <- plot_ly(
          x = 0:23,
          y = "Meetup events",
          z = mat_meetup,
          type = "heatmap",
          colorscale = "Blues",
          hovertemplate = paste(
            "<b>Meetup events</b><br>",
            "Hour: %{x}<br>",
            "Count: %{z}<extra></extra>"
          ),
          showscale = TRUE
        )
        
        traces <- c(traces, list(p_meetup))
      } else {
        p_empty <- plot_ly(
          x = 0:23,
          y = "Meetup events",
          z = matrix(0, nrow = 1, ncol = 24),
          type = "heatmap",
          colorscale = "Blues",
          hoverinfo = "text",
          text = "0 events",
          showscale = FALSE
        )
        
        traces <- c(traces, list(p_empty))
      }
    }
    
    if (length(traces) == 2 && input$heat_view == "both") {
      subplot(
        traces[[1]],
        traces[[2]],
        nrows = 2,
        shareX = TRUE,
        titleY = TRUE,
        heights = c(0.7, 0.3)
      ) |>
        layout(
          title = "ATUS activity intensity (top) and Meetup event counts (bottom)",
          xaxis = list(title = "Hour")
        )
    } else {
      traces[[1]]
    }
  })
  
  output$heatmap_table <- renderDT({
    
    click <- event_data("plotly_click", source = NULL)
    hd <- heatmap_data()
    
    if (is.null(click) || length(click) == 0) {
      df <- hd$raw_atus
      
      if (!is.null(hd$top_activities)) {
        df2 <- df |>
          filter(.data[[input$heat_activity_level]] %in% hd$top_activities)
        
        tbl <- df2 |>
          count(!!sym(input$heat_activity_level), name = "n") |>
          arrange(desc(n))
        
        colnames(tbl)[1] <- "activity"
        
        return(
          datatable(
            tbl,
            options = list(pageLength = 10, lengthChange = FALSE)
          )
        )
      } else {
        return(
          datatable(
            data.frame(Message = "No ATUS data available"),
            options = list(dom = "t")
          )
        )
      }
    }
    
    clicked_x <- click$x
    clicked_y <- click$y
    
    if (is.character(clicked_y) && clicked_y == "Meetup events") {
      meetup_df <- hd$raw_meetup
      
      if (is.null(meetup_df) || nrow(meetup_df) == 0) {
        return(
          datatable(
            data.frame(Message = "No meetup events for selection"),
            options = list(dom = "t")
          )
        )
      }
      
      sel_hour <- as.integer(clicked_x)
      
      df_sel <- meetup_df |>
        mutate(.hour = vapply(date, extract_hour_from_meetup_date, integer(1))) |>
        filter(.hour == sel_hour)
      
      if (nrow(df_sel) == 0) {
        return(
          datatable(
            data.frame(Message = paste("No meetup events at hour", sel_hour))
          )
        )
      }
      
      cols <- intersect(
        c("event_name", "group", "date", "category", "link"),
        colnames(df_sel)
      )
      
      datatable(
        df_sel[, cols, drop = FALSE],
        options = list(pageLength = 5)
      )
    } else {
      sel_hour <- as.integer(clicked_x)
      sel_activity <- as.character(clicked_y)
      
      df <- hd$raw_atus
      
      if (!(".hour" %in% names(df))) {
        if ("start_hour" %in% names(df)) {
          df <- df |> mutate(.hour = as.integer(start_hour))
        } else {
          df <- df |> mutate(.hour = vapply(time_slot, extract_hour_from_timeslot, integer(1)))
        }
      }
      
      df_sel <- df |>
        filter(
          .hour == sel_hour,
          .data[[input$heat_activity_level]] == sel_activity
        )
      
      if (nrow(df_sel) == 0) {
        return(
          datatable(
            data.frame(
              Message = paste("No ATUS rows for", sel_activity, "at hour", sel_hour)
            )
          )
        )
      }
      
      cols_to_show <- intersect(
        c(
          "id",
          input$heat_activity_level,
          "time_slot",
          "start_time",
          "duration_hours",
          "nonwork_time_hours"
        ),
        colnames(df_sel)
      )
      
      datatable(
        df_sel[, cols_to_show, drop = FALSE],
        options = list(pageLength = 5)
      )
    }
  })
}