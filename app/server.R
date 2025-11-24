library(shiny)
library(dplyr)
library(DT)

server <- function(input, output, session) {
  # Convert free time to minutes because ATUS uses minutes
  free_minutes <- reactive({
    if (is.null(input$free_time) || is.null(input$time_unit)) {
      return(NA_real_)
    }
    if (input$time_unit == "Hours") {
      input$free_time * 60
    } else {
      input$free_time
    }
  })
  # Filtering ATUS data based on user's inputs
  filtered_atus <- reactive({
    data <- ATUS_data
    # Filtering by age range
    if (!is.null(input$age_range)) {
      data <- data %>%
        filter(
          !is.na(age),
          age >= input$age_range[1],
          age <= input$age_range[2]
        )
    }
    # Filtering by activity type
    if (!is.null(input$activity_type) && input$activity_type != "Any") {
      data <- data %>%
        filter(
          first_two_digits_classification == input$activity_type |
            first_four_digits_classifications == input$activity_type)
    }
    # Filtering by location preference
    if (!is.null(input$location_pref) && input$location_pref != "Any Location") {
      data <- data %>%
        filter(location_detail == input$location_pref)
    }
    # Filtering by day of the week
    if (!is.null(input$day_of_wk) && input$day_of_wk != "Any day") {
      data <- data %>%
        filter(day_of_wk == input$day_of_wk)
    }
    # Filtering by duration
    fm <- free_minutes()
    if (!is.na(fm)) {
      data <- data %>%
        filter(!is.na(duration), duration <= fm)
    }
    data
  })
  # Choosing a suggested activity from the filtered ATUS data
  suggested_activity <- reactive({
    data <- filtered_atus()
    if (nrow(data) == 0) {
      return(NULL)
    }
    # Rows need activity details
    data <- data %>% filter(!is.na(activity_detail))
    if (nrow(data) == 0) {
      return(NULL)
    }
    # Randomly sample one row (FOR NOW, might get more complicated later)
    data %>% slice_sample(n = 1)
  })
  # Text output describing the suggested activity
  output$activity_text <- renderText({
    row <- suggested_activity()
    if (is.null(row)) {
      return("We couldn't find an activity that matches all of your choices.")
    }
    activity_name <- row$activity_detail[1]
    location_name <- if (!is.null(row$location_detail[1]) && !is.na(row$location_detail[1])) row$location_detail[1] else "various locations"
    duration_minutes <- if (!is.null(row$duration[1]) && !is.na(row$duration[1])) round(row$duration[1]) else NA_real_
    
    parts <- c(
      paste0("Suggested Activity: ", activity_name, "."),
      paste0("Suitable Location: ", location_name, ".")
    )
    if (!is.na(duration_minutes)) {
      parts <- c(parts, paste0("People usually spend about ", duration_minutes, " minutes on this activity."))
    }
    paste(parts, collapse = " ")
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