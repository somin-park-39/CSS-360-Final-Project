library(shiny)

library(DT)

ui <- fluidPage(
  # Title
  titlePanel(title = "Leisure Activity Suggestion App Prototype"),
  sidebarLayout(
    sidebarPanel(
      # Age
      sliderInput(
        inputId = "age_range",
        label = "Select your age:",
        min = 15,
        max = 85,
        value = c(18, 30),
        step = 1
      ),
      # Activity Type
      selectInput(
        inputId = "activity_type",
        label = "What type of activity do you want to do?",
        choices = c(
          "Any",
          sort(unique(ATUS_data$first_two_digits_classification)),
          sort(unique(ATUS_data$first_four_digits_classifications))
        ),
        selected = "Any"
      ),
      # Location Preference
      selectInput(
        inputId = "location_pref",
        label = "Where would you like to do your activity?",
        choices = c("Any Location", sort(unique(ATUS_data$location_detail))),
        selected = "Any Location"
      ),
      # Day of Week
      selectInput(
        inputId = "day_of_wk",
        label = "What day of the week will you do your activity?",
        choices = c("Any day", unique(ATUS_data$day_of_wk)),
        selected = "Any day"
      ),
      # Free Time
      sliderInput(
        inputId = "free_time",
        label = "How much free time do you have?",
        min = 1,
        max = 1435,
        value = 1,
        step = 5
      ),
      selectInput(
        inputId = "time_unit",
        label = "Is that in minutes or hours?",
        choices = c("Minutes", "Hours"),
        selected = "Minutes"
      ),
    ),
    mainPanel(
      h3("Suggested activity based on ATUS"),
      textOutput("activity_text"),
      br(),
      h3("Local Meetup events"),
      textOutput("meetup_message"),
      DTOutput("meetup_table")
    )
  )
)