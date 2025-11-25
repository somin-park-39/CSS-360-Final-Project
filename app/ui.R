library(shiny)
library(DT)

ui <- fluidPage(
  # Title
  titlePanel("Leisure Activity Suggestion App Prototype"),
  sidebarLayout(
    sidebarPanel(
      # Age
      selectInput(
        inputId = "age_choice",
        label   = "Select your age:",
        choices = 15:85
      ),
      # Activity Type
      selectInput(
        inputId = "activity_type",
        label   = "What type of activity do you want to do?",
        choices = c(
          "Any",
          sort(unique(as.character(ATUS_data$first_two_digits_classification))),
          sort(unique(as.character(ATUS_data$first_four_digits_classifications)))
        ),
        selected = "Any"
      ),
      # Location Preference
      selectInput(
        inputId = "location_pref",
        label   = "Where would you like to do your activity?",
        choices = c("Any Location", sort(unique(ATUS_data$location_detail))),
        selected = "Any Location"
      ),
      # Day of Week
      selectInput(
        inputId = "day_of_wk",
        label   = "What day of the week will you do your activity?",
        choices = c("Any day", as.character(unique(ATUS_data$day_of_wk))),
        selected = "Any day"
      ),
      # Free Time – Hours
      numericInput(
        inputId = "free_hours",
        label   = "How many HOURS of free time do you have?",
        min     = 0,
        max     = 24,
        value   = 0,
        step    = 1
      ),
      # Free Time – Minutes
      numericInput(
        inputId = "free_minutes",
        label   = "How many MINUTES of free time do you have?",
        min     = 0,
        max     = 59,
        value   = 0,
        step    = 1
      )
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