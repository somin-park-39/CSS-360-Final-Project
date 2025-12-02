library(shiny)
library(DT)
library(plotly)

ui <- fluidPage(
  # Title
  titlePanel("What's Today?"),
  sidebarLayout(
    sidebarPanel(
      # Age
      selectInput(
        inputId = "age_choice",
        label   = "Select your age:",
        choices = c("All", unique(as.character(activity_2020_24$age_group))),
        selected = "All"),
      # Time Slot
      selectInput(
        inputId = "time_choice",
        label = "Select time slot:",
        choices = c("All", unique(as.character(activity_2020_24$time_slot))),
        selected = "All"
      ),
      # Weekdays or Weekends
      radioButtons(
        inputId = "days_choice",
        label = "Select day option:",
        choices = c("All", unique(as.character(activity_2020_24$wkday_wkend))),
        selected = "All"
      ),
      
      # Location
      selectInput(
        inputId = "location_choice",
        label   = "Select your location:",
        choices = c("All", unique(na.omit(as.character(activity_2020_24$location_detail)))),
        selected = "All"),
      
      hr(),
      p(class = "text-muted", "click to zoom-in more detailed activities")
      ),
    
    mainPanel(
      h3("Activity Exploration"),
      tabsetPanel(
        tabPanel("Activity treemap",
                 br(),
                 plotlyOutput("treemapPlot", height = "600px")),
        
        tabPanel("Activities by Duration Hours",
                 br(),
                 p("x = Duration Hours, y = Number of People Who Enjoy the Activity"),
                 plotlyOutput("scatterPlot", height = "600px"))),
      
      # meetup
      br(),
      h3("Local Meetup events"),
      textOutput("meetup_message"),
      DTOutput("meetup_table")
    )
  )
)
