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
        choices = c("All", unique(as.character(ATUS_data$age_group))),
        selected = "All"),
      # Time Slot
      selectInput(
        inputId = "time_choice",
        label = "Select time slot:",
        choices = c("All", unique(as.character(ATUS_data$time_slot))),
        selected = "All"
      ),
      # Weekdays or Weekends
      radioButtons(
        inputId = "days_choice",
        label = "Select day option:",
        choices = c("All", unique(as.character(ATUS_data$wkday_wkend))),
        selected = "All"
      ),
      
      # Location
      selectInput(
        inputId = "location_choice",
        label   = "Select your location:",
        choices = c("All", unique(na.omit(as.character(ATUS_data$location_detail)))),
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
                 plotlyOutput("scatterPlot", height = "600px")),
      
      # meetup.com code
      tabPanel("NYC meetup.com Events",
               br(),
               sidebarLayout(
                 sidebarPanel(
                   width = 3, 
                   h4("Categories"),
                   selectInput(
                     inputId = "activity_category",
                     label = "Select Activity Category:",
                     choices = c(
                       "General" = "General",
                       "New Groups" = "New Group",
                       "Socializing" = "Socializing",
                       "Events, Hobbies, and Passions" = "Hobbies Passions",
                       "Sports, Exercise, and Leisure" = "Sports Exercise Leisure",
                       "Traveling and Outdoors" = "Traveling Outdoor",
                       "Career and Business" = "Career Business",
                       "Technology" = "Technology",
                       "Community and Environment" = "Community Environment",
                       "Identity and Language" = "Identity Language",
                       "Games" = "Games",
                       "Dancing" = "Dancing",
                       "Support and Coaching" = "Support Coaching",
                       "Music" = "Music",
                       "Health and Wellbeing" = "Health Wellbeing",
                       "Art and Entertainment" = "Art Entertainment",
                       "Science and Education" = "Science Education",
                       "Pets and Animals" = "Pets Animals",
                       "Religion and Spirituality" = "Religion Spirituality",
                       "Writing" = "Writing",
                       "Parents and Family" = "Parents Family",
                       "Movements and Politics" = "Movements Politics"
                     ),
                     selected = "General"
                   ),
                   p(class = "text-muted", "Filter for local NYC meetup.com events matching your interests!")
                 ),
                 mainPanel(
                   width = 9,
                   h4("Local NYC meetup.com Events"),
                   textOutput("meetup_message"),
                   br(),
                   DTOutput("meetup_table")
                 )
               )
        )
      )
    )
  )
)
