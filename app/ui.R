library(shiny)
library(DT)
library(plotly)

ui <- fluidPage(
  # Title
  titlePanel("What's Today?"),
  
  tabsetPanel(id = "nav_tabs",
    # Tab 1:  Treemap
    tabPanel("Exploring Activities",
             br(),
             sidebarPanel(
               h4("Filters"),
               # Age
               selectInput(
                 inputId = "age_choice",
                 label   = "Select your age:",
                 choices = c("All", unique(as.character(ATUS_data$age_group))),
                 selected = "All"),
               # Time Slot
               selectInput(
                 inputId = "time_choice",
                 label = "Select time:",
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
               p(class = "text-muted", "1. Click to zoom-in more detailed activity options.", br(), "2. Click subcategories to see MeetUp events.")),
               
               mainPanel(
                 plotlyOutput("treemapPlot", height = "600px")
                 )
               ),
    # Tab 2: Bar Chart for Top Activities
    tabPanel("Top Activities for Students & Non-students",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Filters"),
                 # time
                 selectInput(
                   inputId = "time_choice_bar",
                   label = "Select time:",
                   choices = c("All",
                               unique(as.character(ATUS_data$time_slot))),
                   selected = "All"
                   ),
                 # nonwork time hours
                 sliderInput(
                   inputId = "nonwork_slider",
                   label = "Non-work Time (Hours):",
                   min = 0,
                   max = 24,
                   value = c(0, 24),
                   step = 0.5
                   ),
                 
                 hr(),
                 p(class = "text-muted", "Click on a bar in the chart to see duration details.")
                 ),
               
               mainPanel(
                 # bar chart
                 h3("Top 10 Popular Activities: Student vs Non-student"),
                 plotlyOutput("top10Bar", height = "500px"),
                 br(), hr(), br(),
                 
                 # pie chart
                 h4(textOutput("selected_activity_name")),
                 plotlyOutput("activityPie", height = "500px")
                 )
               )
             ),
    
    
    # Tab 3: Meetup Events
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
                 p(class = "text-muted", "Filter for local NYC meetup.com events matching your interests.")
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