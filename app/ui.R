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
             ),
    
    # Tab 4: Heatmap comparing ATUS and meetup hours
    tabPanel(
      "Heatmap: ATUS vs Meetup",
      br(),
      
      sidebarLayout(
        sidebarPanel(
          
          h4("About this visualization"),
          p(
            "These heatmaps are aligned on the same hourly time scale (0–23) to compare ",
            "when people engage in different activities, based on the American Time Use Survey (ATUS), ",
            "with when Meetup events are scheduled.",
            "This allows exploration of whether Meetup events",
            "tend to occur during hours when people are most likely to be available for activities."
          ),
          
          hr(),
          
          h4("Heatmap Controls"),
          
          selectInput(
            "heat_activity_level",
            "Activity grouping:",
            choices = c(
              "Activity detail" = "activity_detail",
              "Broad classification" = "first_four_digits_classifications"
            ),
            selected = "activity_detail"
          ),
          
          sliderInput(
            "heat_top_n",
            "Top N activities (rows):",
            min = 5,
            max = 30,
            value = 12,
            step = 1
          ),
          
          selectInput(
            "heat_days_choice",
            "Day type:",
            choices = c("All", unique(as.character(ATUS_data$wkday_wkend))),
            selected = "All"
          ),
          
          radioButtons(
            "heat_view",
            "Show:",
            choices = c(
              "ATUS only" = "atus",
              "Meetup only" = "meetup",
              "Both (stacked)" = "both"
            ),
            selected = "both",
            inline = TRUE
          ),
          
          hr(),
          
          p(
            class = "text-muted",
            "Click any cell in the heatmap to see contributing rows in the table."
          )
        ),
        
        mainPanel(
          fluidRow(
            column(
              width = 8,
              plotlyOutput("heatmapPlot", height = "520px")
            ),
            column(
              width = 4,
              h4("Cell details"),
              DTOutput("heatmap_table")
            )
          )
        )
      )
    )
  )
)