library(tidyverse)
library(lubridate)

ATUS_data <- read_csv("data/atus_activity_2020_24.csv", show_col_types = FALSE)
meetup_events <- read_csv("data/meetup_art_events.csv", show_col_types = FALSE) %>% filter(!is.na(event_name) & event_name != "")