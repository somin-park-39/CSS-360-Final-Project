library(tidyverse)
library(lubridate)
ATUS_data <- read_csv("data/atus_activity_2020_24.csv", show_col_types = FALSE)
meetup_events <- bind_rows(
  read_csv("data/meetup_events_general.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_new_group.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_socializing.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_hobbies_passions.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_sports_exercise_leisure.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_traveling_outdoor.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_career_business.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_technology.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_community_environment.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_identity_language.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_games.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_dancing.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_support_coaching.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_music.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_health_wellbeing.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_art_entertainment.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_science_education.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_pets_animals.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_religion_spirituality.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_writing.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_parents_family.csv", show_col_types = FALSE),
  read_csv("data/meetup_events_movements_politics.csv", show_col_types = FALSE)
)

