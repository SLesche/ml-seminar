library(tidyverse)
library(baseballr)

source("./hausarbeit/helper_functions.R")
id_map <- data.table::fread("./hausarbeit/data/player_id_map.csv") %>% 
  rename("bbref_id" = BREFID)
scherzer_data <- get_pbp_data("Max Scherzer", id_map)

cole_data <- get_pbp_data("Gerrit Cole", id_map)

darvish_data <- get_pbp_data("Yu Darvish", id_map)

verlander_data <- get_pbp_data("Justin Verlander", id_map)

kimbrel_data <- get_pbp_data("Craig Kimbrel", id_map)

clean_data <- kimbrel_data %>% clean_pbp_data()

plot_pitch_movement(clean_data %>% filter(game_year == 2023))

plot_pitch_usage(clean_data)

plot_pitch_by_count(clean_data %>% filter(game_year == 2018))

plot_pitch_by_batter(clean_data %>% filter(game_year == 2018))

# Recoding Data
# Get info on previous pitch outcomes, previous at bat outcomes


# Which ML Models to try out?
# Random Forest
# Decision Tree Model
# Multinomial Regression
# Logistic Regression for 2 Pitch (Fastball/Offspeed) (LASSO/RIDGE)
# k-nearest Neighbor
# Neural Net
