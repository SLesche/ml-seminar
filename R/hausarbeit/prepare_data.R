library(tidyverse)
library(baseballr)

source("./hausarbeit/helper_functions.R")
id_map <- data.table::fread("./hausarbeit/data/player_id_map.csv") %>% 
  rename("bbref_id" = BREFID)

scherzer_data <- get_pbp_data("Max Scherzer", id_map)
data.table::fwrite(scherzer_data, "./hausarbeit/data/scherzer_data.csv")

hader_data <- get_pbp_data("Josh Hader", id_map)
data.table::fwrite(hader_data, "./hausarbeit/data/hader_data.csv")

cole_data <- get_pbp_data("Gerrit Cole", id_map)
data.table::fwrite(cole_data, "./hausarbeit/data/cole_data.csv")

darvish_data <- get_pbp_data("Yu Darvish", id_map)
data.table::fwrite(darvish_data, "./hausarbeit/data/darvish_data.csv")

verlander_data <- get_pbp_data("Justin Verlander", id_map)
data.table::fwrite(verlander_data, "./hausarbeit/data/verlander_data.csv")

kimbrel_data <- get_pbp_data("Craig Kimbrel", id_map)
data.table::fwrite(kimbrel_data, "./hausarbeit/data/kimbrel_data.csv")

clean_data <- scherzer_data %>% clean_pbp_data()

plot_pitch_usage(clean_data)

plot_pitch_movement(clean_data %>% filter(game_year > 2018))

plot_pitch_by_count(clean_data %>% filter(game_year > 2018))

plot_pitch_by_batter(clean_data %>% filter(game_year > 2018))

plot_pitch_velocity(clean_data %>% filter(game_year > 2018))

# Recoding Data
# Get info on previous pitch outcomes, previous at bat outcomes


# Which ML Models to try out?
# Random Forest
# Decision Tree Model
# Multinomial Regression
# Logistic Regression for 2 Pitch (Fastball/Offspeed) (LASSO/RIDGE)
# k-nearest Neighbor
# Neural Net
