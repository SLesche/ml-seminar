library(tidyverse)
library(baseballr)

source("./hausarbeit/helper_functions.R")
id_map <- data.table::fread("./hausarbeit/data/player_id_map.csv") %>% 
  rename("bbref_id" = BREFID)

# scherzer_data <- get_pbp_data("Max Scherzer", id_map)
# data.table::fwrite(scherzer_data, "./hausarbeit/data/scherzer_data.csv")
scherzer_data <- data.table::fread("./hausarbeit/data/scherzer_data.csv")

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
ml_data <- clean_data %>% filter(game_year > 2018) %>%  prep_for_ml()

ml_ohe <- mltools::one_hot(data.table::as.data.table(ml_data)) %>% 
  mutate(pitch_type = fct_lump_min(addNA(pitch_type), 100))


split_data <- rsample::initial_validation_split(ml_ohe, c(0.9, 0.05))

training_data <- rsample::training(split_data)
validation_data <- rsample::validation(split_data)
testing_data <- rsample::testing(split_data)


# Which ML Models to try out?
# Random Forest
# Decision Tree Model
# Multinomial Regression
# Logistic Regression for 2 Pitch (Fastball/Offspeed) (LASSO/RIDGE)
# k-nearest Neighbor
# Neural Net
