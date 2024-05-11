library(baseballr)
library(tidyverse)
pitcher_overall <- baseballr::bref_daily_pitcher("2022-04-07", "2022-10-02")

id_map <- data.table::fread("baseball/player_id_map.csv") %>% 
  rename("bbref_id" = BREFID)

pitcher_overall_id <- pitcher_overall %>% 
  left_join(., id_map)

baseballr::pitcher_game_logs_fg(1943, 2022) %>% View()

mlb_league_leader_types()

mlb_seasons_all() # for season info

mlb_stats()

data <- mlb_pbp(318647)

# With this pitch-by-pitch I can do anything!
# Maybe focus on a specific pitcher for the beginning?
# Like predict the pitches of Max Scherzer
colnames(data)

test <- statcast_search_pitchers(
  start_date = scherzer_debut_date,
  end_date = lubridate::today(),
  pitcherid = scherzer_mlb_id
  )
