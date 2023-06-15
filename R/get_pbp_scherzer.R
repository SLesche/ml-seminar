library(baseballr)
library(tidyverse)

id_map <- data.table::fread("baseball/player_id_map.csv") %>% 
  rename("bbref_id" = BREFID)

scherzer_info <- id_map %>% 
  filter(PLAYERNAME == "Max Scherzer")

scherzer_fg_id <- scherzer_info$IDFANGRAPHS
scherzer_mlb_id <- scherzer_info$MLBID
scherzer_mlb_info <- mlb_people(scherzer_mlb_id)
scherzer_debut_date <- scherzer_mlb_info$mlb_debut_date
scherzer_active <- scherzer_mlb_info$active
scherzer_active_time <- c(lubridate::year(scherzer_debut_date), lubridate::year(lubridate::today()))

active_years <- numeric()
for (i in 1:(scherzer_active_time[2] - scherzer_active_time[1] + 1)){
  active_years[i] = as.numeric(scherzer_active_time[1]) + i - 1
}

season_info <- data.frame(begin = 1:length(active_years), end = 1:length(active_years))
all_seasons <- mlb_seasons_all()

for (i in seq_along(active_years)){
  season_info$begin[i] = as.character(all_seasons[which(all_seasons$season_id == active_years[i]), "regular_season_start_date"])
  season_info$end[i] = as.character(all_seasons[which(all_seasons$season_id == active_years[i]), "regular_season_end_date"])
}

scherzer_game_logs <- list() 
for (i in seq_along(active_years)){
  scherzer_game_logs[[i]] = statcast_search_pitchers(
    start_date = season_info$begin[i],
    end_date = season_info$end[i],
    pitcherid = scherzer_mlb_id
  )
}

scherzer_game_ids <- numeric()
for (i in seq_along(scherzer_game_logs)){
  scherzer_game_ids = c(scherzer_game_ids, unique(scherzer_game_logs[[i]]$game_pk))
}

scherzer_pbp <- list()
for (i in seq_along(scherzer_game_ids)){
  scherzer_pbp[[i]] = try(mlb_pbp(scherzer_game_ids[i]))
}

# Need to get the ballpark factors

