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

scherzer_errors <- numeric()
for (i in seq_along(scherzer_pbp)){
  if (all(is.na(scherzer_pbp[[i]][2]))){
    scherzer_errors = c(scherzer_errors, i)
  }
}

scherzer_clean_pbp <- scherzer_pbp[- scherzer_errors]

scherzer_pbp_data <- data.table::rbindlist(scherzer_clean_pbp, fill = TRUE) 

scherzer_only_data <- scherzer_pbp_data %>% 
  filter(matchup.pitcher.id == as.character(scherzer_mlb_id)) %>% 
  janitor::remove_empty() %>% 
  janitor::clean_names()

data.table::fwrite(scherzer_only_data, "baseball/scherzer_data.csv")

# Need to get the ballpark factors

scherzer_only_data <- data.table::fread("baseball/scherzer_data.csv")

# Important columns
# Batter Handedness
# Pitch count
# Count
# type Previous Pitch
# Outs
# Men-on-base
# Catcher?
# Result previous pitch
# result previous at-bat?
# inning
# how well was previous pitch located?

scherzer_temp <- scherzer_only_data %>%
  transmute(
    game_id = game_pk,
    inning = about_inning,
    at_bat = at_bat_index,
    pitch_number = pitch_number,
    batter_handedness = matchup_bat_side_code,
    pitch_count_balls = count_balls_start,
    pitch_count_strikes = count_strikes_start,
    pitch_type = details_type_code,
    pitch_type_description = details_type_description,
    current_outs = count_outs_start,
    result_pitch_description = details_description,
    result_pitch_zone = pitch_data_zone,
    result_pitch_code = details_code,
    result_at_bat = result_event_type,
    man_on_first = !is.na(matchup_post_on_first_id),
    man_on_second = !is.na(matchup_post_on_second_id),
    man_on_third = !is.na(matchup_post_on_third_id),
    location_x = pitch_data_coordinates_x,
    location_y = pitch_data_coordinates_y,
    strikezone_top = pitch_data_strike_zone_top,
    strikezone_bottom = pitch_data_strike_zone_bottom
  )

scherzer_clean <- scherzer_temp %>% 
  arrange(game_id, inning, at_bat, pitch_number) %>% 
  group_by(game_id) %>% 
  mutate(
    pitch_count_total = row_number()
  ) %>% 
  group_by(game_id, inning) %>% 
  mutate(
    previous_pitch_type = lag(pitch_type),
    previous_pitch_zone = lag(result_pitch_zone),
    previous_pitch_code = lag(result_pitch_code),
  ) %>% 
  group_by(game_id, inning, at_bat) %>% 
  mutate(
    true_balls = lag(pitch_count_balls),
    true_strikes = lag(pitch_count_strikes)
  ) %>% 
  ungroup() %>% 
  mutate(
    true_balls = ifelse(is.na(true_balls) & is.na(true_strikes), 0, true_balls),
    true_strikes = ifelse(true_balls == 0 & is.na(true_strikes), 0, true_strikes)
  ) %>% 
  filter(!is.na(result_pitch_zone), true_strikes != 3, true_balls != 4) %>% 
  rowwise() %>% 
  mutate(men_on_base = sum(man_on_first, man_on_second, man_on_third)) %>% 
  ungroup() %>% 
  mutate(count = factor(paste0(true_balls, ", ", true_strikes)))

# For categorical predictors, remove low frequency events
# Combine strikes and balls to count predictor variable


scherzer_clean %>% 
  filter(pitch_type %in% c("FF", "CU", "SL")) %>% 
  ggplot(
    aes(x = location_x, y = location_y, color = factor(result_pitch_zone))
  ) +
  geom_point()+
  geom_hline(yintercept = mean(scherzer_clean$strikezone_top))

filter_uncommon_levels <- function(variable){
  
}
# Test multinom model
data_multinom <- scherzer_clean %>% 
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "SL"), previous_pitch_type %in% c("CH", "CU", "FC", "FF", "SL"))

multinom <- nnet::multinom(pitch_type ~ pitch_count_total + batter_handedness, data = data_multinom)


predict <- table(data_multinom$pitch_type, predict(multinom))

# Test stan model
data_stan <- scherzer_clean %>% 
  filter(
    pitch_type %in% c("CH", "CU", "FC", "FF", "SL"),
    previous_pitch_type %in% c("CH", "CU", "FC", "FF", "SL")
  ) %>% 
  mutate(
    offspeed = ifelse(pitch_type %in% c("FC", "FF"), 0, 1)
  ) %>% 
  head(1000)

design <- model.matrix(~batter_handedness+count+pitch_count_total+previous_pitch_type, data = data_stan)
list_stan <- list(
  N = nrow(design),
  # ncat = length(unique(data_stan$pitch_type)),
  K = ncol(design),
  y = data_stan$offspeed,
  X = design
)

params <- colnames(design)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

simple_model <- rstan::stan(
  file = "logit_model.stan",
  data = list_stan
)

plot(simple_model, par=paste0("beta[", 1:18, "]"), plotfun="trace", inc_warmup=TRUE)
plot(simple_model, par=paste0("beta[", 1:18, "]"), show_density = TRUE)
params

posterior_samples <- extract(simple_model)

predictions <- colMeans(posterior_samples$y_rep)

multivariate_design <- model.matrix(~batter_handedness, data = data)
list_multivariate_stan <- list(
  N = nrow(multivariate_design),
  # ncat = length(unique(data_stan$pitch_type)),
  K = ncol(multivariate_design),
  y = data.frame(data_stan$offspeed,
  X = design
)