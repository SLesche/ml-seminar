get_pbp_data <- function(player_name, id_map){
  player_info = id_map %>% 
    filter(PLAYERNAME == player_name)
  
  fg_id = player_info$IDFANGRAPHS
  mlb_id = player_info$MLBID
  mlb_info = mlb_people(mlb_id)
  debut_date = mlb_info$mlb_debut_date
  active = mlb_info$active
  active_time = c(lubridate::year(debut_date), lubridate::year(lubridate::today()))
  
  active_years = numeric()
  for (i in 1:(active_time[2] - active_time[1] + 1)){
    active_years[i] = as.numeric(active_time[1]) + i - 1
  }
  active_years = subset(active_years, active_years >= 2008)
  season_info = data.frame(begin = 1:length(active_years), end = 1:length(active_years))
  all_seasons = mlb_seasons_all()
  
  for (i in seq_along(active_years)){
    season_info$begin[i] = as.character(all_seasons[which(all_seasons$season_id == active_years[i]), "regular_season_start_date"])
    season_info$end[i] = as.character(all_seasons[which(all_seasons$season_id == active_years[i]), "regular_season_end_date"])
  }
  
  game_logs = list() 
  for (i in seq_along(active_years)){
    game_logs[[i]] = statcast_search_pitchers(
      start_date = season_info$begin[i],
      end_date = season_info$end[i],
      pitcherid = mlb_id
    )
  }
  
  entry_ids = c()
  for (i in seq_along(game_logs)){
    n_entries = nrow(game_logs[[i]])
    if (n_entries == 0){
      entry_ids = c(entry_ids, i)
    }
  }
  if (length(entry_ids) != 0){
    game_logs[[entry_ids]] = c()
  }
  
  pbp_data = data.table::rbindlist(game_logs, fill = TRUE) 
  
  final_data = pbp_data %>% 
    janitor::remove_empty() %>% 
    janitor::clean_names()
  
  return(final_data)
}

clean_pbp_data <- function(data){
  clean_data = data %>% 
    dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                  game_type == "R") %>% 
    dplyr::mutate(pfx_x_in_pv = 12*pfx_x,
                  pfx_z_in = 12*pfx_z)
  
  return(clean_data)
}


plot_pitch_movement <- function(data){
  data %>% 
    ggplot2::ggplot(ggplot2::aes(x = pfx_x_in_pv, y = pfx_z_in, color = pitch_name)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(size = 1.5, alpha = 0.25) +
    ggplot2::scale_x_continuous(limits = c(-25,25),
                                breaks = seq(-20,20, 5),
                                labels = scales::number_format(suffix = "\"")) +
    ggplot2::scale_y_continuous(limits = c(-25,25),
                                breaks = seq(-20,20, 5),
                                labels = scales::number_format(suffix = "\"")) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = paste("Pitch Movement"),
                  subtitle = "Pitcher's POV",
                  caption = "Data: Baseball Savant via baseballr", 
                  x = "Horizontal Break",
                  y = "Induced Vertical Break",
                  color = "Pitch Name")+
    theme_minimal()
  
}

plot_pitch_usage <- function(data){
  data %>% 
    group_by(
      game_year, pitch_name
    ) %>% 
    summarize(
      n = n()
    ) %>% 
    filter(grepl("[A-Za-z]", pitch_name)) %>% 
    group_by(
      game_year
    ) %>% 
    mutate(
      freq = n / sum(n)
    ) %>% 
    ggplot(
      aes(x = game_year, y = freq, color = pitch_name, group = pitch_name)
    ) +
    geom_line(linewidth = 1.5)+
    labs(
      title = "Pitch Usage",
      caption = "Data: Baseball Savant via baseballr", 
      x = "Year",
      y = "Percent of Pitches Thrown",
      color = "Pitch Name"
    )+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    theme_minimal()
}

plot_pitch_by_count <- function(data){
  data %>% 
    mutate(count = paste0(balls, "-", strikes)) %>% 
    group_by(
      count, pitch_name, 
    ) %>% 
    summarize(
      n = n()
    ) %>% 
    filter(grepl("[A-Za-z]", pitch_name)) %>% 
    group_by(
      count
    ) %>% 
    mutate(
      freq = n / sum(n)
    ) %>% 
    ggplot(
      aes(x = count, y = freq, fill = pitch_name)
    )+
    geom_bar(stat ="identity", position = "stack")+
    labs(
      title = "Pitch Usage by Count",
      caption = "Data: Baseball Savant via baseballr", 
      x = "Count",
      y = "Percent of Pitches Thrown",
      color = "Pitch Name"
    )+
    ggplot2::scale_y_continuous(labels = scales::percent)+
    theme_minimal()
}

plot_pitch_by_batter <- function(data){
  data %>% 
    group_by(
      stand, pitch_name, 
    ) %>% 
    summarize(
      n = n()
    ) %>% 
    filter(grepl("[A-Za-z]", pitch_name)) %>% 
    group_by(
      stand
    ) %>% 
    mutate(
      freq = n / sum(n)
    ) %>% 
    ggplot(
      aes(x = "", y = freq, fill = pitch_name)
    )+
    geom_bar(stat ="identity", width = 1)+
    facet_wrap(~stand) +  # Facet by batter hand (R and L)
    coord_polar("y", start = 0) +  # Make the plot circular (pie chart)
    theme_void() +  # Remove axis and background
    labs(
      title = "Pitch Usage by Batter Handedness (L vs R)",
      caption = "Data: Baseball Savant via baseballr", 
      x = "Count",
      y = "Percent of Pitches Thrown",
      fill = "Pitch Name"
    )
}

plot_pitch_velocity <- function(data){
  data %>% 
    mutate(velocity = release_speed*1.6093) %>% 
    ggplot(aes(x = velocity, fill = pitch_name))+
    geom_density(alpha = 0.5)+
    labs(
      title = "Pitch Velocity",
      caption = "Data: Baseball Savant via baseballr", 
      x = "Pitch Velocity (km/h)",
      y = "",
      fill = "Pitch Name"
    )+
    theme_minimal()
}

# Make a "Will it dong" Model
# Or my own xBA model, compare to the existing xBA

prep_for_ml <- function(data){
  # Make variable: distance from intended, which is mean of the position given some variables
  # Get average pitch mix as additional variable
  available_pitches = unique(data$pitch_type)
  relevant_variables = c(
    paste0("freq_", available_pitches),
    "game_year",
    "inning",
    "pitch_count_total",
    "pitch_number",
    "balls",
    "strikes",
    "stand",
    "opponent_team",
    "on_3b",
    "on_2b",
    "on_1b",
    "men_on_base",
    "runners_in_scoring_position",
    "outs_when_up",
    "inning",
    "inning_topbot",
    "bat_score",
    "fld_score",
    "score_difference",
    "if_fielding_alignment",
    "of_fielding_alignment"
  )
  lag_variables = c(
    "pitch_type", 
    "is_fastball",
    "release_speed", 
    "events", 
    "description", 
    "zone",
    "distance_from_intended",
    "delta_run_exp",
    "hit_location",
    "bb_type",
    "pfx_x",
    "pfx_z",
    "hit_distance_sc",
    "launch_speed",
    "launch_angle",
    "barrel",
    "sweet_spot",
    "hard_hit",
    "effective_speed",
    "release_spin_rate",
    "estimated_ba_using_speedangle",
    "estimated_woba_using_speedangle",
    "dodged_a_hit", # Estimated ba > 0.5
    "is_hit", # hit in last pa
    "is_base",
    "launch_speed_angle",
    "release_extension",
    "spin_axis",
    "delta_home_win_exp",
    "delta_run_exp",
    "yanked"
  )
  
  data[data == ""] = NA
  
  intended_target = data %>% 
    group_by(game_year, stand, pitch_type, balls, strikes) %>% 
    summarize(
      intended_x = mean(pfx_x_in_pv, na.rm = TRUE),
      intended_z = mean(pfx_z_in, na.rm = TRUE)
    ) %>% ungroup()
  
  pitch_mix = data %>% 
    group_by(game_year, stand, balls, strikes) %>%
    count(pitch_type) %>% 
    mutate(
      freq = n / sum(n)
    ) %>% 
    filter(pitch_type != "") %>% 
    pivot_wider(
      id_cols = c("game_year", "stand", "balls", "strikes"),
      names_from = pitch_type,
      values_from = freq,
      names_prefix = "freq_"
    )
  
  pitch_mix[is.na(pitch_mix)] = 0

  data_ml = data %>% 
    left_join(., intended_target) %>% 
    left_join(., pitch_mix) %>% 
    arrange(game_pk, inning, at_bat_number, pitch_number) %>% 
    group_by(game_pk) %>% 
    mutate(
      pitch_count_total = row_number()
    ) %>% 
    ungroup() %>% 
    select(game_pk, game_date, inning, at_bat_number, pitcher, batter, pitch_number, pitch_count_total,
           balls, strikes,
           pitch_name, description, delta_run_exp, everything()) %>% 
    mutate(
      across(starts_with("on_"), ~!is.na(.)),
    ) %>% 
    mutate(
      men_on_base = rowSums(select(., starts_with("on_"))),
      runners_in_scoring_position = ifelse(
        on_2b | on_3b,
        1,
        0
      ),
      score_difference = fld_score - bat_score,
      dodged_a_hit = ifelse(estimated_ba_using_speedangle > 0.5, 1, 0),
      distance_from_intended = sqrt((pfx_x_in_pv - intended_x)^2 + (pfx_z_in - intended_z)^2),
      opponent_team = ifelse(
        inning_topbot == "Bot",
        home_team,
        away_team
      ),
      sweet_spot = ifelse(launch_angle > 8 & launch_angle < 32, 1, 0),
      hard_hit = ifelse(launch_speed > 95, 1, 0),
      is_hit = ifelse(
        events %in% c("double", "home_run", "sac_fly", "single", "triple"),
        1,
        0
      ),
      is_base = ifelse(
        events %in% c("double", "home_run", "sac_fly", "single", "triple",
                      "walk", "hit_by_pitch", "intent_walk"),
        1,
        0
      ),
      is_fastball = ifelse(
        pitch_type %in% c("FC", "FF", "SI"),
        1,
        0
      )
    ) %>% 
    mutate(
      yanked = ifelse(distance_from_intended > 7, 1, 0),
      barrel = ifelse(sweet_spot + hard_hit == 2, 1, 0)
    ) %>% 
    mutate(
      across(
        c("game_year",
          "stand",
          "opponent_team",
          "inning_topbot",
          "events", 
          "description", 
          "zone",
          "hit_location",
          "bb_type",
          "if_fielding_alignment",
          "of_fielding_alignment"
          ),
        ~addNA(factor(.))
      )
    ) %>% 
    mutate(
      across(
        c("game_year",
          "stand",
          "opponent_team",
          "inning_topbot",
          "events", 
          "description", 
          "zone",
          "hit_location",
          "bb_type",
          "if_fielding_alignment",
          "of_fielding_alignment"
        ),
        ~fct_lump_min(.,min = 100)
      )
    ) %>% 
    select(
      all_of(c("game_pk", relevant_variables, lag_variables))
    )
  
  data_ml[is.na(data_ml)] = 0
  
  data_lagged_ml = data_ml %>%
    group_by(
      game_pk, inning
    ) %>% 
    mutate(
      across(lag_variables, ~lag(., 1), .names = "lag1_{.col}"),
      across(lag_variables, ~lag(., 2), .names = "lag2_{.col}"),
      across(lag_variables, ~lag(., 3), .names = "lag3_{.col}"),
      across(lag_variables, ~lag(., 4), .names = "lag4_{.col}"),
      across(lag_variables, ~lag(., 5), .names = "lag5_{.col}"),
    ) %>% 
    ungroup() %>% 
    select(-all_of(lag_variables[!lag_variables %in% c("is_fastball", "pitch_type")]))
    
    return(data_lagged_ml)
}
