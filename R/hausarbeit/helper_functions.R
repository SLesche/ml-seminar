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
