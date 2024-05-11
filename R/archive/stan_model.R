library(tidyverse)
library(rstan)
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

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
    result_pitch_zone = factor(pitch_data_zone),
    result_pitch_code = details_code,
    result_at_bat = result_event_type,
    man_on_first = !is.na(matchup_post_on_first_id),
    man_on_second = !is.na(matchup_post_on_second_id),
    man_on_third = !is.na(matchup_post_on_third_id),
    location_x = pitch_data_coordinates_x,
    location_y = pitch_data_coordinates_y,
    strikezone_top = pitch_data_strike_zone_top,
    strikezone_bottom = pitch_data_strike_zone_bottom,
    pitch_velo = pitch_data_start_speed,
    spinrate = pitch_data_breaks_spin_rate
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
    previous_pitch_x = lag(location_x),
    previous_pitch_y = lag(location_y)
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

# Test stan model
data_stan <- scherzer_clean %>% 
  filter(
    !is.na(location_x), !is.na(location_y),
    !is.na(previous_pitch_x), !is.na(previous_pitch_y),
    pitch_type %in% c("CH", "CU", "FC", "FF", "SL"),
    previous_pitch_type %in% c("CH", "CU", "FC", "FF", "SL")
  ) %>% 
  mutate(
    offspeed = ifelse(pitch_type %in% c("FC", "FF"), 0, 1)
  ) %>% 
  head(2000)

# design <- model.matrix(~batter_handedness+count+pitch_count_total+previous_pitch_type, data = data_stan)
# list_stan <- list(
#   N = nrow(design),
#   # ncat = length(unique(data_stan$pitch_type)),
#   K = ncol(design),
#   y = data_stan$offspeed,
#   X = design
# )
# 
# params <- colnames(design)
# 
# simple_model <- rstan::stan(
#   file = "logit_model.stan",
#   data = list_stan
# )
# 
# plot(simple_model, par=paste0("beta[", 1:18, "]"), plotfun="trace", inc_warmup=TRUE)
# plot(simple_model, par=paste0("beta[", 1:18, "]"), show_density = TRUE)
# params
# 
# posterior_samples <- extract(simple_model)

# predictions <- colMeans(posterior_samples$y_rep)

multivariate_design <- model.matrix(~batter_handedness + pitch_type, data = data_stan)
params <- colnames(multivariate_design)
outcomes <- c("location_x", "location_y")

list_multivariate_stan <- list(
  N = nrow(multivariate_design),
  J = ncol(multivariate_design),
  K = length(outcomes),
  x = multivariate_design,
  y = data.frame(data_stan[, outcomes])
  )

multivar_model <- rstan::stan(
  file = "multivariate.stan",
  data = list_multivariate_stan
)

plot(multivar_model, par=paste0("beta[1,", 2:length(params), "]"), plotfun="trace", inc_warmup=TRUE)
plot(multivar_model, par=paste0("beta[2,", 2:length(params), "]"), show_density = TRUE)

data_stan %>% 
  ggplot(aes(x = location_x, y = location_y)) +
  stat_density2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE)+
  scale_fill_gradientn(colors = c("blue", "white", "red"))

posterior_samples <- extract(multivar_model)
x_pred <- t(posterior_samples$ypred[, , 1])
y_pred <- t(posterior_samples$ypred[, , 2])

is_slider = data_stan$pitch_type == "SL"
is_fastball = data_stan$pitch_type == "FF"
density_posterior <- MASS::kde2d(x_pred[is_fastball, ], y_pred[is_fastball, ])
filled.contour(density_posterior, color.palette = heat.colors, xlim = c(50, 150), ylim = c(100, 180))

density_prior <- MASS::kde2d(x = data_stan$location_x, y = data_stan$location_y)
filled.contour(density_prior, color.palette = heat.colors, xlim = c(50, 150), ylim = c(100, 180))

ggplot(aes(x = x_pred[1, ], y = y_pred[1, ])) +
  stat_density2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE)+
  scale_fill_gradientn(colors = c("blue", "white", "red"))
# I have generated quantities (posterior distribution) for the y value vectors
# now compare the "prior" distribution of any given pitch with the posterior distribution
