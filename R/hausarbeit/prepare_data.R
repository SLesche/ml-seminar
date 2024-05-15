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

training_data <- rsample::training(split_data) %>% 
  select(-starts_with("lag2"), -starts_with("lag3"),-starts_with("lag4"),-starts_with("lag5")) %>% 
  na.omit()

x_train = model.matrix(pitch_type~., training_data)[,-1]
x_test = model.matrix(pitch_type~., test_ohe)[,-1]
y_train = training_data %>%
  select(pitch_type) %>%
  unlist() %>%
  as.numeric()

set.seed(0)
#find optimal lambda value that minimizes MSE on train set
nFolds = 10
foldid = sample(rep(seq(nFolds), length.out = nrow(training_data)))
library(glmnet)
cv_model = glmnet::cv.glmnet(x = as.matrix(x = x_train), 
                     y = y_train, alpha = 1, 
                     nfolds = nFolds,
                     foldid = foldid,
                     family = "multinomial")
best_lambda = cv_model$lambda.min

# fit best model
best_model = glmnet::glmnet(x_train, y_train, alpha = 1, lambda = best_lambda, family = "multinomial")

### Evaluate model 2 ##
# predict raw values for the train set and test set
predictions2 = predict(best_model, s=best_lambda, newx=x_train, type = "response")
predictions_test2 = predict(best_model, s = best_lambda, newx = x_test)

# # calculate and print r2 and rmse scores for train and test set
score_train1 = eval_metrics(predictions2, training_data$pitch_type)
print(c("Score Train Model 2: LASSO Regression", score_train1))

score_test1 = eval_metrics(predictions_test2, test_ohe$G3)
print(c("Score Test Model 2: LASSO Regression", score_test1))
# fit best model
best_model = glmnet(
  training_data %>% select(-!!sym(outcome_var)),
  training_data %>% select(!!sym(outcome_var)),
  alpha = 0, 
  lambda = best_lambda,
  family = "multinom"
)

predictions = predict(best_model, s=best_lambda, newx=training_data[, -outcome_var])
predictions_test = predict(best_model, s = best_lambda, newx = testing_data[, -outcome_var])

# # calculate and print r2 and rmse scores for train and test set
score_train = eval_metrics(predictions, training_data[, outcome_var])
print(c("Score Train: RIDGE Regression", score_train1))

score_test = eval_metrics(predictions_test, testing_data[, outcome_var])
print(c("Score Test: RIDGE Regression", score_test1))

nrow(na.omit(training_data))

validation_data <- rsample::validation(split_data)
testing_data <- rsample::testing(split_data)


# Which ML Models to try out?
# Random Forest
# Decision Tree Model
# Multinomial Regression
# Logistic Regression for 2 Pitch (Fastball/Offspeed) (LASSO/RIDGE)
# k-nearest Neighbor
# Neural Net
