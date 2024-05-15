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
  mutate(pitch_type = factor(pitch_type))

split_data <- rsample::initial_validation_split(ml_ohe, c(0.9, 0.05))

training_data <- rsample::training(split_data) %>% 
  select(
    pitch_type,
    starts_with("freq"),
    starts_with("lag1")
    ) %>% 
  na.omit()

testing_data <- rsample::testing(split_data) %>% 
  select(
    pitch_type,
    starts_with("freq"),
    starts_with("lag1")
  ) %>% 
  na.omit()

x_train = model.matrix(pitch_type~., training_data)[,-1]
x_test = model.matrix(pitch_type~., testing_data)[,-1]
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
best_model = glmnet::glmnet(x_train, y_train, alpha = 1, family = "multinomial")

### Evaluate model 2 ##
# predict raw values for the train set and test set
predictions2 = predict(best_model, s=best_lambda, newx=x_train, type = "class")

# # calculate and print r2 and rmse scores for train and test set
score_train1 = eval_metrics_classification(predictions2, as.numeric(training_data$pitch_type))


training_data <- rsample::training(split_data) %>% 
  select(
    is_fastball,
    starts_with("lag1"),
    starts_with("lag2")
  ) %>% 
  na.omit()

testing_data <- rsample::testing(split_data) %>% 
  select(
    is_fastball,
    starts_with("lag1"),
    starts_with("lag2")
  ) %>% 
  na.omit()

x_train = model.matrix(is_fastball~., training_data)[,-1]
x_test = model.matrix(is_fastball~., testing_data)[,-1]
y_train = training_data %>%
  select(is_fastball) %>%
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
                             family = "binomial")
best_lambda = cv_model$lambda.min

# fit best model
best_model = glmnet::glmnet(x_train, y_train, alpha = 1, family = "binomial")

### Evaluate model 2 ##
# predict raw values for the train set and test set
predictions2 = predict(best_model, s=best_lambda, newx=x_train, type = "class")

# # calculate and print r2 and rmse scores for train and test set
score_train1 = eval_metrics_classification(predictions2, as.numeric(training_data$is_fastball))


# Which ML Models to try out?
# Random Forest
# Decision Tree Model
# Multinomial Regression
# Logistic Regression for 2 Pitch (Fastball/Offspeed) (LASSO/RIDGE)
# k-nearest Neighbor
# Neural Net

### Run Random Forest on train set, use rpart; hyperparam setting: ntree=200, nodesize = 10
library(randomForest)
rforest_1 <- randomForest(pitch_type ~., data = training_data, ntree = 200, nodesize = 1, type = "classification")

# get predictions and train set performance 
pred_rforest_1_train <- predict(rforest_1, training_data, type = "class")
train_perf_rforest_1 <- eval_metrics_classification(
  pred_rforest_1_train,
  training_data$pitch_type
  )

# get predictions and test set performance 
pred_rforest_1_test <- predict(rforest_1, testing_data)
test_perf_rforest_1 <- eval_metrics_classification(
  pred_rforest_1_test, 
  testing_data$pitch_type
  )
c