eval_metrics_regression <- function(pred, true){
  SSE = sum((pred - true)^2)
  SST = sum((true - mean(true))^2)
  R_square = 1 - SSE / SST
  RMSE = sqrt(SSE/length(true))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}

eval_metrics_classification <- function(pred, true){
  cf = caret::confusionMatrix(factor(pred),factor(true))
  #print
  #print(cf)
}


run_ridge_regression <- function(training_data, testing_data, validation_data){
  training_data = rbind(training_data, validation_data)
  
  x_train = model.matrix(outcome~., training_data)[,-1]
  x_test = model.matrix(outcome~., testing_data)[,-1]
  y_train = training_data %>%
    select(outcome) %>%
    unlist() %>%
    as.numeric()
  
  # Ridge Regression  ----
  set.seed(0)
  #find optimal lambda value that minimizes MSE on train set
  nFolds = 10
  foldid = sample(rep(seq(nFolds), length.out = nrow(training_data)))
  cv_model = glmnet::cv.glmnet(x = as.matrix(x = x_train), 
                               y = y_train, alpha = 0, 
                               nfolds = nFolds,
                               foldid = foldid,
                               family = "binomial")
  best_lambda = cv_model$lambda.min
  
  # fit best model
  best_model = glmnet::glmnet(x_train, y_train, alpha = 0, family = "binomial")
  
  ### Evaluate model 2 
  # predict raw values for the train set and test set
  predictions_train = predict(best_model, s=best_lambda, newx=x_train, type = "class")
  
  # # calculate and print r2 and rmse scores for train and test set
  score_train = eval_metrics_classification(predictions_train, as.numeric(training_data$outcome))
  
  # Predict test
  predictions_test = predict(best_model, s=best_lambda, newx=x_test, type = "class")
  
  # # calculate and print r2 and rmse scores for train and test set
  score_test = eval_metrics_classification(predictions_test, as.numeric(testing_data$outcome))
  
  return(c(score_train$overall[1], score_test$overall[1]))
}

run_lasso_regression <- function(training_data, testing_data, validation_data){
  training_data = rbind(training_data, validation_data)
  x_train = model.matrix(outcome~., training_data)[,-1]
  x_test = model.matrix(outcome~., testing_data)[,-1]
  y_train = training_data %>%
    select(outcome) %>%
    unlist() %>%
    as.numeric()
  
  # Lasso Regression ----
  set.seed(0)
  #find optimal lambda value that minimizes MSE on train set
  nFolds = 10
  foldid = sample(rep(seq(nFolds), length.out = nrow(training_data)))
  cv_model = glmnet::cv.glmnet(x = as.matrix(x = x_train), 
                               y = y_train, alpha = 1, 
                               nfolds = nFolds,
                               foldid = foldid,
                               family = "binomial")
  best_lambda = cv_model$lambda.min
  
  # fit best model
  best_model = glmnet::glmnet(x_train, y_train, alpha = 1, family = "binomial")
  
  ### Evaluate model 2 
  # predict raw values for the train set and test set
  predictions_train = predict(best_model, s=best_lambda, newx=x_train, type = "class")
  
  # # calculate and print r2 and rmse scores for train and test set
  score_train = eval_metrics_classification(predictions_train, as.numeric(training_data$outcome))
  
  # Predict test
  predictions_test = predict(best_model, s=best_lambda, newx=x_test, type = "class")
  
  # # calculate and print r2 and rmse scores for train and test set
  score_test = eval_metrics_classification(predictions_test, as.numeric(testing_data$outcome))
  
  return(c(score_train$overall[1], score_test$overall[1]))
  
}

run_random_forest <- function(training_data, testing_data, validation_data){
  try_nodesize = seq(1, 20, 3)
  
  validation_accuracy = numeric(length(try_nodesize))
  
  for (i in seq_along(try_nodesize)){
    # Random Forest ----
    rforest = randomForest(outcome ~.,
                              data = training_data,
                              ntree = 200,
                              nodesize = try_nodesize[i]
    )
    
    # get predictions and validation set performance 
    pred_rforest_val = predict(rforest, validation_data)
    val_perf_rforest = eval_metrics_classification(
      pred_rforest_val, 
      validation_data$outcome
    )
    
    validation_accuracy[i] = val_perf_rforest$overall[i]
  }
  
  best_nodesize = try_nodesize[validation_accuracy == max(validation_accuracy)]
  
  # Random Forest ----
  rforest = randomForest(outcome ~.,
                         data = training_data,
                         ntree = 200,
                         importance = TRUE,
                         nodesize = best_nodesize
  )
  
  # get predictions and train set performance 
  pred_rforest_train <- predict(rforest, training_data, type = "class")
  train_perf_rforest <- eval_metrics_classification(
    pred_rforest_train,
    training_data$outcome
  )
  
  
  # get predictions and test set performance 
  pred_rforest_test <- predict(rforest, testing_data)
  test_perf_rforest <- eval_metrics_classification(
    pred_rforest_test, 
    testing_data$outcome
  )
  
  return(c(train_perf_rforest$overall[1], test_perf_rforest$overall[1]))
  
}

run_knn <- function(training_data, testing_data, validation_data){
  # KNN ----
  train_scale = training_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(where(is.numeric), scale)) %>% 
    janitor::remove_constant() %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  val_scale = validation_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(where(is.numeric), scale)) %>% 
    janitor::remove_constant() %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  
  test_scale = testing_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(where(is.numeric), scale)) %>% 
    janitor::remove_constant() %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  try_k = seq(5, 100, 10)
  knn_accuracy = numeric(length(try_k))
  # Fitting KNN Model to training dataset 
  for (i in seq_along(try_k)){
    classifier_knn = class::knn(train = train_scale, 
                                 test = val_scale, 
                                 cl = training_data$outcome, 
                                 k = try_k[i]) 
    knn_accuracy[i] = eval_metrics_classification(classifier_knn, validation_data$outcome)$overall[1]
  }
  
  best_k = try_k[knn_accuracy == max(knn_accuracy)]
  
  classifier_knn = class::knn(train = train_scale, 
                              test = test_scale, 
                              cl = training_data$outcome, 
                              k = best_k) 
  return(c(eval_metrics_classification(classifier_knn, validation_data$outcome)$overall[1]))
}

run_svm <- function(training_data, testing_data, validation_data){
  try_cost = seq(0.01, 1, 0.1)
  # Support Vector Machines ----
  svm_model <- svm(outcome ~ .,data = cbind(train_scale, outcome = training_data$outcome), cost = 0.1)
  
  pred_svm_train <- predict(svm_model, train_scale, type = "class")
  train_perf_svm <- eval_metrics_classification(
    pred_svm_train,
    training_data$outcome
  )
  
  # get predictions and validation set performance 
  pred_svm_val <- predict(svm_model, val_scale, type = "class")
  val_perf_svm <- eval_metrics_classification(
    pred_svm_val, 
    validation_data$outcome
  )
}
  