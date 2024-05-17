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

is_binary_column <- function(col){
  all(col %in% c(0, 1))
}

run_ridge_regression <- function(training_data, testing_data, validation_data){
  training_data = rbind(training_data, validation_data)
  
  cols_to_scale = training_data %>% 
    select_if(is.numeric) %>% 
    select_if(~!is_binary_column(.))
  
  train_scale = training_data %>% 
    select(-ends_with("hit_location_other"), -ends_with("Other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_constant() %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  test_scale = testing_data %>% 
    select(-ends_with("hit_location_other"), -ends_with("Other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_constant() %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  x_train = model.matrix(outcome~., train_scale)[,-1]
  x_test = model.matrix(outcome~., test_scale)[,-1]
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
  
  result = list()
  result$model = best_model
  result$train_acc = score_train$overall[1]
  result$test_acc = score_test$overall[1]
  return(result)
}

run_lasso_regression <- function(training_data, testing_data, validation_data){
  training_data = rbind(training_data, validation_data)
  
  cols_to_scale = training_data %>% 
    select_if(is.numeric) %>% 
    select_if(~!is_binary_column(.))
  
  train_scale = training_data %>% 
    select(-ends_with("hit_location_other"), -ends_with("Other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  test_scale = testing_data %>% 
    select(-ends_with("hit_location_other"), -ends_with("Other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  x_train = model.matrix(outcome~., train_scale)[,-1]
  x_test = model.matrix(outcome~., test_scale)[,-1]
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
  
  result = list()
  result$model = best_model
  result$train_acc = score_train$overall[1]
  result$test_acc = score_test$overall[1]
  return(result)
  
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
  
  best_nodesize = try_nodesize[validation_accuracy == max(validation_accuracy)][1]
  
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
  
  result = list()
  result$model = rforest
  result$best_param = best_nodesize
  result$train_acc = train_perf_rforest$overall[1]
  result$test_acc = test_perf_rforest$overall[1]
  return(result)
  
}

get_predictor_importance_rf <- function(model, training_data){
  imp = hstats::perm_importance(model, training_data %>% select(-outcome), training_data$outcome, loss = 'squared_error', normalize = T)
  
  # prepare model for viz & calc shap
  unified_model = treeshap::randomForest.unify(model, data.matrix(select(training_data, -outcome)))
  treeshap = treeshap::treeshap(unified_model, data.matrix(select(training_data, -outcome))) # may take some while
  result = list()
  
  result$importance = imp
  result$shap = treeshap
  
  return(result)
}

run_knn <- function(training_data, testing_data, validation_data){
  # KNN ----
  cols_to_scale = training_data %>% 
    select_if(is.numeric) %>% 
    select_if(~!is_binary_column(.))
  
  train_scale = training_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  val_scale = validation_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  
  test_scale = testing_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  try_k = seq(5, 200, 10)
  knn_accuracy = numeric(length(try_k))
  # Fitting KNN Model to training dataset 
  for (i in seq_along(try_k)){
    classifier_knn = class::knn(train = train_scale, 
                                 test = val_scale, 
                                 cl = training_data$outcome, 
                                 k = try_k[i]) 
    knn_accuracy[i] = eval_metrics_classification(classifier_knn, validation_data$outcome)$overall[1]
  }
  
  best_k = try_k[knn_accuracy == max(knn_accuracy)][1]
  
  classifier_knn = class::knn(train = train_scale, 
                              test = test_scale, 
                              cl = training_data$outcome, 
                              k = best_k) 
  
  score_test = eval_metrics_classification(classifier_knn, testing_data$outcome)
  
  result = list()
  result$model = classifier_knn
  result$best_param = best_k
  result$test_acc = score_test$overall[1]
  return(result)
}

run_svm <- function(training_data, testing_data, validation_data){
  cols_to_scale = training_data %>% 
    select_if(is.numeric) %>% 
    select_if(~!is_binary_column(.))
  
  train_scale = training_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  val_scale = validation_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  
  test_scale = testing_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  try_degree = seq(1, 4, 1)
  svm_accuracy = numeric(length(try_degree))
  
  for (i in seq_along(try_degree)){
    # Support Vector Machines ----
    svm_model <- svm(outcome ~ .,data = cbind(train_scale, outcome = training_data$outcome), 
                     kernel = "polynomial",
                     degree = try_degree[i])
    
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
    
    svm_accuracy[i] = val_perf_svm$overall[1]
  }
  
  best_degree = try_degree[svm_accuracy == max(svm_accuracy)][1]
  
  # Support Vector Machines ----
  svm_model <- svm(outcome ~ .,data = cbind(train_scale, outcome = training_data$outcome), 
                   kernel = "polynomial",
                   degree = best_degree)
  
  pred_svm_train <- predict(svm_model, train_scale, type = "class")
  train_perf_svm <- eval_metrics_classification(
    pred_svm_train,
    training_data$outcome
  )
  
  # get predictions and validation set performance 
  pred_svm_test <- predict(svm_model, test_scale, type = "class")
  val_perf_svm <- eval_metrics_classification(
    pred_svm_test, 
    testing_data$outcome
  )
  
  result = list()
  result$model = svm_model
  result$best_param = best_degree
  result$train_acc = score_train$overall[1]
  result$test_acc = score_test$overall[1]
  return(result)
}
  

run_svm_linear <- function(training_data, testing_data, validation_data){
  cols_to_scale = training_data %>% 
    select_if(is.numeric) %>% 
    select_if(~!is_binary_column(.))
  
  train_scale = training_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  val_scale = validation_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  
  test_scale = testing_data %>% 
    select(-outcome, -ends_with("hit_location_other")) %>% 
    mutate(across(all_of(names(cols_to_scale)), scale)) %>% 
    janitor::remove_empty() %>% 
    as.data.frame()
  
  try_cost = 1 / c(2, 5, 10, 100, 1000, 10000)
  svm_accuracy = numeric(length(try_cost))
  
  for (i in seq_along(try_cost)){
    # Support Vector Machines ----
    svm_model <- svm(outcome ~ .,data = cbind(train_scale, outcome = training_data$outcome), 
                     kernel = "linear",
                     cost = try_cost[i])
    
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
    
    svm_accuracy[i] = val_perf_svm$overall[1]
  }
  
  best_cost = try_cost[svm_accuracy == max(svm_accuracy)][1]
  
  # Support Vector Machines ----
  svm_model <- svm(outcome ~ .,data = cbind(train_scale, outcome = training_data$outcome), 
                   kernel = "linear",
                   cost = best_cost)
  
  pred_svm_train <- predict(svm_model, train_scale, type = "class")
  train_perf_svm <- eval_metrics_classification(
    pred_svm_train,
    training_data$outcome
  )
  
  # get predictions and validation set performance 
  pred_svm_test <- predict(svm_model, test_scale, type = "class")
  val_perf_svm <- eval_metrics_classification(
    pred_svm_test, 
    testing_data$outcome
  )
  
  result = list()
  result$model = svm_model
  result$best_param = best_degree
  result$train_acc = score_train$overall[1]
  result$test_acc = score_test$overall[1]
  return(result)
}
