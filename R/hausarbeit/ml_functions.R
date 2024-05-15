eval_metrics_regression <- function(pred, true){
  SSE = sum((pred - true)^2)
  SST = sum((true - mean(true))^2)
  R_square = 1 - SSE / SST
  RMSE = sqrt(SSE/length(true))
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}

run_ridge_regression <- function(training, validation, testing, outcome_var){
  set.seed(0)
  #find optimal lambda value that minimizes MSE on train set
  nFolds = 10
  foldid = sample(rep(seq(nFolds), length.out = nrow(training_data)))
  cv_model = glmnet::cv.glmnet(x = as.matrix(x = training_data %>% select(-vars(outcome_var))), 
                       y = training_data %>% select(vars(outcome_var)), 
                       alpha = 0, 
                       nfolds = nFolds,
                       foldid = foldid)
  best_lambda = cv_model$lambda.min
  
  # fit best model
  best_model = glmnet(
    training_data[, -outcome_var],
    training_data[, outcome_var],
    alpha = 0, 
    lambda = best_lambda
    )
  
  predictions = predict(best_model, s=best_lambda, newx=training_data[, -outcome_var])
  predictions_test = predict(best_model, s = best_lambda, newx = testing_data[, -outcome_var])
  
  # # calculate and print r2 and rmse scores for train and test set
  score_train = eval_metrics(predictions, training_data[, outcome_var])
  print(c("Score Train: RIDGE Regression", score_train1))
  
  score_test = eval_metrics(predictions_test, testing_data[, outcome_var])
  print(c("Score Test: RIDGE Regression", score_test1))
  
}