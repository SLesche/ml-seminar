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
  print(cf)
}
