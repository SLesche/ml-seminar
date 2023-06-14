library(mlbench)
library(tidyverse)
library(MASS)
# Loading data
data(BreastCancer)

# Different ways of scaling data
bc_scaled <-as.data.frame(apply(BreastCancer[, 2:10], 2, \(x) scale(as.numeric(x))))

bc_scale <- BreastCancer %>% 
  mutate(across(-c(Id, Mitoses), \(x)scale(as.numeric(x))))

bc_no_na <- na.omit(bc_scaled)

Y <- BreastCancer$Class[as.numeric(rownames(bc_no_na))]

bc_data <- data.frame(bc_no_na, Y)

# folds
cvfolds <- caret::createFolds(c(1:nrow(bc_no_na)), k = 10)

result <- numeric()

for (i in 1:length(cvfolds)){
  trainindex = unlist(cvfolds[-i])
  testindex = unlist(cvfolds[i])
  
  lda_model_cv = lda(Y ~ ., data = bc_data[trainindex, ])
  prediction = predict(lda_model_cv, bc_data[testindex, ])$class
  
  sensitivity = sum(bc_data$Y[testindex] == "malignant" & prediction == "malignant")/sum(bc_data$Y[testindex] == "malignant")
  specificity = sum(bc_data$Y[testindex] == "benign" & prediction == "benign")/sum(bc_data$Y[testindex] == "benign")
  
  result = c(result, (sensitivity + specificity) / 2)
}

# bootstrapping
result <- numeric()

for (i in 1:100){
  trainindex = sample(1:nrow(bc_data), replace = TRUE)
  testindex = setdiff(1:nrow(bc_data), trainindex)
  
  lda_model_cv = lda(Y ~ ., data = bc_data[trainindex, ])
  prediction = predict(lda_model_cv, bc_data[testindex, ])$class
  
  sensitivity = sum(bc_data$Y[testindex] == "malignant" & prediction == "malignant")/sum(bc_data$Y[testindex] == "malignant")
  specificity = sum(bc_data$Y[testindex] == "benign" & prediction == "benign")/sum(bc_data$Y[testindex] == "benign")
  
  result = c(result, (sensitivity + specificity) / 2)
}

plot(density(result), xlim = c(0, 1))
