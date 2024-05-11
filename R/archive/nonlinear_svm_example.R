library(e1071)
library(mlbench)
library(tidyverse)
# Loading data
data(BreastCancer)

# Different ways of scaling data
bc_scaled <-as.data.frame(apply(BreastCancer[, 2:10], 2, \(x) scale(as.numeric(x))))

bc_scale <- BreastCancer %>% 
  mutate(across(-c(Id, Mitoses), \(x)scale(as.numeric(x))))

bc_no_na <- na.omit(bc_scaled)

Y <- BreastCancer$Class[as.numeric(rownames(bc_no_na))]

bc_data <- data.frame(bc_no_na, Y)

svm_model <- svm(Y ~ ., data = bc_no_na[, c(1, 2)], kernel = "radial", cost = 1, gamma = 10)
svm_model
plot(svm_model, data.frame(bc_no_na[, c(1, 2)], Y), symbolPalette = rainbow(4), color.palette = terrain.colors)

