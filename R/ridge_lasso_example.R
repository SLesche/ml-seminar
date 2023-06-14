library(mlbench)
library(tidyverse)
library(glmnet)

# Loading data
data(BreastCancer)

# Different ways of scaling data
bc_scaled <-as.data.frame(apply(BreastCancer[, 2:10], 2, \(x) scale(as.numeric(x))))

bc_scale <- BreastCancer %>% 
  mutate(across(-c(Id, Mitoses), \(x)scale(as.numeric(x))))

# Model with all predictors
mult_model <- lm(Cl.thickness ~ ., data = bc_scaled)
summary(mult_model)

# Omit nas
bc_no_na <- na.omit(bc_scaled)

# criterium
y <- as.matrix(bc_no_na[, 1])
x <- as.matrix(bc_no_na[, -1])

ridge <- glmnet(x, y, family = "gaussian", alpha = 0)
plot(ridge)

lasso <- glmnet(x, y, family = "gaussian", alpha = 1)
plot(lasso)
lasso$beta
