library(mlbench)
library(tidyverse)
library(glmnet)

# Loading data
data(BreastCancer)

# Different ways of scaling data
bc_scaled <-as.data.frame(apply(BreastCancer[, 2:10], 2, \(x) scale(as.numeric(x))))

bc_scale <- BreastCancer %>% 
  mutate(across(-c(Id, Mitoses), \(x)scale(as.numeric(x))))

bc_no_na <- na.omit(bc_scaled)

# PCA
pca <- prcomp(bc_no_na)
summary(pca)

plot(pca$x[, 1], pca$x[, 2])

plot(pca$rotation[, 1], pca$rotation[, 2], xlim = c(-1, 1), ylim = c(-1, 1))
