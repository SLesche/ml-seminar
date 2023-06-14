library(tree)
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