library(mlbench)
library(MASS)

# Loading data
data(BreastCancer)

# Different ways of scaling data
bc_scaled <-as.data.frame(apply(BreastCancer[, 2:10], 2, \(x) scale(as.numeric(x))))

bc_scale <- BreastCancer %>% 
  mutate(across(-c(Id, Mitoses), \(x)scale(as.numeric(x))))

bc_no_na <- na.omit(bc_scaled)

# Outcome
Y <- BreastCancer$Class[as.numeric(rownames(bc_no_na))]


lda_model = lda(bc_no_na, Y)
lda_model

plot(lda_model)

# Predict
pred <- predict(lda_model, bc_no_na)
pred
