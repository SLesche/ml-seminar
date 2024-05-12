library(tidyverse)
packs = c('tidyverse', 'skimr', 'caTools', 'mltools', 'psych', 'glmnet', 'rsample', 'data.table', 'rpart','caret', 'rpart.plot', 'randomForest')
if (!require("pacman")) install.packages("pacman")
pacman::p_load(packs, update=F, character.only = T)

# Loading data
