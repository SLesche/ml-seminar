library(tidyverse)
packs = c('tidyverse', 'skimr', 'caTools', 'mltools', 'psych', 'glmnet', 'rsample', 'data.table', 'rpart','caret', 'rpart.plot', 'randomForest')
if (!require("pacman")) install.packages("pacman")
pacman::p_load(packs, update=F, character.only = T)

# Loading data
train <- read.csv("./data/train_tt.csv")
test <- read.csv("./data/test_tit_x.csv")

eval_metrics = function(pred, true){
  cf = caret::confusionMatrix(factor(pred),factor(true))
}

# Exploration
colnames(test)

full = bind_rows(train, test) %>% 
  mutate(
    age_estimated = ifelse(grepl("\\.5$", as.character(Age)), 1, 0),
    age_missing = ifelse(is.na(Age), 1, 0),
    length_name = nchar(Name),
    n_names = str_count(Name, " "),
    has_nickname = ifelse(grepl("\"[A-Za-z]+\"", Name), 1, 0),
    has_othername = ifelse(grepl(")$", Name), 1, 0),
    is_underage = ifelse(Age < 18, 1, 0),
    is_child = ifelse(Age < 10, 1, 0),
    cabin_letter = str_extract(Cabin, "^[A-Z]")
  ) %>% 
  mutate(
    parent_onboard = ifelse(is_underage & Parch > 0, 1, 0),
    child_onboard = ifelse(!is_underage & Parch > 0, 1, 0),
    has_spouse = ifelse(!is_underage & SibSp > 0, 1, 0),
    is_married = ifelse(Title %in% c("Mrs", "Mr"), 1, 0),
    has_siblings = ifelse(is_underage & SibSp > 0, 1, 0),
    fam_size = SibSp + Parch
  ) %>% 
  mutate(
    imputed_age = Age,
    fare_square = Fare^2,
  ) %>% 
  mutate(
    imputed_age = ifelse(is.na(Age) & Title == "Master", 7, Age),
    ticket_has_letter = ifelse(grepl("[A-Za-z]+", Ticket), 1, 0),
  ) %>% 
  select(-X, -PassengerId, -Name, -Surname, -X.1, -Cabin, -Ticket) %>%
  mutate_at(vars(-c(Age, Fare, fare_square, length_name, imputed_age, Survived, n_names, fam_size)), list(as.factor)) %>% 
  mutate_if(is.factor, addNA)

na_imputed <- data.frame(
  original = full$Age,
  imputed_missForest = missForest::missForest(full[, c("n_names", "Fare", "fam_size", "length_name", "Age")])$ximp$Age
)

full <- full %>% 
  mutate(imputed_age = na_imputed$imputed_missForest) %>% 
  select(-Age)

full_ohe <- one_hot(as.data.table(full)) %>% 
  janitor::remove_constant() %>% 
  janitor::remove_empty() %>% 
  janitor::clean_names() %>% 
  mutate(survived = factor(survived))

data <- full_ohe[1:764, ]
test_data <- full_ohe[765:nrow(full_ohe),]

training_data = data
over_sampled_training = upSample(training_data, training_data$survived)
over_sampled_training$Class = c()

### Run Random Forest on train set, use rpart; hyperparam setting: ntree=200, nodesize = 10
rforest_1 <- randomForest(survived ~., data = over_sampled_training, type = "classification", ntree = 2000, nodesize = 10)

# get predictions and train set performance 
pred_rforest_1_train <- predict(rforest_1, over_sampled_training)
train_perf_rforest_1 <- eval_metrics(pred_rforest_1_train, over_sampled_training$survived)

test_predictions <- predict(rforest_1, test_data)

# calculate feature importance 
imp = hstats::perm_importance(rforest_1, over_sampled_training %>% select(-survived), over_sampled_training$survived, loss = 'squared_error', normalize = T)
plot(imp) +
  ggtitle("PFI") +
  xlab('Reduction in MSE (compared to average loss) when predictor included')

write.csv(test_predictions, file = "./data/predictions_coding_challenge_3.csv")
test_data$survived <- test_predictions
