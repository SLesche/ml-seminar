# Install and load the required package
library(randomForest)

# Assuming df is your dataset
# Replace 'your_dataset.csv' with the actual file path or data frame name
df <- read.csv('your_dataset.csv')

df <- data.frame(
  stage = round(runif(150, 0, 5), 0),
  gender = round(runif(150, 0, 1), 0),
  excercise = round(runif(150, 0, 1), 0),
  treatment = round(runif(150, 0, 1), 0)
)

df$health = round(df$stage * 40, -2) / 100

# Create training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the Random Forest model
rf_model <- randomForest(stage ~ ., data = df, ntree = 1000, importance = TRUE)
