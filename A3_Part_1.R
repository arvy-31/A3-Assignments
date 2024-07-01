
# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages('caret')
install.packages('rpart.plot')

library(readr)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(dplyr)
library(tidyr)



# Import the dataset
data <- read_csv("D:\\SCMA632__FIRE632\\Stats\\Assignment\\A3\\Car Evaluation.csv",  progress = TRUE)

# Display the first few rows of the dataset
head(data)

# Check for missing values
sum(is.na(data))

# Remove rows with missing values
data <- na.omit(data)

# Convert character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# One-hot encode categorical variables
features <- data %>%
  select(buying_price = `buying price`, maintenance_cost = `maintenance cost`, 
         number_of_doors = `number of doors`, number_of_persons = `number of persons`,
         luggage_boot = `luggage boot`, safety) %>%
  mutate(across(where(is.factor), as.factor)) %>%
  model.matrix(~ . - 1, data = .) %>%
  as.data.frame()

# Encode the target variable
data$decision_encoded <- ifelse(data$decision == "unacceptable", 0, 1)

# Define the target variable
target <- data$decision_encoded

# Split the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(target, p = 0.7, list = FALSE)
trainData <- features[trainIndex, ]
testData <- features[-trainIndex, ]
trainTarget <- target[trainIndex]
testTarget <- target[-trainIndex]

install.packages('glmnet')

library(glmnet)

# Convert data to matrix format for glmnet
trainDataMatrix <- as.matrix(trainData)
testDataMatrix <- as.matrix(testData)


# Fit the logistic regression model with Lasso regularization
logreg_model <- cv.glmnet(trainDataMatrix, trainTarget, family = "binomial", alpha = 1)

# Predict on the test set
pred_prob <- predict(logreg_model, newx = testDataMatrix, s = "lambda.min", type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(testTarget, pred_class)

# ROC curve
roc_obj <- roc(testTarget, as.vector(pred_prob))
auc_value <- auc(roc_obj)

# Plot the ROC curve
plot(roc_obj, col = "darkorange", lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve")
abline(a = 0, b = 1, col = "navy", lwd = 2, lty = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "darkorange", lwd = 2)

# Print confusion matrix and AUC value
print(conf_matrix)
print(auc_value)

# Fit the decision tree model
tree_model <- rpart(trainTarget ~ ., data = trainData, method = "class")

# Predict on the test set
tree_pred_class <- predict(tree_model, newdata = testData, type = "class")

# Confusion matrix for the decision tree
tree_conf_matrix <- table(testTarget, tree_pred_class)

# ROC curve for the decision tree
tree_pred_prob <- predict(tree_model, newdata = testData, type = "prob")[,2]
tree_roc_obj <- roc(testTarget, tree_pred_prob)
tree_auc_value <- auc(tree_roc_obj)

# Plot the ROC curve for the decision tree
plot(tree_roc_obj, col = "darkgreen", lwd = 2, main = "ROC Curve for Decision Tree")
abline(a = 0, b = 1, col = "navy", lwd = 2, lty = 2)
legend("bottomright", legend = paste("AUC =", round(tree_auc_value, 2)), col = "darkgreen", lwd = 2)

# Print confusion matrix and AUC value for the decision tree
print(tree_conf_matrix)
print(tree_auc_value)