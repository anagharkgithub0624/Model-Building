# Load necessary libraries
library(dplyr)
library(caret)

# Assuming your dataset is named 'tech_product_data.csv'
# Load the dataset
data <- read.csv("smart.csv")

# View the first few rows of the dataset
head(data)


# Handle missing values (if any)
# For simplicity, we'll remove rows with missing values
data <- na.omit(data)

# Descriptive statistics
summary(data)

# Multiple Linear Regression Model
# Define the formula
formula <- price ~ rating + num_cores + processor_speed + battery_capacity +
  fast_charging_available  + ram_capacity + internal_memory +
  screen_size + refresh_rate +

# Split data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$price, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

# Print model summary
summary(model)

print(paste("RMSE: ", rmse))
##########################################################################################

# Load necessary libraries
library(dplyr)
library(caret)

# Load the dataset
data <- read.csv("smart.csv")

# View the first few rows of the dataset
head(data)

# Handle missing values (if any)
# For simplicity, we'll remove rows with missing values
data <- na.omit(data)

# Descriptive statistics
summary(data)

# Multiple Linear Regression Model
# Define the formula
formula <- price ~ rating + num_cores + processor_speed + battery_capacity +
  fast_charging_available + ram_capacity + internal_memory +
  screen_size + refresh_rate

# Split data into training and testing sets (70% training, 30% testing)
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$price, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]

# Train the model
model <- lm(formula, data = dataTrain)

# Print model summary
summary(model)


# Calculate model accuracy (R-squared and RMSE)
r_squared <- caret::R2(predictions, dataTest$price)
rmse <- caret::RMSE(predictions, dataTest$price)

# Print accuracy
print(paste("R-squared: ", r_squared))
print(paste("RMSE: ", rmse))

#####
####################################################################################

# Load necessary libraries
library(dplyr)
library(caret)
library(glmnet)
# Split data into training and testing sets (70% training, 30% testing)
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$price, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]
# Prepare the data for glmnet
x_train <- model.matrix(price ~ rating + num_cores + processor_speed + battery_capacity +
                          fast_charging_available + ram_capacity + internal_memory +
                          screen_size + refresh_rate, dataTrain)[, -1]
y_train <- dataTrain$price

x_test <- model.matrix(price ~ rating + num_cores + processor_speed + battery_capacity +
                         fast_charging_available + ram_capacity + internal_memory +
                         screen_size + refresh_rate, dataTest)[, -1]
y_test <- dataTest$price
# Lasso Regression (alpha = 1)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)

# Predict on the test set
lasso_predictions <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test)

# Calculate model accuracy (R-squared and RMSE)
r_squared_lasso <- caret::R2(lasso_predictions, y_test)
rmse_lasso <- caret::RMSE(lasso_predictions, y_test)

# Print accuracy
print(paste("Lasso R-squared: ", r_squared_lasso))
print(paste("Lasso RMSE: ", rmse_lasso))


### rigid###############
# Ridge Regression (alpha = 0)
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)

# Predict on the test set
ridge_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test)

# Calculate model accuracy (R-squared and RMSE)
r_squared_ridge <- caret::R2(ridge_predictions, y_test)
rmse_ridge <- caret::RMSE(ridge_predictions, y_test)

# Print accuracy
print(paste("Ridge R-squared: ", r_squared_ridge))
print(paste("Ridge RMSE: ", rmse_ridge))

