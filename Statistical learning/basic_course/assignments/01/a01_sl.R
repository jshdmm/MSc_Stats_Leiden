library(ggplot2)
library(papaja)
library(tidyverse)

set.seed(42)

# Input sample size n
n_train <- 50

# Function to generate data
generate_data <- function(n_train) {
  set.seed(42)
  x <- runif(n_train, -3, 3)
  epsilon <- rnorm(n_train, 0, 1)
  y <- 8 * sin(x) + epsilon
  data <- data.frame(x = x, y = y)
  return(data)
}
}


lm_simple <- lm(y ~ x)
lm_middle <- lm(y ~ sin(x))
lm_complex <- lm(y ~ poly(x, 18))
y_simple <- predict(lm_simple)
y_middle <- predict(lm_middle)
y_complex <- predict(lm_complex)


##### GPT ######


# Function to generate data
generate_data <- function(n) {
  set.seed(42)
  x <- runif(n, -3, 3)
  epsilon <- rnorm(n, 0, 1)
  y <- 8 * sin(x) + epsilon
  data <- data.frame(x = x, y = y)
  return(data)
}

# function to train models, set test size
train_and_evaluate <- function(data, degree, test_size = 10000) {
  set.seed(42)
  
  # Generate test data
  test_data <- generate_data(test_size)
  
  # Train polynomial regression model
  poly_features <- poly(data$x, degree, raw = TRUE)
  model <- lm(y ~ poly_features - 1, data = data)
  
  # Make predictions on test set
  test_predictions <- predict(model, newdata = data.frame(poly_features = poly(test_data$x, degree, raw = TRUE) - 1))
  
  # Calculate MSE on the test set
  mse <- mean((test_data$y - test_predictions)^2)
  
  return(mse)
}

# Generate training set of size 50
train_data_50 <- generate_data(50)

# Train and evaluate polynomial regression models with degree 3 and 15
mse_degree_3_50 <- train_and_evaluate(train_data_50, degree = 3)
mse_degree_15_50 <- train_and_evaluate(train_data_50, degree = 15)

# Generate training set of size 10000
train_data_10000 <- generate_data(10000)

# Train and evaluate polynomial regression models with degree 3 and 15
mse_degree_3_10000 <- train_and_evaluate(train_data_10000, degree = 3)
mse_degree_15_10000 <- train_and_evaluate(train_data_10000, degree = 15)

# Print results
cat("Test MSE (Degree 3, Training Set Size 50):", mse_degree_3_50, "\n")
cat("Test MSE (Degree 15, Training Set Size 50):", mse_degree_15_50, "\n")
cat("Test MSE (Degree 3, Training Set Size 10000):", mse_degree_3_10000, "\n")
cat("Test MSE (Degree 15, Training Set Size 10000):", mse_degree_15_10000, "\n")







### Code from Julian

set.seed(42)

# Function to generate data
generate_data <- function(n) {
  x <- runif(n, -3, 3)
  epsilon <- rnorm(n, 0, 1)
  y <- 8 * sin(x) + epsilon
  train <- data.frame(x = x, y = y)
  return(train)
}

# set train and test sizes
train_size_train = 50 
test_size_train = 10000


# Generate test data
train_data = generate_data(train_size_train)
test_data <- generate_data(test_size_train)


# train two poly regression models (3, and 15)

#3 degrees
fit_3 <- lm(y ~ poly(x, degree = 3), data = train_data)
train_pred <- predict(fit_3, newdata = train_data)
test_pred <- predict(fit_3, newdata = test_data)
test_err <- mean((test$y - test_pred)^2)
train_err <- mean((train$y - train_pred)^2) #115.86

#15 degrees
fit_15 <- lm(y ~ poly(x, degree = 15), data = train_data)
train_pred <- predict(fit_15, newdata = train_data)
test_pred <- predict(fit_15, newdata = test_data)
test_err <- mean((test$y - test_pred)^2)
train_err <- mean((train$y - train_pred)^2) #234.925

# get MSE on test set



# repeat with training set of size 10000





## generate test data:
xtest <- runif(n, min = -5, max = 5)
ytest <- xtest + 8*sin(xtest/2) + rnorm(n)
test <- data.frame(x = xtest, y = ytest)
degrees <- 1:15
train_err <- test_err <- rep(NA, length(degrees))
train_pred <- matrix(NA, nrow = length(degrees), ncol = n)

for (d in 1:15) {
  fit <- lm(y ~ poly(x, degree = d), data = train)
  train_pred[d,] <- predict(fit, newdata = train)
  test_pred <- predict(fit, newdata = test)
  test_err[d] <- mean((test$y - test_pred)^2)
  train_err[d] <- mean((train$y - train_pred[d,])^2)
}


par(mfrow = c(1, 2))
plot(1:15, train_err, xlab = "Degree of polynomial",
     ylab = "train MSE", ylim = c(0, max(c(train_err, test_err))))
lines(c(1, 15), c(1, 1), col = "red")
plot(1:15, test_err, xlab = "Degree of polynomial",
     ylab = "test MSE", ylim = c(0, max(c(train_err, test_err))))
lines(c(1, 15), c(1, 1), col = "red")



plot(x, y, main = "Train data, true and fitted curves", cex = .5, cex.main = .8)
curve(x + 8*sin(x/2), add = TRUE, lwd = 2)
lines(sort(x), train_pred[1, order(x)], col = "grey", lwd = 2)
lines(sort(x), train_pred[2, order(x)], col = "blue", lwd = 2, lty = 3)
lines(sort(x), train_pred[3, order(x)], col = "red", lwd = 2)
lines(sort(x), train_pred[15, order(x)], col = "green", lwd = 2)
legend("topleft", legend = c("true function", paste("degree =", c(1, 2, 3, 15))),
       lty = c(1, 1, 3, 1, 1), col = c("black", "grey", "blue", "red", "green"),
       cex = .5)


