# ---- Load necessary packages ----
library(readr)
library(dplyr)

# ---- Load data ----
x_data <- read_csv("X.csv")
y_data <- read_csv("y.csv")
time_data <- read_csv("time.csv")

# ---- Prepare variables ----
x1 <- x_data$x1
x2 <- x_data$x2
y <- y_data$y

# ---- Construct polynomial terms ----
x1_sq <- x1^2
x1_cu <- x1^3
x1_4 <- x1^4
x1_5 <- x1^5

# ---- Helper function for manual least squares ----
estimate_theta <- function(X, y) {
  XtX <- t(X) %*% X
  XtY <- t(X) %*% y
  theta_hat <- solve(XtX) %*% XtY
  return(theta_hat)
}

# ---- Construct design matrices for each model ----

# Model 1: y = θ1 * x1^3 + θ2 * x1^5 + θ3 * x2 + bias + ε
X1 <- cbind(1, x1_cu, x1_5, x2)
theta1 <- estimate_theta(X1, y)

# Model 2: y = θ1 * x1 + θ2 * x2 + bias + ε
X2 <- cbind(1, x1, x2)
theta2 <- estimate_theta(X2, y)

# Model 3: y = θ1 * x1 + θ2 * x1^2 + θ3 * x1^4 + θ4 * x2 + bias + ε
X3 <- cbind(1, x1, x1_sq, x1_4, x2)
theta3 <- estimate_theta(X3, y)

# Model 4: y = θ1 * x1 + θ2 * x1^2 + θ3 * x1^3 + θ4 * x1^5 + θ5 * x2 + bias + ε
X4 <- cbind(1, x1, x1_sq, x1_cu, x1_5, x2)
theta4 <- estimate_theta(X4, y)

# Model 5: y = θ1 * x1 + θ2 * x1^3 + θ3 * x1^4 + θ4 * x2 + bias + ε
X5 <- cbind(1, x1, x1_cu, x1_4, x2)
theta5 <- estimate_theta(X5, y)

# ---- Display estimated parameters ----
theta_df <- data.frame(
  Model = paste0("Model ", 1:5),
  Parameters = I(list(theta1, theta2, theta3, theta4, theta5))
)

print(theta_df)
