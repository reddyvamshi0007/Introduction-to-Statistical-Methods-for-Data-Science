# === Load Required Libraries ===
library(ggplot2)
library(gridExtra)

# === Step 1: Load Data ===
X_data <- read.csv("X.csv")
y_data <- read.csv("y.csv")

# Extract variables
x1 <- X_data$x1
x2 <- X_data$x2
y <- y_data$y
n <- length(y)

# === Step 2: Create Input Matrices for 5 Models ===

# Model 1: y = θ1·x1³ + θ2·x1⁵ + θ3·x2 + θ_bias + ε
X_model1 <- cbind(x1^3, x1^5, x2, rep(1, n))

# Model 2: y = θ1·x1 + θ2·x2 + θ_bias + ε
X_model2 <- cbind(x1, x2, rep(1, n))

# Model 3: y = θ1·x1 + θ2·x1² + θ3·x1⁴ + θ4·x2 + θ_bias + ε
X_model3 <- cbind(x1, x1^2, x1^4, x2, rep(1, n))

# Model 4: y = θ1·x1 + θ2·x1² + θ3·x1³ + θ4·x1⁵ + θ5·x2 + θ_bias + ε
X_model4 <- cbind(x1, x1^2, x1^3, x1^5, x2, rep(1, n))

# Model 5: y = θ1·x1 + θ2·x1³ + θ3·x1⁴ + θ4·x2 + θ_bias + ε
X_model5 <- cbind(x1, x1^3, x1^4, x2, rep(1, n))

# === Step 3: Estimate Model Parameters Using Least Squares ===
theta1 <- solve(t(X_model1) %*% X_model1) %*% t(X_model1) %*% y
theta2 <- solve(t(X_model2) %*% X_model2) %*% t(X_model2) %*% y
theta3 <- solve(t(X_model3) %*% X_model3) %*% t(X_model3) %*% y
theta4 <- solve(t(X_model4) %*% X_model4) %*% t(X_model4) %*% y
theta5 <- solve(t(X_model5) %*% X_model5) %*% t(X_model5) %*% y

# === Step 4: Compute Predictions and Residuals ===
y_pred1 <- X_model1 %*% theta1
res1 <- as.vector(y - y_pred1)

y_pred2 <- X_model2 %*% theta2
res2 <- as.vector(y - y_pred2)

y_pred3 <- X_model3 %*% theta3
res3 <- as.vector(y - y_pred3)

y_pred4 <- X_model4 %*% theta4
res4 <- as.vector(y - y_pred4)

y_pred5 <- X_model5 %*% theta5
res5 <- as.vector(y - y_pred5)

# === Step 5: Define Plotting Function ===
plot_residuals <- function(residuals, model_name) {
  df <- data.frame(residuals = residuals)
  p1 <- ggplot(df, aes(x = residuals)) +
    geom_histogram(aes(y=..density..), bins=50, fill="skyblue", color="black") +
    geom_density(color="darkblue", size=1) +
    ggtitle(paste(model_name, "- Residual Histogram")) +
    theme_minimal()
  
  p2 <- ggplot(df, aes(sample = residuals)) +
    stat_qq(color = "darkred") +
    stat_qq_line(color = "black") +
    ggtitle(paste(model_name, "- Q-Q Plot")) +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol=2)
}

# === Step 6: Plot Residual Distributions and Q-Q Plots for All Models ===
plot_residuals(res1, "Model 1")
plot_residuals(res2, "Model 2")
plot_residuals(res3, "Model 3")
plot_residuals(res4, "Model 4")
plot_residuals(res5, "Model 5")
