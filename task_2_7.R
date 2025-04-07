set.seed(123)  # For reproducibility

# === Step 1: Split the data (70% train, 30% test) ===
n <- length(y)
train_indices <- sample(1:n, size = floor(0.7 * n))
test_indices <- setdiff(1:n, train_indices)

# Training data
x1_train <- x1[train_indices]
x2_train <- x2[train_indices]
y_train <- y[train_indices]

# Testing data
x1_test <- x1[test_indices]
x2_test <- x2[test_indices]
y_test <- y[test_indices]

# === Step 2: Rebuild best model (Model 3) on training data ===
X_train <- cbind(x1_train, x1_train^2, x1_train^4, x2_train, rep(1, length(y_train)))
theta_best <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train

# === Step 3: Predict on test data ===
X_test <- cbind(x1_test, x1_test^2, x1_test^4, x2_test, rep(1, length(y_test)))
y_pred_test <- X_test %*% theta_best

# === Step 4: Compute residual variance on training set ===
y_train_pred <- X_train %*% theta_best
residuals_train <- y_train - y_train_pred
sigma2_hat <- sum(residuals_train^2) / (length(y_train) - ncol(X_train))

# === Step 5: Compute 95% confidence intervals ===
# Var(y_pred) = σ² * X(X'X)^-1 X'
XTX_inv <- solve(t(X_train) %*% X_train)
standard_errors <- sqrt(diag(X_test %*% XTX_inv %*% t(X_test)) * sigma2_hat)
margin_error <- 1.96 * standard_errors  # 95% confidence

lower_bound <- y_pred_test - margin_error
upper_bound <- y_pred_test + margin_error

# === Step 6: Plot Predictions with 95% Confidence Interval ===
df_test <- data.frame(
  index = 1:length(y_test),
  y_true = y_test,
  y_pred = y_pred_test,
  lower = lower_bound,
  upper = upper_bound
)

library(ggplot2)

ggplot(df_test, aes(x = index)) +
  geom_point(aes(y = y_true), color = "blue", size = 2) +
  geom_line(aes(y = y_pred), color = "red", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray70", alpha = 0.4) +
  labs(
    title = "Model 3 Predictions with 95% Confidence Interval",
    x = "Test Sample Index",
    y = "MEG Output"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
