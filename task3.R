# Load necessary packages
library(ggplot2)
library(gridExtra)

# Selected model: Model 3
X_model3 <- cbind(x1, x1^2, x1^4, x2, 1)
theta3 <- as.vector(solve(t(X_model3) %*% X_model3) %*% t(X_model3) %*% y)

# Original prediction and RSS
y_pred_model3 <- X_model3 %*% theta3
rss_original <- sum((y - y_pred_model3)^2)

# Get indices of top 2 parameters by absolute value
top_params <- order(abs(theta3), decreasing = TRUE)[1:2]

# Set prior range (±200% of estimated value)
range_factor <- 2.0
n_samples <- 20000  # Increase number of samples

set.seed(42)  # For reproducibility

prior_theta1 <- runif(n_samples,
                      min = theta3[top_params[1]] * (1 - range_factor),
                      max = theta3[top_params[1]] * (1 + range_factor))

prior_theta2 <- runif(n_samples,
                      min = theta3[top_params[2]] * (1 - range_factor),
                      max = theta3[top_params[2]] * (1 + range_factor))

# Rejection ABC
epsilon <- 2 * rss_original  # More lenient

accepted_theta1 <- c()
accepted_theta2 <- c()

for (i in 1:n_samples) {
  theta_temp <- theta3
  theta_temp[top_params[1]] <- prior_theta1[i]
  theta_temp[top_params[2]] <- prior_theta2[i]
  
  y_sim <- X_model3 %*% theta_temp
  rss_sim <- sum((y - y_sim)^2)
  
  if (rss_sim <= epsilon) {
    accepted_theta1 <- c(accepted_theta1, prior_theta1[i])
    accepted_theta2 <- c(accepted_theta2, prior_theta2[i])
  }
}

# Show results
cat("Number of accepted samples:", length(accepted_theta1), "\n")

# Check for sufficient accepted samples
if (length(accepted_theta1) < 30) {
  stop("Too few accepted samples. Try increasing epsilon or prior range again.")
}

# Posterior data frame
posterior_df <- data.frame(theta1 = accepted_theta1, theta2 = accepted_theta2)

# Marginal posterior plots
p1 <- ggplot(posterior_df, aes(x = theta1)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  ggtitle(paste("Posterior of θ", top_params[1])) +
  theme_minimal()

p2 <- ggplot(posterior_df, aes(x = theta2)) +
  geom_histogram(bins = 30, fill = "tomato", color = "white") +
  ggtitle(paste("Posterior of θ", top_params[2])) +
  theme_minimal()

# Joint posterior
p_joint <- ggplot(posterior_df, aes(x = theta1, y = theta2)) +
  geom_point(alpha = 0.4, color = "purple") +
  ggtitle("Joint Posterior of Top 2 Parameters") +
  xlab(paste("θ", top_params[1])) +
  ylab(paste("θ", top_params[2])) +
  theme_minimal()

# Show plots
grid.arrange(p1, p2, p_joint, nrow = 2)
