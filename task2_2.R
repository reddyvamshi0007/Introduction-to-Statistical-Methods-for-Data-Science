# ---- Function to compute RSS ----
compute_rss <- function(X, theta, y) {
  y_hat <- X %*% theta          # Predicted y
  residuals <- y - y_hat        # Errors
  rss <- sum(residuals^2)       # RSS
  return(rss)
}

# ---- Compute RSS for all models ----
rss1 <- compute_rss(X1, theta1, y)
rss2 <- compute_rss(X2, theta2, y)
rss3 <- compute_rss(X3, theta3, y)
rss4 <- compute_rss(X4, theta4, y)
rss5 <- compute_rss(X5, theta5, y)

# ---- Display RSS results ----
rss_df <- data.frame(
  Model = paste0("Model ", 1:5),
  RSS = c(rss1, rss2, rss3, rss4, rss5)
)

print(rss_df)
