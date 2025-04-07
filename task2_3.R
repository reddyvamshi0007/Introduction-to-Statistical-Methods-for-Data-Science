# ---- Function to compute log-likelihood ----
compute_log_likelihood <- function(rss, n) {
  sigma2_hat <- rss / (n - 1)
  log_likelihood <- - (n / 2) * log(2 * pi) -
    (n / 2) * log(sigma2_hat) -
    (1 / (2 * sigma2_hat)) * rss
  return(log_likelihood)
}

# ---- Compute number of samples ----
n <- length(y)

# ---- Compute log-likelihood for each model ----
loglike1 <- compute_log_likelihood(rss1, n)
loglike2 <- compute_log_likelihood(rss2, n)
loglike3 <- compute_log_likelihood(rss3, n)
loglike4 <- compute_log_likelihood(rss4, n)
loglike5 <- compute_log_likelihood(rss5, n)

# ---- Display results ----
loglike_df <- data.frame(
  Model = paste0("Model ", 1:5),
  LogLikelihood = c(loglike1, loglike2, loglike3, loglike4, loglike5)
)

print(loglike_df)
