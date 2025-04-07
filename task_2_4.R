# ---- Number of parameters in each model ----
k1 <- 4
k2 <- 3
k3 <- 5
k4 <- 6
k5 <- 5

# ---- AIC and BIC computation ----
compute_aic <- function(k, loglike) {
  return(2 * k - 2 * loglike)
}

compute_bic <- function(k, loglike, n) {
  return(k * log(n) - 2 * loglike)
}

# ---- Compute AIC and BIC for each model ----
aic1 <- compute_aic(k1, loglike1)
bic1 <- compute_bic(k1, loglike1, n)

aic2 <- compute_aic(k2, loglike2)
bic2 <- compute_bic(k2, loglike2, n)

aic3 <- compute_aic(k3, loglike3)
bic3 <- compute_bic(k3, loglike3, n)

aic4 <- compute_aic(k4, loglike4)
bic4 <- compute_bic(k4, loglike4, n)

aic5 <- compute_aic(k5, loglike5)
bic5 <- compute_bic(k5, loglike5, n)

# ---- Combine results ----
model_eval_df <- data.frame(
  Model = paste0("Model ", 1:5),
  AIC = c(aic1, aic2, aic3, aic4, aic5),
  BIC = c(bic1, bic2, bic3, bic4, bic5)
)

print(model_eval_df)
