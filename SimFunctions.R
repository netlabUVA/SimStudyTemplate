
generate_data <- function(n, betas, residual_error) {
  p <- length(betas)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- X %*% betas + rnorm(n, sd = residual_error)
  data <- data.frame(y, X)
  names(data) <- c("y", paste0("X", 1:p))
  return(data)
}

fit_model <- function(data) {
  formula <- as.formula(paste("y ~", paste(names(data)[-c(1, length(data))], collapse = " + ")))
  model <- lm(formula, data = data)
  return(model)
}


calculate_relative_bias <- function(model, true_betas, residual_error) {
  estimated_betas <- coef(model)[-1]  # Exclude intercept
  print(length(true_betas))
  relative_bias <- (estimated_betas - true_betas[-length(true_betas)]) / true_betas[-length(true_betas)]
  return(relative_bias)
}




