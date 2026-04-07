library(nloptr)
library(modelsummary)
library(tidyverse)

# ==============================================================================
# simulate data
# ==============================================================================

# set seed
set.seed(100)

# dimensions
N <- 100000
K <- 10

# regressors: first column is 1s, rest are normal random variables
X <- cbind(1, matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1))

# error term
epsilon <- rnorm(N, mean = 0, sd = 0.5)

# true beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# outcome
Y <- X %*% beta + epsilon

# check
dim(X)
length(epsilon)
length(beta)
dim(Y)

# ==============================================================================
# OLS via matrix algebra
# ==============================================================================

# OLS via closed-form matrix formula
beta_hat_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat_ols

# compare OLS estimates to true beta values
beta_hat_ols - beta

# ==============================================================================
# OLS using built-in lm()
# ==============================================================================

# note: lm() includes an intercept by default, so use X[, -1]
# to exclude the first column of 1s from X
fit_lm <- lm(Y ~ X[, -1])

# view regression output
summary(fit_lm)

# ==============================================================================
# OLS via gradient descent: setup
# ==============================================================================

# learning rate (given in assignment)
alpha <- 0.0000003

# number of iterations
n_iter <- 10000

# initial guess for beta
beta_hat_gd <- rep(0, K)

# ==============================================================================
# objective function and gradient for OLS
# ==============================================================================

# OLS objective function:
# sum of squared residuals
ols_obj <- function(beta, Y, X) {
  resid <- Y - X %*% beta
  sum(resid^2)
}

# gradient of the OLS objective function
ols_grad <- function(beta, Y, X) {
  -2 * t(X) %*% (Y - X %*% beta)
}

# ==============================================================================
# gradient descent loop
# ==============================================================================

# store objective function values
obj_values <- numeric(n_iter)

# gradient descent iterations
for (i in 1:n_iter) {
  
  # compute gradient at current beta
  grad <- ols_grad(beta_hat_gd, Y, X)
  
  # update beta
  beta_hat_gd <- beta_hat_gd - alpha * grad
  
  # store objective function value
  obj_values[i] <- ols_obj(beta_hat_gd, Y, X)
}

# check gradient descent estimates
beta_hat_gd

# compare gradient descent estimates to matrix OLS estimates
beta_hat_gd - beta_hat_ols

# compare gradient descent estimates to true beta values
beta_hat_gd - beta

# look at the first few and last few objective values
head(obj_values)
tail(obj_values)

# plot objective function values over iterations
plot(obj_values, type = "l",
     xlab = "Iteration",
     ylab = "Objective Function Value",
     main = "Gradient Descent Convergence")

# plot log objective values so convergence is easier to see
plot(log(obj_values), type = "l",
     xlab = "Iteration",
     ylab = "Log Objective Function Value",
     main = "Gradient Descent Convergence (Log Scale)")

# zoom in on the first 200 iterations
plot(log(obj_values[1:200]), type = "l",
     xlab = "Iteration",
     ylab = "Log Objective Function Value",
     main = "Gradient Descent Convergence: First 200 Iterations")

# ==============================================================================
# OLS via nloptr
# ==============================================================================

# starting values for beta
beta_start <- rep(0, K)

# OLS objective function for nloptr
eval_f <- function(beta, Y, X) {
  resid <- Y - X %*% beta
  sum(resid^2)
}

# gradient function for nloptr
eval_grad_f <- function(beta, Y, X) {
  -2 * t(X) %*% (Y - X %*% beta)
}

# ==============================================================================
# OLS via nloptr: L-BFGS
# ==============================================================================

# minimize the OLS objective using L-BFGS
opt_lbfgs <- nloptr(
  x0 = beta_start,
  eval_f = eval_f,
  eval_grad_f = eval_grad_f,
  lb = rep(-Inf, K),
  ub = rep(Inf, K),
  opts = list(
    algorithm = "NLOPT_LD_LBFGS",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  Y = Y,
  X = X
)

# L-BFGS estimates
opt_lbfgs$solution

# compare L-BFGS estimates to matrix OLS estimates
opt_lbfgs$solution - as.vector(beta_hat_ols)

# ==============================================================================
# OLS via nloptr: Nelder-Mead
# ==============================================================================

# minimize the OLS objective using Nelder-Mead
opt_nm <- nloptr(
  x0 = beta_start,
  eval_f = eval_f,
  lb = rep(-Inf, K),
  ub = rep(Inf, K),
  opts = list(
    algorithm = "NLOPT_LN_NELDERMEAD",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  Y = Y,
  X = X
)

# Nelder-Mead estimates
opt_nm$solution

# compare Nelder-Mead estimates to matrix OLS estimates
opt_nm$solution - as.vector(beta_hat_ols)

# compare all OLS estimates in one object
cbind(
  true_beta = beta,
  ols_matrix = as.vector(beta_hat_ols),
  grad_descent = as.vector(beta_hat_gd),
  lbfgs = opt_lbfgs$solution,
  nelder_mead = opt_nm$solution
)

# ==============================================================================
# MLE for normal linear regression
# ==============================================================================

# negative log-likelihood function
# theta = (beta, sigma)
eval_f_mle <- function(theta, Y, X) {
  
  # split parameter vector into beta and sigma
  beta <- theta[1:(length(theta) - 1)]
  sigma <- theta[length(theta)]
  
  # residuals
  resid <- Y - X %*% beta
  
  # negative log-likelihood
  n <- nrow(X)
  n * log(sigma) + as.numeric(crossprod(resid)) / (2 * sigma^2)
}

# gradient of the negative log-likelihood
eval_grad_f_mle <- function(theta, Y, X) {
  
  # split parameter vector into beta and sigma
  beta <- theta[1:(length(theta) - 1)]
  sigma <- theta[length(theta)]
  
  # residuals
  resid <- Y - X %*% beta
  
  # sample size
  n <- nrow(X)
  
  # gradient with respect to beta
  grad_beta <- -(1 / sigma^2) * t(X) %*% resid
  
  # gradient with respect to sigma
  grad_sigma <- n / sigma - as.numeric(crossprod(resid)) / sigma^3
  
  # return as one vector
  c(as.vector(grad_beta), grad_sigma)
}

# ==============================================================================
# MLE for normal linear regression: starting values
# ==============================================================================

# starting values: beta starts at 0, sigma starts at 1
theta_start <- c(rep(0, K), 1)


# ==============================================================================
# MLE via nloptr: L-BFGS
# ==============================================================================

# minimize the negative log-likelihood using L-BFGS
opt_mle <- nloptr(
  x0 = theta_start,
  eval_f = eval_f_mle,
  eval_grad_f = eval_grad_f_mle,
  lb = c(rep(-Inf, K), 1e-8),
  ub = c(rep(Inf, K), Inf),
  opts = list(
    algorithm = "NLOPT_LD_LBFGS",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  Y = Y,
  X = X
)

# MLE estimates
opt_mle$solution

# compare MLE beta estimates to matrix OLS estimates
opt_mle$solution[1:K] - as.vector(beta_hat_ols)

# compare estimated sigma to true sigma
opt_mle$solution[K + 1] - 0.5

# separate MLE estimates into beta and sigma
beta_hat_mle <- opt_mle$solution[1:K]
sigma_hat_mle <- opt_mle$solution[K + 1]

# inspect MLE estimates
beta_hat_mle
sigma_hat_mle



# compare all beta estimates in one object
cbind(
  true_beta = beta,
  ols_matrix = as.vector(beta_hat_ols),
  grad_descent = as.vector(beta_hat_gd),
  lbfgs = opt_lbfgs$solution,
  nelder_mead = opt_nm$solution,
  mle = beta_hat_mle
)

# compare estimated sigma to true sigma
c(true_sigma = 0.5, mle_sigma = sigma_hat_mle)

# ==============================================================================
# OLS the easy way for submission
# ==============================================================================

# OLS using lm() exactly as requested in the assignment
fit_lm_ps8 <- lm(Y ~ X - 1)

# view regression output
summary(fit_lm_ps8)

# ==============================================================================
# export regression output to LaTeX
# ==============================================================================

# export lm() results to a table
modelsummary(
  fit_lm_ps8,
  output = "dataframe"
)


