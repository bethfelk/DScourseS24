library(nloptr)
library(modelsummary)

# Question 4: Define values
# Set seed
set.seed(100)

# Create the X matrix
# Set the dimensions of the matrix
N <- 100000  # Number of rows
K <- 10      # Number of columns

# Fill it in
X <- matrix(0, nrow = N, ncol = K)  # Initialize the matrix with zeros
X[, 1] <- 1  # Set the first column to 1's
X[, -1] <- matrix(rnorm((N * (K - 1)), mean = 0, sd = 1), nrow = N)  

# Create epsilon
N <- 100000  # Length of the vector

# Set the variance
sigma_squared <- 0.25  # Variance (σ^2)

# Generate the vector epsilon (eps)
eps <- rnorm(N, mean = 0, sd = sqrt(sigma_squared))

# Create beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Create Y
Y <- X %*% beta + eps

# Question 5: Compute OLS estimate of beta 

# Define values
X_transpose <- t(X)  # Transpose of matrix X
X_transp_X <- X_transpose %*% X  # X'X matrix
X_transp_Y <- X_transpose %*% Y  # X'Y vector

# Estimate the OLS equation
beta_hat_OLS <- solve(X_transp_X) %*% X_transp_Y  

# Print the estimate
print(beta_hat_OLS)

# How does my estimate compare to the true values?
# My estimate is extrememly close (within 0.01) for each of the 10 beta values



# Question 6: gradient descent method for OLS
alpha <- 0.0000003
iter <- 500
beta_gradient <- function(X, Y, beta) return(as.vector(-2*t(X)%*%(Y-X%*%beta)))

beta_matrix <- matrix(0, ncol = 1, nrow = ncol(X))

# create a vector to contain all xs for all steps
x.All <- vector("numeric", iter)

for(i in 1:iter) {
  beta_matrix <- beta_matrix - step * beta_gradient(X, Y, beta_matrix)
  x.All[i] <- beta.grad
}

print(paste("The estimate of beta is ", beta_matrix, sep = ""))


# Question 7: L-BFGS method for OLS
## Our objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}

## Gradient of our objective function
lbfgs_gradient <- function(beta,Y,X) {
  return (as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
lbfgs_results <- nloptr(x0=beta0,eval_f=objfun,eval_grad_f=lbfgs_gradient,opts=options,Y=Y,X=X)
print(lbfgs_results)

# Question 7: Nelder-Mead method for OLS
# objective function is the same as L-BFGS objective function
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}

## initial values
beta1 <- runif(dim(X)[2]) 

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
nm_results <- nloptr(x0=beta1,eval_f=objfun,opts=options,Y=Y,X=X)
print(nm_results)

# compare the results
print(nm_results)
print(lbfgs_result)

# The results of the L-BFGS and Nelder-Mead estimations are very similar, with
# both estimates being very close to the true beta values. The L-BFGS seems to
# be very slightly more accurate for most of the betas however.

# Question 8: L-BFGS method for MLE
# Objective function
objfun_mle <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  log_likelihood <- sum(-0.5 * (log(2 * pi * (sig^2)) + ((Y - X %*% beta) / sig)^2))
  return(-log_likelihood) # Negative log-likelihood for minimization
}

# Define the gradient function
gradient_mle <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Set initial parameter values
theta0 <- c(runif(ncol(X)), 1)  # Initialize beta with random values and sigma with 1

# Define optimization options
options <- list("algorithm" = "NLOPT_LN_SBPLX", "xtol_rel" = 1.0e-6)

# Run optimization
results_mle <- nloptr(x0 = theta0, eval_f = objfun_mle, eval_grad_f = gradient_mle, opts = options, Y = Y, X = X)

# Extract the estimated parameters
betahat_mle <- result$solution[1:(length(result$solution) - 1)]
sigmahat_mle <- result$solution[length(result$solution)]

# Print the result
print(results_mle)
print(betahat_mle)
print(sigmahat_mle)


# Question 9: OLS the easy way
ols_easy <- lm(Y ~ X -1)
modelsummary(ols_easy, stars = TRUE, output= "latex")