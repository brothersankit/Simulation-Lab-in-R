library(nleqslv)
generate_data_function <- function(theta, n) {
  x <- c()
  for (i in 1:n) {
    y <- runif(1)
    
    solve_subproblem <- function(x) {
      f <- (y - 1 + (((theta + 1 + theta * x) / (theta + 1)) * exp(-theta * x)))
      return(f)
    }
    
    x[i] <- nleqslv(1.2, solve_subproblem)$x
  }
  return(x)
}
n_iterations <- 1000
theta_param <- 2
x <- generate_data_function(theta_param, n_iterations)
print(x)
calculate_theta_function <- function(theta) {
  sum1 <- sum(x)
  f <- ((sum1 / n_iterations) - (theta + 2) / (theta * (theta + 1)))
  return(f)
}
estimated_theta <- nleqslv(1.2, calculate_theta_function)$x
print(estimated_theta)
