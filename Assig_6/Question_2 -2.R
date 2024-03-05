library(nleqslv)
n_iterations <- 1000
generate_gpd_data <- function(alpha, beta, n) {
  gpd_data <- c()
  for (i in 1:n) {
    u <- runif(1)
    gpd_data[i] <- (-1 / beta) * log(1 - (u^(1 / alpha)))
  }
  return(gpd_data)
}
gpd_data <- generate_gpd_data(1.2, 1.2, n_iterations)
print(gpd_data)
estimate_beta <- function(beta) {
  sum1 <- sum(gpd_data)
  sum2 <- sum(log(1 - exp(-beta * gpd_data)))
  sum3 <- sum((gpd_data * exp(-beta * gpd_data)) / (1 - exp(-beta * gpd_data)))
  
  f <- sum1 + (((n_iterations / sum2) + 1) * sum3) - (n_iterations / beta)
  return(f)
}
estimated_beta <- nleqslv(1, estimate_beta)$x
print(estimated_beta)
alpha_value <- -n_iterations/sum(log(1-exp(-estimated_beta*gpd_data)))
print(alpha_value)
                
                