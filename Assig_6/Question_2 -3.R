library(nleqslv)
n_iterations <- 1000
alpha_param <- 1.2
beta_param <- 1.2
generate_data_function <- function(alpha, beta, n) {
  data <- c()
  for (i in 1:n) {
    y <- runif(1)
    data[i] <- ((1 - (1 - y)^(1 / beta))^1 / alpha)
  }
  return(data)
}
data <- generate_data_function(alpha_param, beta_param, n_iterations)
print(data)
sum1 <- sum(log(1 - (data)^alpha_param))
sum2 <- sum(((data) * log(data)) / (1 - (data)^alpha_param))
sum3 <- sum(log(data))
estimate_alpha_function <- function(alpha) {
  f <- n_iterations / alpha - (((-n_iterations / sum1) - 1) * sum2 - sum3)
  return(f)
}
estimated_alpha <- nleqslv(1.2, estimate_alpha_function)$x
print(estimated_alpha)
estimated_beta <- -(n_iterations / sum(log(1 - (data)^estimated_alpha)))
print(estimated_beta)
