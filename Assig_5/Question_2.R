#(i)
generate_cauchy_samples <- function(alpha, n) {
  samples <- numeric(n)
  for (i in 1:n) {
    u <- runif(1)
    samples[i] <- (1 / alpha) * tan(pi * u - (pi / 2))
  }
  return(samples)
}
cauchy_samples <- generate_cauchy_samples(alpha = 1, n = 1000)
print(cauchy_samples)





#(ii)
n <- 1000
alpha <- 2.0  
beta <- 0.5
generate_generalized_exponential_samples <- function(alpha, beta, n) {
  samples <- numeric(n)
  for (i in 1:n) {
    u <- runif(1)
    x <- (-1 / beta) * log(1 - u^(1 / alpha))
    samples[i] <- x
  }
  return(samples)
}
gen_exp_samples <- generate_generalized_exponential_samples(alpha, beta, n)
print(gen_exp_samples)




#(iii)
n <- 1000
alpha <- 0.5  
beta <- 1
generate_kumaraswamy_samples <- function(alpha, beta, n) {
  samples <- numeric(n)
  for (i in 1:n) {
    u1 <- runif(1)
    u2 <- runif(1)
    x <- (1 - (1 - u1)^(1 / beta))^(1 / alpha)
    samples[i] <- x
  }
  return(samples)
}
kumaraswamy_samples <- generate_kumaraswamy_samples(alpha, beta, n)
print(kumaraswamy_samples)

