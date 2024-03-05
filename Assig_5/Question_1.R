library(nleqslv)
u <- runif(1)
theta <- 2.0
lindley_function <- function(x) {
  cdf <- 1 - (theta + 1 + theta * x) / (theta + 1) * exp(-theta * x)
  return(u - cdf)
}
lindley_sample <- nleqslv(2, lindley_function)
print(lindley_sample)
