library(nleqslv)
generate_samples <- function(a, n) {
  samples <- numeric(n)
  for (i in 1:n) {
    y <- runif(1)
    solve_equation <- function(x) {
      f <- (y - a * x + x^2 * (1 - a))
      return(f)
    }
    result <- nleqslv(runif(1), solve_equation)  # Use a random initial guess
    samples[i] <- result$x
  }
  return(samples)
}
generated_samples <- generate_samples(a = 0, n = 2000)
print(generated_samples)
