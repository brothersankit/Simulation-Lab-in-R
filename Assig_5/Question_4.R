n <- 1000
samp_01 <- numeric(0)
while (length(samp_01) < n) {
  u1 <- runif(1)
  u2 <- runif(1)
  v1 <- 2 * u1 - 1
  v2 <- 2 * u2 - 1
  w <- v1^2 + v2^2
  if (w <= 1) {
    y <- (-2 * log(w) / w)^(1/2)
    x1 <- v1 * y
    x2 <- v2 * y
    samp_01 <- c(samp_01, x1, x2)
  }
}
samp_01 <- head(samp_01, n)
print(samp_01)
