library(nleqslv)

# Given data
data_values <- c(0.080, 0.084, 0.102, 0.124, 0.326, 0.358, 0.412, 0.444, 0.456, 0.504, 0.498, 0.564, 0.648,
                 0.666, 0.682, 0.732, 0.770, 0.814, 0.840, 0.862, 0.882, 0.922, 0.924, 0.964, 1.034, 1.034,
                 1.048, 1.128, 1.134, 1.172, 1.238, 1.240, 1.242, 1.244, 1.294, 1.302, 1.372, 1.522, 1.526)
data_length <- length(data_values)
print(data_length)

sorted_data <- sort(data_values)
print(sorted_data)

# Finding MLE for beta and eta
beta_val <- 1.2
eta_val <- 1.2
alpha_val <- 0.05
fn <- function(C){
  sum1 <- sum2 <- sum3 <- 0
  for(i in 1:data_length){
    sum1 <- sum1 + sorted_data[i] 
    sum2 <- sum2 + exp(beta_val * sorted_data[i] - 1)
    sum3 <- sum3 + sorted_data[i] * exp(beta_val * sorted_data[i] - 1)
  }
  f <- data_length / beta_val + sum1 - (data_length / sum2) * sum3
  return(f)
}
mle_beta_val <- nleqslv(beta_val, fn)$x
print(mle_beta_val)

sum4 <- 0
for(i in 1:data_length){
  sum4 <- sum4 + exp(mle_beta_val * sorted_data[i] - 1)
}
mle_eta_val <- data_length / sum4
print(mle_eta_val)

# Chi-Square test

num_intervals <- 10     # Number of intervals
max_data <- max(sorted_data)
min_data <- min(sorted_data)

data_range <- max_data - min_data
interval_length <- data_range / num_intervals
print(interval_length)
min_interval <- min_data
print(min_interval)
max_interval <- min_data + interval_length
print(max_interval)
O <- c()
E <- c()
for(i in 1:num_intervals){
  frequency <- 0
  for(j in 1:data_length){
    if(sorted_data[j] >= min_interval && sorted_data[j] <= max_interval){
      frequency <- frequency + 1
    }
  }
  O[i] <- frequency
  
  e <- (data_length * (1 - exp(-mle_eta_val * exp(mle_beta_val * max_interval - 1))) - 
          (1 - exp(-mle_eta_val * exp(mle_beta_val * min_interval - 1))))
  E[i] <- e
  
  min_interval <- min_interval + interval_length
  max_interval <- max_interval + interval_length
}
print(O)
print(E)
W <- 0
for(i in 1:num_intervals){
  W <- W + (((O[i] - E[i])^2) / E[i])
}
print(W)

chi_square_val <- qchisq(1 - alpha_val, num_intervals - 3)
print(chi_square_val)

if(W <= chi_square_val){
  print('H0 is accepted')
}else{
  print('H0 is not accepted')
}

# K-S test

F0 <- c()
F1 <- c()
F2 <- c()
D_p <- c()
D_n <- c()
for(i in 1:data_length){
  F0[i] <- (1 - exp(-mle_eta_val * exp(mle_beta_val * sorted_data[i] - 1)))
  F1[i] <- i / data_length
  F2[i] <- (i - 1) / data_length
  D_p[i] <- abs(F0[i] - F1[i])
  D_n[i] <- abs(F0[i] - F2[i])
}
max_D_p <- max(D_p)
max_D_n <- max(D_p)
value_of_D <- max(max_D_p, max_D_n)
print(value_of_D)
if(value_of_D <= 0.2178){
  print('H0 is accepted')
}else{
  print('H0 is not accepted')
}
