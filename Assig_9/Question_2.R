library(nleqslv)

# Given data
data_values <- c(.70, .84, .58, .50, .55, .82, .59, .71, .72, .61, .62, .49, .54, .36, .36, .71, .35,
                 .64, .85, .55, .59, .29, .75, .46, .46, .60, .60, .36, .52, .68, .80, .55, .84, .34,
                 .34, .70, .49, .56, .71, .61, .57, .73, .75, .44, .44, .81, .80, .87, .29, .50)
data_length <- length(data_values)
print(data_length)

sorted_data <- sort(data_values)
print(sorted_data)

# Constants
C_val <- 1.2
K_val <- 1.2
alpha_val <- 0.05

# Function to calculate the objective
objective_function <- function(C) {
  sum1 <- sum2 <- sum3 <- 0
  for (i in 1:data_length) {
    sum1 <- sum1 + log(1 - exp(-(C * sorted_data[i])^2))
    sum2 <- sum2 + C * (sorted_data[i])^2
    sum3 <- sum3 + (2 * C^2 * sorted_data[i] * exp(-(C * sorted_data[i])^2)) / (1 - exp(-(C * sorted_data[i])^2))
  }
  f <- (2 * data_length / C - ((data_length / sum1) + 1) * sum3 - 2 * sum2)
  return(f)
}

# Finding the optimal value for C
mle_C_val <- nleqslv(C_val, objective_function)$x
print(mle_C_val)

# Calculating K
sum4 <- 0
for (i in 1:data_length) {
  sum4 <- sum4 + log(1 - exp(-(mle_C_val * sorted_data[i])^2))
}
mle_K_val <- -data_length / sum4
print(mle_K_val)

# Chi-Square Test
num_intervals <- 6   
max_data <- max(sorted_data)
min_data <- min(sorted_data)
interval_length <- (max_data - min_data) / num_intervals
print(interval_length)

min_interval <- min_data
print(min_interval)

max_interval <- min_data + interval_length
print(max_interval)

observed <- c()
expected <- c()
for (i in 1:num_intervals) {
  frequency <- 0
  for (j in 1:data_length) {
    if (sorted_data[j] >= min_interval && sorted_data[j] <= max_interval) {
      frequency <- frequency + 1
    }
  }
  observed[i] <- frequency
  
  expected[i] <- (data_length * ((1 - exp(-(mle_C_val * max_interval)^2)^mle_K_val) - 
                                   (1 - exp(-(mle_C_val * min_interval)^2)^mle_K_val)))
  
  min_interval <- min_interval + interval_length
  max_interval <- max_interval + interval_length
}
print(observed)
print(expected)

chi_square_value <- 0
for (i in 1:num_intervals) {
  chi_square_value <- chi_square_value + (((observed[i] - expected[i])^2) / expected[i])
}
print(chi_square_value)

critical_value <- qchisq(1 - alpha_val, num_intervals - 3)
print(critical_value)

if (chi_square_value <= critical_value) {
  print('H0 is accepted')
} else {
  print('H0 is not accepted')
}

# Kolmogorov-Smirnov Test
F0 <- c()
F1 <- c()
F2 <- c()
D_p <- c()
D_n <- c()
for (i in 1:data_length) {
  F0[i] <- (1 - exp(-(mle_C_val * sorted_data[i])^2)^mle_K_val)
  F1[i] <- i / data_length
  F2[i] <- (i - 1) / data_length
  D_p[i] <- abs(F0[i] - F1[i])
  D_n[i] <- abs(F0[i] - F2[i])
}
max_D_p <- max(D_p)
max_D_n <- max(D_p)
value_of_D <- max(max_D_p, max_D_n)
print(value_of_D)

if (value_of_D <= 0.19234) {
  print('H0 is accepted')
} else {
  print('H0 is not accepted')
}
