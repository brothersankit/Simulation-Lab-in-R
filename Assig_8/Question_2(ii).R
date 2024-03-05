library(nleqslv)
generated_sample <- c()
num_samples <- 1000
theta <- 1.2
for(i in 1:num_samples){
  y<-runif(1)
  create_function_for_x<-function(generated_sample){
    f <- ((y+((theta+1+theta*generated_sample)/(theta+1))*exp(-theta*generated_sample))-1)
    return(f)
  }
  generated_sample[i] <- nleqslv(1.2,create_function_for_x)$x
}
print(generated_sample)

max_value <- max(generated_sample)
min_value <- min(generated_sample)
max_value
min_value

observed_freq <- c()
expected_freq <- c()
num_intervals <- 6
increment_size <- (max_value - min_value) / num_intervals
chi_square_statistic <- 0


for (interval in 1:num_intervals) {
  lower_bound <- min_value
  min_value <- lower_bound + increment_size
  frequency_count <- 0
  for (i in 1:num_samples) {
    if (generated_sample[i] >= lower_bound && generated_sample[i] <= min_value) {
      frequency_count <- frequency_count + 1
    }
  }
  observed_freq[interval] <- frequency_count
  
  
  expected_frequency = num_samples * ( (1 - ((theta + 1 + theta * min_value) / (theta + 1)) * exp(-theta * min_value)) -(1 - ((theta + 1 + theta * lower_bound) / (theta + 1)) * exp(-theta * lower_bound ) ) )
  expected_freq[interval] <- expected_frequency
}

print("Observed frequency is")
print(observed_freq)

print("Expected frequency is")
print(expected_freq)


for (interval in 1:num_intervals) {
  chi_square_statistic <- chi_square_statistic + (observed_freq[interval] - expected_freq[interval])^2 / expected_freq[interval]
}

print(chi_square_statistic)

if (chi_square_statistic <= 11.07) {
  print("We failed to reject the hypothesis, and the value of the chi-square statistic is ")
  print(chi_square_statistic)
} else {
  print("We reject the hypothesis, and the value of the chi-square statistic is ")
  print(chi_square_statistic)
}

