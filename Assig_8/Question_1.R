observed_values = c(12, 8, 20, 2, 14, 10, 15, 6, 9, 4)
expected_values = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

W_value = 0
for (i in 1:10) {
  W_value = W_value + ((observed_values[i] - expected_values[i]) ^ 2) / 10
}

W_value
if (W_value <= 16.92 && sum(observed_values) == sum(expected_values)) {
  cat("We fail to reject the null hypothesis ", W_value, "\n")
} else {
  cat("We reject the null hypothesis ", W_value, "\n")
}
