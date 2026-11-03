generate_dataset <- function(n = 300, m = 1000, chosen_function = rbeta, ...) {
  result <- list()
  for (i in 1:n) {
    result[[i]] <- chosen_function(m, ...)
  }
  return(result)
}

random_samples <- generate_dataset(300, 1000, rbeta, 3, 4)
str(random_samples)

calculate_mean_values <- function(dataset) {
  result <- c()
  for (i in 1:length(dataset)) {
    result <- c(result, mean(dataset[[i]]))
  }
  return(result)
}

mean_values <- calculate_mean_values(random_samples)

qqnorm(random_samples[[1]], main = "Original distribution", pch = 1, frame = FALSE)
qqnorm(mean_values, main = "Mean distributions", pch = 1, frame = FALSE)