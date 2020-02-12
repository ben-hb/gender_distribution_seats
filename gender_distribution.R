library(tidyverse)
library(reprex)
library(ggthemes)

# 0 women
# 1 men

actual <- c(0, 2, 3, 0, 4, 3, 2, 2)

students <- rep(0, 4*length(actual) - sum(actual)) %>% 
  append(rep(1, sum(actual)))

results <- c()
trial <- c()

for (i in 1:500) {
  for (f in 1:length(actual)) {
  temp <- sample(students, 4, replace = FALSE)
  trial[f] <- sum(temp, na.rm = TRUE)
  }
  results[i] <- var(trial)
  trial <- c()
}

results_df <- as.data.frame(results)

actual_df <- as.data.frame(actual)

ggplot(results_df, aes(x = results)) + 
  geom_density(bindwidth = 1) + 
  geom_vline(xintercept = var(actual), linetype = "dotted") + 
  geom_text(aes("Variance = 2")) + 
  theme_economist()

