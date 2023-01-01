library(dslabs)
library(tidyverse)
library(broom)
library(caret)
data("mnist_27")
set.seed(1995)

# Create 10 bootstrap samples for the mnist_27
indexes <- createResample(mnist_27$train$y, 10)

# Generate a random dataset
# expected value and standard error of the 75th quantile
set.seed(1)
y <- rnorm(100, 0, 1)

B <- 10000
dat <- replicate(B, { y <- rnorm(100, 0, 1)
                 quantile(y, 0.75)
                 })
mean(dat)
sd(dat)
# In practice, we can't run a Monte Carlo simulation

# Generate a random dataset
# Use 10 bootstrap samples to estimate the expected value and standard error of the 75th quantile
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

