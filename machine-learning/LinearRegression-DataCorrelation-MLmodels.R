library(tidyverse)
library(MASS)
library(caret)

set.seed(1, sample.kind="Rounding")

# generate data from a bivariate normal distrubution
n <- 100
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n=100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
#plot(dat)

#ps <- seq(0, 1, 0.1)
#dat %>% 
  #mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  #group_by(g) %>%
  #summarize(y = mean(y), x = mean(x)) %>%
  #qplot(x, y, data =.)

# Evaluate the RMSE
set.seed(1, sample.kind="Rounding")
B <- 100
dat_replicate <- replicate(B,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set <- dat[test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(dat_replicate)
sd(dat_replicate)

###
###
###

# Compare with larger datasets

n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

# On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases.

###
###
###

# Make Correlation Larger
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)
# When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y

###
###
###

set.seed(1, sample.kind="Rounding")

Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

set.seed(1, sample.kind="Rounding")

rmse1 <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
min(rmse1)
mean(rmse1)
sd(rmse1)

##

rmse2 <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
min(rmse2)
mean(rmse2)
sd(rmse2)

##

set.seed(1, sample.kind="Rounding")

Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")

rmse3 <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set <- dat[test_index, ]
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse3)
sd(rmse3)
min(rmse3)
