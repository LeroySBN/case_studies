library(tidyverse)
library(caret)
library(dslabs)
library(purrr)
data("heights")

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  set.seed(1)
  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
  test_set <- heights[test_index,]
  train_set <- heights[-test_index,]
  knn_fit <- knn3(sex ~ height, data = train_set, k = k)
  predictions <- predict(knn_fit, test_set, type = "class")
  F_meas(data = predictions, reference = factor(test_set$sex))
})

max(F_1)
ks[which.max(F_1)]
plot(ks, F_1)

### OR

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
