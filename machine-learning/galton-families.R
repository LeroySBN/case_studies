library(HistData)
library(tidyverse)
library(caret)
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Predict son's height using father's height
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# if we're ignoring the father's height and guessing
# the son's height, we would guess the average height of son's
m <- mean(train_set$son)

# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

# Alternatively, use predict function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# others
?predict.glm
?predict.lm
?predict.knn3
