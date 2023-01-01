# Use the training set to build a model with several of the models available from the caret package.
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Create a matrix of predictions for the test set
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

acc <- sapply(fits, function(object){
  y_hat <- predict(object, newdata = mnist_27$test)
  confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]
})
mean(acc)

#OR

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

# Which individual methods perform better than the ensemble?
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]