library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")
names(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
dist(d[1,])

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))

###
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = FALSE)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
train_set_x = as.data.frame(x[-test_index,])
test_set_x = as.data.frame(x[test_index,])

train_set_y <- y[-test_index]
test_set_y <- y[test_index]
ks = seq(1,12,2)
acc <- sapply(ks, function(k){
  knn_fit <- knn3(train_set_x, factor(train_set_y), k = k)
  predictions <- predict(knn_fit, test_set_x, type = "class")
  confusionMatrix(data = predictions, reference = factor(test_set_y))$overall["Accuracy"]
})

acc

## OR

set.seed(1)
library(caret)

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

test_index <- createDataPartition(y, list = FALSE)

sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})