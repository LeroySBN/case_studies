library(caret)
library(tidyverse)
data(iris)
names(iris)

############
############

str(iris)

iris %>% group_by(Species) %>% summarize(count=n())
write.csv(iris, file="iris.csv")

iris_set <- iris %>% filter(Species =="setosa")
write.csv(iris_set, file="iris_set.csv")

iris_ver <- iris %>% filter(Species =="versicolor")
write.csv(iris_ver, file="iris_ver.csv")

iris_vir <- iris %>% filter(Species =="virginica")
write.csv(iris_vir, file="iris_vir.csv")

############
############


iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
levels(iris$Species)


set.seed(2, sample.kind="Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

summary(train)
iris %>% select(Petal.Length) %>% summarize(mean(Petal.Length), sd(Petal.Length))

# which feature has the highest accuracy

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

# use smart cutoff value from the training data to calculate overall accuracy in the test data

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

plot(iris,pch=21,bg=iris$Species)

# We had an overall accuracy greater than 96% in the training data, but the overall accuracy was lower in the test data. (overtrained)
# Petal.Length and Petal.Width in combination could potentially be more information than either feature alone

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

