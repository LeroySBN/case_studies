# Generate a set of random predictors and outcomes
#set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
library(tidyverse)
library(caret)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

# Since we have so many predictors, we selected a random sample x_subset.
# Use the subset when training the model
x_subset <- x[ ,sample(p, 100)]

# Because x and y are completely independent, you should not be able to predict y using x with accuracy greater than 0.5
# Confirm this by running cross-validation using logistic regression to fit the model.
# Because we have so many predictors, we selected a random sample x_subset. Use the subset when training the model.
fit <- train(x_subset, y, method = "glm")
fit$results

# Now, instead of using a random selection of predictors, we are going to search for those that are most predictive of the outcome. 
# We can do this by comparing the values for the  y=1  group to those in the  y=0  group, for each predictor, using a t-test.
# You can do perform this step like this:
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

# Create an index ind with the column numbers of the predictors that were "statistically significantly" associated with y.
# Use a p-value cutoff of 0.01 to define "statistically significantly
ind <- which(pvals <= 0.01)
length(ind)

# redefinining x_subset to be the subset of x
# defined by the columns showing "statistically significant" association with y
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

# Re-run the cross-validation again, but this time using kNN. 
# Try out the following grid k = seq(101, 301, 25) of tuning parameters.
# Make a plot of the resulting accuracies.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%.
# Because we used the entire dataset to select the columns in the model, the accuracy is too high. 
# The selection step needs to be included as part of the cross-validation algorithm, and then the cross-validation itself is performed after the column selection step.
# As a follow-up exercise, try to re-do the cross-validation, this time including the selection step in the cross-validation algorithm. The accuracy should now be close to 50%.


##### SECTION 2
library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")
names(tissue_gene_expression)

k = seq(1,7,2)

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)
