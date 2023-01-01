library(tidyverse)
library(caret)
# An education expert is advocating for smaller schools. 
# The expert bases this recommendation on the fact that among the best performing schools, many are small schools. 
# Let's simulate a dataset for 1000 schools.
# First, let's simulate the number of students in each school, using the following code:
set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent from size.
# This is the parameter we want to estimate in our analysis. 
# The true quality can be assigned using the following code:
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. 
# There is random variability in test taking, so we will simulate the test scores as normally distributed with the average determined by the school quality with a standard deviation of 30 percentage points. 
# This code will simulate the test scores
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# What is the ID of the top school?
schools %>% arrange(desc(score)) %>% slice(1:10) %>% summarize(median(size))

# What is the median school size overall?
median(schools$size)

# According to this analysis, it appears that small schools produce better test scores than large schools. 
# Four out of the top 10 schools have 100 or fewer students. But how can this be? We constructed the simulation so that quality and size were independent. 
# Repeat the exercise for the worst 10 schools.
schools %>% arrange(desc(score)) %>% slice(991:1000) %>% summarize(median(size))

# From this analysis, we see that the worst schools are also small. 
# Plot the average score versus school size to see what's going on. 
# Highlight the top 10 schools based on the true quality.
schools %>% ggplot(aes(size, score)) + geom_point()


schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

# Let's use regularization to pick the best schools. 
# Regularization shrinks deviations from the average towards 0. 
# To apply regularization here, we first need to define the overall average for all schools
overall <- mean(sapply(scores, mean))
alpha <- 25
b_i <- size + (score - overall)/(size + alpha))
schools %>% mutate(newscore = if(score > overall, b_i))  %>% 
  
  arrange(desc(score)) %>% slice(1:10)
