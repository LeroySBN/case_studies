library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% filter(state == "Florida" & enddate >= "2016-11-04" ) %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)

# Calculate average spread (`avg`) and the standard error (`se`)
results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(n()))
results

#The CLT tells us that our estimate of the spread d^ has a normal distribution with expected value d and standard deviation sigma
#Estimate posterior distribution. we set mu=0 and tau=0.01
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
sigma <- results$se
B <- sigma^2/(sigma^2+tau^2)
B

# Calculate the expected value of the posterior distribution
#B * mu + (1-B)*Y
# Compute the standard error of the posterior distribution
#sqrt(1/(1/sigma^2 + 1/tau^2))


# Assign the expected value of the posterior distribution
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (Probability that Trump wins Florida)
pnorm(0,exp_value,se)

# Construct the 95% credible interval.
ci <- B*mu + (1-B)*Y + c(-1,1)*qnorm(0.975)*sqrt( 1/ (1/sigma^2 + 1/tau^2))
ci