#define the results object used for empirical Bayes election forecasting.

library(tidyverse)
library(dslabs)

polls <- polls_us_election_2016 %>% filter(state == "U.S." & enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% filter(enddate == max(enddate)) %>% ungroup()

results <- one_poll_per_pollster %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#Compute the posterior mean, standard error, credible interval and probability

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)
#This gives us a probability of almost 1, 99.9999%. That's the probability that we're going to report for Clinton winning the popular vote. We're saying it's almost 100%. This seems to be a little bit too overconfident. Also it is not in agreement with FiveThirtyEight's 81.4%.

#simulate 6 data points from 1 pollsters
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#simulate 6 data points from 5 pollsters
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))})
boxplot(X~J, data = house-effect-level-1)

#the model above does not account for pollster to pollster variability.
#We introduce h to represent the house effect
#in this model, we assume the average house effect is 0. We think that for every pollster that's biased in favor of one party, there's another that is favored in favor of the other, so it all averages out.
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))})
boxplot(X~J, data = house-effect-level-2)

#Here we introduce b to account for the general bias (bias of a pollster in favor of one party to the other)
#Note that sigma now includes an estimate of the variability due to general bias sigma-b=.025 .
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)
#we get a probability of Clinton winning the popular vote of 81.7%-much lower than the 99.999--because we're including the general bias variability.
