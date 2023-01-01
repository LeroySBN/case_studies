library(dslabs)
library(tidyverse)

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>% filter(enddate == max(enddate)) %>% ungroup()

# histogram of spread estimates
one_poll_per_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.
results <- one_poll_per_pollster %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)