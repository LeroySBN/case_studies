library(dplyr)
library(ggplot2)

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") & enddate >= "2016-10-15" & state == "U.S.") %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(spread, pollster)) + geom_boxplot() + geom_point()
ggsave("figs/filtered- pollster-spread-barplot.png")

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))
sigma

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), sd=sd(spread), N=n())
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- max(res$avg) - min(res$avg)
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$sd[2]^2/res$N[2] + res$sd[1]^2/res$N[1])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
ci

# Calculate the p-value
(1- pnorm(estimate, 0 ,se_hat)) * 2