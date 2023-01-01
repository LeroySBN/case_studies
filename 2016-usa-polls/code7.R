library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#aggregating results from polls taken during the last week before the election.
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & !grepl("CD", "state") & enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results

# 10 closest races (battleground states)
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")
results
# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

#The code below assigns a standard deviation to the states that had just one poll by substituting the missing value by the median of the standard deviations of all the other states.
results <- results %>% mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
results

#We're going to use a Monte Carlo simulation to generate outcomes from simulated elections. 
#Then we're going to use this to make probability statements.
#For each state, we apply the Bayesian approach we learned to generate an Election Day d for each state.
#We could construct the priors for each state based on recent history.
#However, to keep it simple, we assign the same prior to each state.
#This prior is going to assume that we know nothing about what will happen.
#So the expected value will be 0.
#Because from election year to election year the results from a specific state don't change that much, we will assign a standard deviation of 2%. So tau is going to be now 0.02.
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    
    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    
    # total votes for Clinton
    .$clinton + 7    
  # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    
# over 269 votes wins election
# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)
ggsave("figs/simulated-clinton-election-win-with-no-general-bias.png")

#Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election