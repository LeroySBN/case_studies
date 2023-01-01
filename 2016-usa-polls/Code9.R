# Load the libraries and data
library(dplyr)
library(dslabs)
library(tidyverse)
data("polls_us_election_2016")
names(polls_us_election_2016)

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
names(polls)

# Create an object called `cis` that has the estimate of the proportion of Clinton voters for each poll, standard error of the spread and its confident intervals
cis <- polls %>% mutate(X_hat=(spread+1)/2, se=2*sqrt(X_hat*(1-X_hat)/samplesize),lower=spread - qnorm(0.975)*se, upper=spread + qnorm(0.975)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
names(ci_data)
# Create an object that summarizes the proportion of confidence intervals that contain the actual value.
p_hits <- ci_data %>% mutate(hit = (actual_spread >= lower & actual_spread <=upper)) %>% summarize(proportion_hits=mean(hit))
p_hits

# proportion of hits for each pollster that has at least 5 polls.
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% mutate(hit = (actual_spread >= lower & actual_spread <=upper)) %>% group_by(pollster) %>% filter(n() >= 5) %>% summarize(proportion_hits=mean(hit)) %>% arrange
p_hits
# Stratify by pollsters
p_hits <- ci_data %>% 
  mutate(hit = (actual_spread >= lower & actual_spread <=upper)) %>% 
  group_by(pollster) %>% 
  filter(n() >= 5) %>% 
  summarize(proportion_hits=mean(hit), n=n(), grade=grade[1]) %>% 
  arrange(desc(proportion_hits))
p_hits
# Stratify by state and plot the data
p_hits <- ci_data %>% 
  mutate(hit = (actual_spread >= lower & actual_spread <=upper)) %>% 
  group_by(state) %>% 
  filter(n() >= 5) %>% 
  summarize(proportion_hits=mean(hit), n=n()) %>% 
  arrange(desc(proportion_hits))
p_hits
p_hits %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat = "identity") + coord_flip()
ggsave("figs/simulated-vs-actual-results-by-state.png")

# Calculate the difference between the predicted and actual spread to indicate if the correct winner was predicted
errors <- ci_data %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
errors
# Examine the last 6 rows of `errors`
tail(errors,n=6)
# Histogram of the errors
hist(errors$error)
# Median of the errors (GENERAL BIAS)
median(errors$error)

# Boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>% 
  ggplot(aes(state, error)) + geom_boxplot() + geom_point()
ggsave("figs/simulated-vs-actual-error-barplot.png")

# Boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>% 
  group_by(state) %>% 
  filter(n()>=5) %>% 
  ggplot(aes(state, error)) + geom_boxplot() + geom_point()
ggsave("figs/simulated-vs-actual-error-boxplot-by-state.png")

# Proportion of hits for each state that has more than 5 polls
p_hits <- errors %>% group_by(state) %>% 
  filter(n()>=5) %>% 
  summarize(proportion_hits=mean(hit), n=n()) %>%
  ggplot(aes(proportion_hits)) + geom_bar()
p_hits
#Plotting prediction results
p_hits %>% reorder(state, proportion_hits) %>% ggplot(aes(proportion_hits)) + geom_bar() + coord_flip()
ggsave("figs/bar-error")