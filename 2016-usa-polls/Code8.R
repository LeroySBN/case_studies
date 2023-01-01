# select all national polls by one pollster (IPSOS)
one_pollster <- polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>% summarize(empirical = sd(spread), theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01, color = "black")
ggsave("figs/IPSOS-spread-distribution.png")
#distribution of the data does not look normal as the theory would predict, as we can see in this figure.
#Where is this extra variability coming from?
#This plot makes a strong case that the extra variability comes from time variation not accounted for by the theory that assumes that p is fixed across time.

#Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
ggsave("figs/IPSOS-spread-distribution-with-time-variability.png")

#Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))
ggsave("figs/IPSOS-clinton-trump-candidate-percentage-with-time-variability.png")

