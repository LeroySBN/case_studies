# calculate the spread of pollsters who held polls more than five times
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-15" & state == "U.S.") %>% group_by(pollster) %>% filter(n() >= 5) %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>% summarize (avg=mean(spread), s=sd(spread))
var