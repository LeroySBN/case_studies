library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)

# Effects of BB on runs over time

Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>% 
  mutate(R = R/G,
         BB = BB/G,
         HR = HR/G) %>%
  select(R, BB, yearID) %>% 
  do(tidy(lm(R ~ BB+HR, data = .), conf.int=TRUE)) %>% 
  filter(term == "BB") %>% 
  select(yearID, estimate) %>% 
  ggplot(aes(yearID, estimate)) + geom_point() # Increased
ggsave("figs/scatterplot-BB-per-year-61to18.png")

##OR

res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

# Effect of year on the impact of BB
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)

# Effects of HR on runs over time
Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>% 
  mutate(R = R/G,
         HR = HR/G,
         BB = BB/G) %>%
  select(R, HR, yearID) %>% 
  do(tidy(lm(R ~ HR+BB, data = .), conf.int=TRUE)) %>% 
  filter(term == "HR") %>% 
  select(yearID, estimate) %>% 
  ggplot(aes(yearID, estimate)) + geom_point()
ggsave("figs/scatterplot-HR-per-year-61to18.png")


dbBB <-

fit3 <- dbBB%>%
  
