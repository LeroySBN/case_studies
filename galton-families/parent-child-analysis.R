library(tidyverse)
library(HistData)
data("GaltonFamilies")
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later

# Does one parent's height have a stronger association with child height?
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton # The return is a tibble

library(broom)

# Number of observations in each group
galton %>% filter(pair == "father_daughter") %>% nrow()
galton %>% filter(pair == "father_son") %>% nrow()
galton %>% filter(pair == "mother_daughter") %>% nrow()
galton %>% filter(pair == "mother_son") %>% nrow()

# Which pair has the strongest correlation in heights?
galton %>% group_by(pair) %>% summarize(cor(parentHeight, childHeight))
# Father-son has the highest and mother-son has lowest correlations


# Estimate of the pair coefficients
galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight") %>% 
  select(pair, estimate, conf.low, conf.high)

#Errorbar plot
galton %>% group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight") %>% 
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_errorbar() + geom_point()
ggsave("fig/errorbar-parent-child.png")

# Sets of parent-child heights significantly correlated at a p-value cut off of .05?
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
