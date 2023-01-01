library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

?Teams
names(Teams)
Lahman::Fielding
?Fielding
# Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-homeruns-vs-wins.png")

# Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-stolenbases-vs-wins.png")

# Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-basesonballs-vs-runs.png")

# Scatterplot of the relationship between at bat and runs
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-atbat-vs-runs.png")

# Scatterplot of the relationship between runs and at bat
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-runs-vs-atbat.png")

# Scatterplot of the relationship between wins and fielding error
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)
ggsave("figs/scatterplot-wins-vs-fieldingerror.png")
# As the number of errors per game increases, the win rate tends to decrease.

# Scatterplot of the relationship between triples and doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(doubles_per_game = X2B / G, triples_per_game = X3B / G) %>%
  ggplot(aes(doubles_per_game, triples_per_game)) + 
  geom_point(alpha = 0.5) + geom_abline()
ggsave("figs/scatterplot-tripples-vs-doubles.png")
# There is no clear relationship between doubles per game and triples per game.

##########################
##########################

baseball_sabermetrics <- Teams %>%  filter(yearID %in% 1961:2001) %>% mutate( 
  R_per_game = R/G, AB_per_game = AB/G,  
  win_rate = W / G, E_per_game = E / G, 
  doubles_per_game = X2B / G, triples_per_game = X3B / G)
str(baseball_sabermetrics)
write.csv(baseball_sabermetrics, file = "data/baseball_sabermetrics.csv")
  
##########################
##########################
  
# Correlation coefficient between number of runs per game and number of at bats per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate( R_per_game = R/G, AB_per_game = AB/G) %>% 
  summarize (r=cor(R_per_game,AB_per_game)) %>% 
  pull(r)

# Correlation coefficient between number of wins per game and number of errors per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate( win_rate = W / G, E_per_game = E / G) %>% 
  summarize (r=cor(win_rate,E_per_game)) %>% 
  pull(r)

# Correlation coefficient between number of doubles per game and number of tripples per game?
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate( doubles_per_game = X2B / G, triples_per_game = X3B / G) %>% 
  summarize (r=cor(doubles_per_game,triples_per_game)) %>% 
  pull(r)

# find regression line for predicting runs from BBs
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))