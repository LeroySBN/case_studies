# Building a Better Offensive Metric

# linear regression with two variables (multivariate variable)
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# Create a model with all five variables representing the hits, assuming they are joint normally
# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, singles = (H - X2B - X3B - HR) / G, doubles = X2B / G, triples = X3B / G, HR = HR / G, R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G, 
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1) + 
  geom_abline()
# So instead of using batting average or just the number of home runs
#as a measure for picking players, we can use our fitted model
#to form a more informative metric that relates
#more directly to run production.

# Account for per plate appearance (players who dont to play a full game hence affecting their overall average)
# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% # plate appearances per team per game, averaged across all teams
  pull(pa_per_game) %>% 
  mean

# compute the per-plate-appearance rates for players available in 2002 using data from 1999-2001 since we are picking players in 2002.
# Filter out players with small plate apprance to avoid small sample artifacts.
# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

#variability across players
players %>% ggplot(aes(R_hat)) + geom_histogram(binwidth = 0.5, color = "black")

# To actually build the teams, we will need to know the players' salaries, since we have a limited budget.
# We are pretending to be the Oakland A's in 2002 with only a $40 million budget.
# We also need to know the players' position because we're going to need one shortstop, one second baseman, one third baseman, et cetera.

# Start by adding 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# Filter out the desired list
#names(Fielding)
#players <- Fielding %>%  filter(yearID == 2002) %>% 
 # filter(!POS %in% c("OF", "P")) %>% # Remove outfielders and pitchers (Remain with a defensive position)
  #group_by(playerID) %>% 
  #top_n(1, G) %>% # Pick one position most played by each player
  #filter(row_number(G) == 1) %>% # Pick one in the case of ties
  #ungroup() %>% 
  #select(playerID, POS) %>% 
  #right_join(players, by = "playerID") %>% 
  #filter(!is.na(POS) & !is.na(salary))

# add players' first and last names
#players <- Master %>%
 # select(playerID, nameFirst, nameLast, debut) %>%
  #mutate(debut = as.Date(debut)) %>%
  #right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")

tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()

pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max)

players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst.x, nameLast.x, POS, salary.x, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary.x, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

