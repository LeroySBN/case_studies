# Is sophomore slump evident

library(Lahman)
# create a table with player ID, their names, and their most played position
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# create a table with only the ROY award winners and add their batting statistics
# Focus on batting average
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
ROY

# use the spread function to have one column for the rookie and sophomore years batting averages
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

# calculate the proportion of players who have a lower batting average their sophomore year
mean(ROY$sophomore - ROY$rookie <= 0)

# do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year)
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# see what happens to the worst performers of 2013
arrange(two_years, `2013`)

# see  the correlation for performance in two separate years
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

# There is no such thing as a sophomore slump.
# This is all explained with a simple statistical fact.
# The correlation of performance in two separate years is high but not perfect.
# Because a correlation is not perfect, regression tells us that on average, we expect high performers from 2013 to do a little bit worse in 2014.
# This regression to the mean. It's not a jinx. It's just due to chance.
# The rookies of the year are selected from the top values of x So it is expected that their y will regress to the mean.