data("Batting")
str(Batting)
names(Batting)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10

top %>% as_tibble()

# Use the correct join or bind function to create a combined table of the names 
# and statistics of the top 10 home run (HR) hitters for 2016.

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
names(top_names)

# Inspect the Salaries data frame
data("Salaries")
str(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
names(top_salary)

# Inspect the AwardsPlayers table

AwardsPlayers2016 <- AwardsPlayers %>% filter(yearID==2016)
names(AwardsPlayers2016)

top_with_award <- top_names %>% semi_join(AwardsPlayers2016)

Nontop10awards <- AwardsPlayers2016 %>% anti_join(top_names) %>% arrange(playerID)
unique(Nontop10awards$playerID)
