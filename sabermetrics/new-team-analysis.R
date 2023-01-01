# compare the stability of singles and BBs between 2000 and the average of 1999-2001 seasons
library(Lahman)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>% 
  filter(pa >= 100) %>% 
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>% 
  ungroup() %>% 
  select(playerID, mean_singles, mean_bb)

mean(duplicated(bat_01$playerID))

bat_01 %>%
  filter(mean_bb > 0.2) %>% nrow()

bat_01 %>%
  filter(mean_singles > 0.2) %>% nrow()

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# Join the two tables
bat_03 <- inner_join(bat_01,bat_02)

# Calculate correlation between the singles and bb of the two tables
bat_03 %>% summarize(r = cor(singles, mean_singles)) %>% pull(r)
bat_03 %>% summarize(r = cor(bb, mean_bb)) %>% pull(r)

# Scatterplot and linear model of both singles datasets
bat_03 %>% select(singles, mean_singles) %>% 
  ggplot(aes(singles, mean_singles)) +
  geom_point()
ggsave("figs/scatterplot-singles-2002-vs-99to01.png")


lm(singles ~ mean_singles, data = bat_03)

# Scatterplot and linear model of both BB datasets
bat_03 %>% select(bb, mean_bb) %>% 
  ggplot(aes(bb, mean_bb)) +
  geom_point()
ggsave("figs/scatterplot-BB-2002-vs-99to01.png")

lm(bb ~ mean_bb, data = bat_03)