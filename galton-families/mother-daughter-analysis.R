#set.seed(1989) #R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>%    
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight)

# calculate values to plot regression line on original data
female_heights %>%
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))

rf <- female_heights %>% summarize(r = cor(mother, daughter)) %>% pull(r)

# predicted height of a daughter with a 60 inch tall father
fconditional_avg <- female_heights %>%
  filter(round(mother) == 60) %>%
  summarize(avg = mean(daughter)) %>%
  pull(avg)
fconditional_avg

# stratify mothers' heights to make a boxplot of daughter heights
female_heights %>% mutate(mother_strata = factor(round(mother))) %>%
  ggplot(aes(mother_strata, daughter)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
female_heights %>%
  mutate(mother = round(mother)) %>%
  group_by(mother) %>%
  summarize(daughter_conditional_avg = mean(daughter)) %>%
  ggplot(aes(mother, daughter_conditional_avg)) +
  geom_point() + geom_abline()

rf <- female_heights %>% summarize(r=cor(mother, daughter)) %>% .$r

female_heights %>% 
  mutate(mother = round(mother)) %>% 
  group_by(mother) %>% 
  summarize(daughter = mean(daughter)) %>% 
  mutate(z_mother = scale(mother), z_daughter = scale(daughter)) %>% 
  ggplot(aes(z_mother, z_daughter)) +
  geom_point() +
  geom_abline(intercept = 0, slope = rf)

# calculate values to plot regression line on original data
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
rf <- cor(female_heights$mother, female_heights$daughter)
mf <- rf * s_y/s_x
bf <- mu_y - mf*mu_x

mu_y + rf * 60

# add regression line to plot (son given father)
female_heights %>%
  ggplot(aes(mother, daughter)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = bf, slope = mf)

# If we plot the data in standard units, intercept=0
female_heights %>%
  ggplot(aes(scale(mother), scale(daughter))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = rf)