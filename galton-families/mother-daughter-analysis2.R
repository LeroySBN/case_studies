#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# linear regression model predicting the mothers' heights using daughters' heights
fit <- lm(mother ~ daughter, data = female_heights)
summary(fit)

# Height of the first mother
min(female_heights$mother)

# Predicted height of the first mother
# predict Y directly
fit <- female_heights %>% lm(mother ~ daughter, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
female_heights %>%
  mutate(Y_hat = predict(lm(mother ~ daughter, data=.))) %>%
  ggplot(aes(daughter, Y_hat))+
  geom_line() + geom_smooth(method = "lm")


