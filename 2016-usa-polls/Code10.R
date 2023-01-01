# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-", "C-")) %>% group_by(grade,hit) %>% summarize(n=n()) %>% spread(grade,n)
totals

# Print the proportion of hits for grade A- polls to the console
prop_a <- sum(totals[2,3]) / sum(totals[3])
prop_a
# Print the proportion of hits for grade C- polls to the console
prop_c <- sum(totals[2,2]) / sum(totals[2])
prop_c

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test

# p-value of the chi-squared test
chisq_test$p.value

# Odds of getting the prediction right for grade C- polls
odds_C <- (sum(totals[2,2]) / sum(totals[2]))/(sum(totals[1,2]) / sum(totals[2]))
odds_C

# Odds of getting the prediction right for grade A- polls
odds_A <- (sum(totals[2,3]) / sum(totals[3]))/(sum(totals[1,3]) / sum(totals[3]))
odds_A

# Calculate odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

#We did not find meaningful differences between the poll results from grade A- and grade C- polls in this subset of the data, which only contains polls for about a week before the election.