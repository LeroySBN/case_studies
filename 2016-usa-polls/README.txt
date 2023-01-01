ELECTION FORECASTING

Code.R
We analyze real 2016 US polling data organized by FiveThirtyEight. We start by using reliable national polls taken within the week before the election to generate an urn model.
For illustrative purpose, we will assume that there are only two parties, Clinton and Trump.
Our initial estimate of the spread did not include the actual spread. Part of the reason is that different pollsters have different numbers of polls in our dataset, and each pollster has a bias.
Pollster bias reflects the fact that repeated polls by a given pollster have an expected value different from the actual spread and different from other pollsters. Each pollster has a different bias.
The urn model does not account for pollster bias. We will develop a more flexible data-driven model that can account for effects like bias.

Code2.R
Instead of using an urn model where each poll is a random draw from the same distribution of voters, we instead define a model using an urn that contains poll results from all possible pollsters.
We assume the expected value of this model is the actual spread  d=2p−1. Our new standard error now factors in pollster-to-pollster variability. It can no longer be calculated from  p  or  d  and is an unknown parameter.
The central limit theorem still works to estimate the sample average of many polls  (X1,...,XN) because the average of the sum of many random variables is a normally distributed random variable with expected value  d  and standard error sigma. We can estimate the unobserved data as the sample standard deviation, which is calculated with the sd function.

Code3.R (First level of model - prior distribution)
We isolate two pollsters and measure variability between Clinton and Trump.

Code4.R (Second level of model - sampling distribution)
We filter out pollsters who held more than five polls and measure variability between Clinton and Trump.

Code5.R (Second level of model - sampling distribution)
Florida is one of the most closely watched states in the U.S. election because it has many electoral votes and the election is generally close. We measure poll variability of the state between Clinton and Trump.

Code6.R
In this code, we assume that we don't have any poll data and use historical data that shows the winner of the popular vote has an average spread of about 3.5% (tau=0.035). Now we can use the formulas for the posterior distribution for the parameter d (the spread) to report the probability of d being bigger than 0 given the observed poll data.

This gives us a probability of almost 1, 99.9999%. That's the probability that we're going to report for Clinton winning the popular vote. We're saying it's almost 100%. This seems to be a little bit too overconfident. Also it is not in agreement with FiveThirtyEight's 81.4%. What explains this difference? To understand why this happens we can look at what happens after elections. After elections are over, one can look at the difference between the pollsters' predictions and the actual results. An important observation that our model does not account for is that it is common to see what is called a general bias that affects many pollsters in the same way. There is no good explanation for this. But we do observe it in historical data. One election the average of polls favors Democrats by 2%. The next election they favor Republicans with 1%. Than next there's no bias. Then the next Republicans are favored are 3%, and so on. In 2016, the polls were biased in favor of the Democrats by about 1% or 2%. Although we now know this bias, before the election we had no way of knowing it. So we can't correct our polls accordingly. What we can do is including a term in our model that accounts for this variability.

Code6.R
Here we simulate a poll given we have a spread probability of 2.1 to establish the chances of Clinton winning the popular vote.

Code7.R
We predict the electoral college result for 2016.
We're going to use a Monte Carlo simulation to generate outcomes from simulated elections, then use this to make probability statements.
For each state, we apply the Bayesian approach we learned to generate an Election Day d for each state.

Code8.R
To make sure that the variability we observe is not due to pollster effects, we're going to stick to just one pollster. Using this code, we're going to look at Ipsos data. Since there's no pollster effect, perhaps the theoretical standard error will match the data-derived standard deviation.

Code9.R (Code Check)
Compare the actual election results to the confidence intervals of our simulated poll to check the proportion that fell in our prediction

Code10.R
In the previous code, we determined whether or not each poll predicted the correct winner for their state in the 2016 U.S. presidential election. Each poll was also assigned a grade by the poll aggregator. Now we're going to determine if polls rated A- made better predictions than polls rated C-.

[FINAL REPORT IN PROCESS]