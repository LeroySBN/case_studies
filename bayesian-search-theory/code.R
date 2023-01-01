options(digits = 3)
pr_A <- 0.2
pr_B <- 0.6
pr_C <- 0.15
pr_D <- 0.05
pr_missed <- 0.1
pr_different <- 1
pr_found <- 0.9



#probability the plane is not in B
prBN <- 1 - pr_B
prBN

#probability the plane is in B but is not found
prBNF <- pr_B * pr_missed
prBNF

#probability the plane is not found in B on day 1
pr_Bmissed <- 1 - pr_B + pr_B*pr_missed
pr_Bmissed

#posterior probability that the plane is in area B given that it is not found on day 1
(pr_missed*pr_B)/pr_Bmissed


#posterior probability that the plane is in area C given that it is not found on day 1
pr_different * pr_C/pr_Bmissed

#probability of finding the plane on the first day
pr_B * pr_found

#probability that the plane is not found on the first day but is found on the second day
pr_A * pr_found

#probability that the plane is found within 2 days
(pr_B * pr_found) + (pr_A * pr_found)

