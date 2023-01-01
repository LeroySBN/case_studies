# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

# extract all tables in the html file and select the needed table with the data
tab <- h %>% html_nodes("table")
tab <- tab[[2]]

# convert html table into a data frame
tab <- tab %>% html_table
class(tab)
tab
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)