polls <- polls %>% 
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls %>% head
polls <- polls[-1,]
str(polls)


# Replace all N/As in the undecided column to 0
str_replace_all(polls$undecided, "N/A", "0")

# remove rows that don't have a percentage sign in the remain column
polls$remain
pattern <- "[a-z]" # Non numerical entries
str_detect(polls$remain, pattern)
remove(polls, list = c(16,75,82,111))


# Split dates to start date and end date
polls$dates
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)])

# Date analysis

library(dslabs)
library(lubridate)
data(brexit_polls)
names(brexit_polls)

brexit_polls %>% select(enddate)
x <- ymd(brexit_polls$startdate)
y <- ymd(brexit_polls$enddate)
