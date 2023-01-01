# Load required libraries
library(tidyverse)
library(rvest)

# Keep the page we want
raw_data_research_funding_rates <- txt[2]

# Examine the data
raw_data_research_funding_rates %>% head ()

# Each line on the page, including the table rows, is separated by the symbol for newline: \n
# Create a list with the lines of the text as elements
tab <- str_split(raw_data_research_funding_rates, "\n")
tab
str(tab)

tab <- tab[[1]]
tab %>% head

# Information for the column names is the third and forth entires
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# Extracting the table data
the_names_1 %>% head
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2 %>% head
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Join these to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
tmp_names
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

# Get the actual data

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)

write.csv(new_research_funding_rates, file = "data/new_research_funding_rates.csv")
