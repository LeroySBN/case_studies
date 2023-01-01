library(rvest)
library(tidyverse)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
nodes
str(nodes)
html_text(nodes[[8]])
html_table(nodes[[8]])

html_text(nodes[[10]])
tab_1 <- html_table(nodes[[10]]) %>% select(X2,X3,X4) %>% setNames(c("Team", "Payroll", "Average")) 
tab_1 <- tab_1[-(1),]
tab_1

html_text(nodes[[19]])
tab_2 <- html_table(nodes[[19]]) %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- tab_2[-(1),]
tab_2

tab <- full_join(tab_1,tab_2, by="Team")
tab
unique(tab$Team)
