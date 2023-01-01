library(tidyverse)
library(dslabs)
library(readr)
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
?read_csv
dat <- read_csv(url)
dat1 <- read_csv(url, col_names = FALSE)
str(dat)
str(dat1)
url2 <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
co2_mauna_loa <- read_table(url2, skip = 56, col_names = TRUE)
str(co2_mauna_loa)