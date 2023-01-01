library(tidyverse)
library(stringr)
library(dslabs)
data("murders")

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern)