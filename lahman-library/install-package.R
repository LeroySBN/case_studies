install.packages("Lahman")
library(tidyverse)
library(ggrepel)
library(Lahman)

str(LahmanData)
names(LahmanData)

Lahman::Master
Lahman::AwardsPlayers
data("Master")
str(Master)

data("AwardsPlayers")
str(AwardsPlayers)
names(AwardsPlayers)

Master %>% as_tibble()
