US Murders Report
================
Leroy Buliro
2019 M10 15

Introduction
------------

This is a report on 2010 gun murder rates obtained from FBI reports. The original data was obtained from [this Wikipedia page](https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state).

We are going to use the following library:

``` r
library(tidyverse)
```

Next we shall download the data and save it.

``` r
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dest_file <- "data/murders.csv"
download.file(url, destfile = dest_file)
```

then load and wrangle the data.

Murder rate by state
--------------------

We note the large state to state variability by generating a barplot showing the murder rate by state:

![](report_files/figure-markdown_github/murder-rate-by-state-1.png)
