The data shows scientific funding rates by gender in the Netherlands.

The data come from a paper published in the prestigious journal PNAS. However, the data are not provided in a spreadsheet; they are in a table in a PDF document. We could extract the numbers by hand, but this could lead to human error. Instead we can try to wrangle the data using R.

Data source: https://www.pnas.org/content/112/40/12349.abstract

download-pdf.R
We start by downloading the PDF document then importing it into R.

wrangle-data.R
In this section, we extract data from the pdf to build a data frame.

analysis-fishers-exact-test.R
We use R.A. Fisher's exact test to establish if there's gender bias in awarding funds.