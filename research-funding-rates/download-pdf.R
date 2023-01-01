# Download pdf and load required library

library("pdftools")
temp_file <- tempfile()
url <- "https://www.pnas.org/highwire/filestream/620531/field_highwire_adjunct_files/0/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)

txt <- pdf_text("pnas.201510159SI.pdf")

str(txt)
file.remove(temp_file)
