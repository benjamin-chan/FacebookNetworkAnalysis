require(rmarkdown)
render(file.path(getwd(), "FacebookNetwork.Rmd"), output_format="html_document")
file.rename(file.path(getwd(), "FacebookNetwork.md"), file.path(getwd(), "README.md"))
