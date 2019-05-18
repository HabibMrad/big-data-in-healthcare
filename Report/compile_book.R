setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bookdown::render_book("index.Rmd")

