fn <- "_main.Rmd"
if (file.exists(fn)) 
  file.remove(fn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bookdown::render_book("index.Rmd")

