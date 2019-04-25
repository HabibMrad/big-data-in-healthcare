descriptive_latex_table = function (columns_name, mean, median, sd)
{
  sink("table.txt")
  
  i = 1
  for (column in columns_name) 
  {
    fmean = formatC(mean[i], digits = 3, format = "f")
    fmedian = formatC(median[i], digits = 3, format = "f")
    fsd = formatC(sd[i], digits = 3, format = "f")
    row = paste(column, fmean, fmedian, fsd, sep = "  &  ", collapse = NULL)
    row = paste(row, "  \\\\\n")
    cat(row)  
    i = i + 1
  }
  
  sink()
}