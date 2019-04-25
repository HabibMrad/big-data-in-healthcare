library(DataExplorer)

# Setta la working directory dove si trova il file corrente
# Funziona solo con R studio!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataset = read.table("dataset.csv",  na.strings=".", sep = "\t",  header=T, row.names=NULL)

# Rimuovo l'id della riga
dataset = dataset[, -1]

# Converto le variabili categoriche in categorie numeriche
dataset$Diam = ifelse(dataset$Diam == "<=2cm", 0, 1)
dataset$N = ifelse(dataset$N == "<4", 0, 1)
dataset$ER = ifelse(dataset$ER == "Negative", 0, 1)
dataset$Grade = ifelse(dataset$Grade == "Poorly diff", 1, 
                       ifelse(dataset$Grade == "Intermediate", 2, 3))

dataset$Diam = factor(dataset$Diam)
dataset$N = factor(dataset$N)
dataset$ER = factor(dataset$ER)
dataset$Grade = factor(dataset$Grade)
dataset$event = factor(dataset$event)

attributes_mean = apply(data.matrix(dataset), 2, mean)
attributes_sd = apply(data.matrix(dataset), 2, sd)
attributes_median = apply(data.matrix(dataset), 2, median)

# windows()
# plot_intro(dataset)
# windows()
# plot_bar(dataset)
# windows()
# plot_histogram(dataset)
# windows()
# plot_correlation(dataset)

cormat = cor(data.matrix(dataset))

outcome_correlation = cormat[2, ]

