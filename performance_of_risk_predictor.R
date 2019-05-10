library(survival)
library(pROC)
library(pec)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataset = read.table("dataset.csv",  na.strings=".", sep = "\t",  header=T, row.names=NULL)

# Rimuovo l'id della riga
dataset = dataset[, -1]

# Prendo solo le covariate cliniche
dataset = dataset[1:7]

# Converto le variabili categoriche in categorie numeriche
dataset$Diam = ifelse(dataset$Diam == "<=2cm", 0, 1)
dataset$N = ifelse(dataset$N == "<4", 0, 1)
dataset$ER = ifelse(dataset$ER == "Negative", 0, 1)
dataset$Grade = ifelse(dataset$Grade == "Poorly diff", 0, 
                       ifelse(dataset$Grade == "Intermediate", 1, 2))


model = coxph(formula = Surv(time, event) ~ Diam + N + ER + factor(Grade) + Age, data = dataset)
summary(model)

# Predictiveness --------------------------------------------------------------------------------
fit = survfit(model, newdata = dataset)
dataset$riskdeath = 1 - as.numeric(summary(fit, times = 12)$surv)

dataset$event.12m = ifelse(dataset$time <= 12 & dataset$event == 1, 1, 0)

estmodel = survfit(Surv(riskdeath, event.12m) ~ 1, data=dataset)


# Predictiveness curve of model:
plot((1 - estmodel$surv) * 100, estmodel$time, main = '', type = 'l', 
     ylim = c(0,1), lwd = 3, ylab = 'r', cex.lab = 1.7, cex.axis = 1.7,
     xlab = expression(paste('P(riskscore',''<='r)*100')), xaxt = "n", yaxt = "n", frame = F) 
axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels = NA, pos = 0)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1), labels = c(0,0.2,0.4,0.6,0.8,1), cex.axis = 1.7, pos = 0)
axis(1, at = c(0,20,40,60,80,100), labels = c(0,20,40,60,80,100), cex.axis = 1.7, pos = 0)


# Predictiveness curve of a useless model:
p = sum(dataset$event.12m) / dim(dataset)[1]
lines(c(0, 100), c(p, p), lty = 2, lwd = 3, col = 'gray') 
text(40, 0.1, labels = bquote(rho ==  .(round(p,3)*100) ~ '%'), cex = 1.7) 

# Predictiveness curve of the ideal risk predictor:
lines(c(0, (1 - p) * 100), c(0, 0), lwd = 4)
lines(c((1 - p) * 100, (1 - p) * 100), c(0, 1), lwd = 4)
lines(c((1 - p) * 100, 100), c(1, 1), lwd = 4)

# Brier score
brier_score = mean((dataset$event.12m - dataset$riskdeath) ^ 2)

# Brier Score under strong calibration
brier_score_sc <- mean(dataset$riskdeath * (1 - dataset$riskdeath))

# Discrimination --------------------------------------------

roc_model = roc(dataset$event.12m, dataset$riskdeath)
plot(1 - roc_model$specificities, roc_model$sensitivities, 
     type = 'l', ylab = 'TPF', xlab = 'FPF', lwd = 3, xaxt = "n", yaxt = "n", 
     xlim = c(0,1), cex.lab = 1.7, frame = F)
axis(1, at = c(0,0.25,0.5,0.75,1), labels = NA, pos = 0)
axis(1, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), cex.axis = 1.7, pos = 0)
axis(2, at = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), cex.axis = 1.7, pos = 0)
lines(x = c(0, 1), y = c(0, 1))

Youden = roc_model$sensitivities + roc_model$specificities - 1
optimal.cut.off = roc_model$thresholds[Youden == max(Youden)]
cbind(optimal.cut.off, Youden = max(Youden))
points(1 - roc_model$specificities[roc_model$thresholds == optimal.cut.off],
           roc_model$sensitivities[roc_model$thresholds == optimal.cut.off],
           pch=0, cex=1.7)

AUC = roc_model$auc

# Brier score (adjusted for censoring) and the c-index. --------------------------------

# Non so se vada
model = coxph(formula = Surv(time, event) ~ Diam + N + ER + factor(Grade) + Age, data = dataset, x = T)
summary(model)

fit = survfit(model, newdata = dataset)
dataset$riskdeath = 1 - as.numeric(summary(fit, times = 12)$surv)

set.seed(10052019)
PredError = pec(
  model,
  maxtime = 17.65913758,
  formula = Surv(time, event == 1) ~ 1,
  data = dataset,
  cens.model = "marginal",
  splitMethod = "bootcv",
  B = 10,
  verbose = TRUE
)

print(PredError, times = seq(0, 17.65913758, 1))

par(mfrow = c(1,1))
plot(PredError)
