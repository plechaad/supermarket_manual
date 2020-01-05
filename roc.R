install.packages("ROCR")
install.packages("pROC")
install.packages("PRROC")
library("ROCR")
library("pROC")
library("PRROC")

setwd("D:/Adéla/R/workspace/supermarket_vrvsmonitor")
dir()
data <- read.csv("adela_supermarket_summary_statistics .csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
?read.csv
str(data)
class(data$AVLT...deficit)
as.logical(data$AVLT...deficit)

# ROCR bez auc a bez confidence interval

?performance

pred <- prediction(data$Error_7, data$AVLT...deficit)
perfAUC <- performance (pred, "tpr", "fpr", measure = "auc")
perfAUC@y.values
perf <- performance (pred, "tpr", "fpr")

plot <- plot(perf, colorize = TRUE)


pred2 <- prediction(data$Error_7, data$AVLTDEL...deficit)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, colorize = TRUE)

perfAUC2 <- performance (pred2, "tpr", "fpr", measure = "auc")
perfAUC2@y.values

pred3 <- prediction(data$Error_9, data$AVLTDEL...deficit)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, colorize = TRUE)

perfAUC3 <- performance (pred3, "tpr", "fpr", measure = "auc")
perfAUC3@y.values


pred4 <- prediction(data$Error_7, data$AVLTDEL...deficit)
perf4 <- performance(pred4, "tpr", "fpr")
plot(perf4, colorize = TRUE)
perfAUC4 <- performance (pred4, "tpr", "fpr", measure = "auc")
perfAUC4@y.values

#ROC by group
pred <- prediction(data$Error_7[data$Group.1.E.2.S. == 1], data$AVLT...deficit[data$Group.1.E.2.S. == 1])
perfAUC <- performance (pred, "tpr", "fpr", measure = "auc")
perfAUC@y.values
perf <- performance (pred, "tpr", "fpr")

# pROC nefunguje

pROCobject <- roc(data$AVLT...deficit, data$Error_7,
                  smooth = TRUE, ci = TRUE, ci.alpha = 0.9, stratified = TRUE,
                  plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROCobject)
plot(sens.ci, type="shape", col="lightblue")
?ci.se
?roc



