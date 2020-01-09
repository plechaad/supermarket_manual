install.packages("ROCR")
install.packages("pROC")
install.packages("PRROC")
library("ROCR")
library("pROC")
library("PRROC")

setwd("J:/rko/supermarket_vrvsmonitor")
dir()
data <- read.csv("adela_supermarket_summary_statistics .csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
?read.csv
str(data)
class(data$AVLT...deficit)

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
ROCError9AVLTdel <- plot(perf3, colorize = TRUE)
save.image(ROCError9AVLTdel, file = "ROCError9AVLTdel.jpg")
perfAUC3 <- performance (pred3, "tpr", "fpr", measure = "auc")
perfAUC3@y.values
png("ploterror9delavlt.png")
plot(perf3, colorize = TRUE)
dev.off()

pred4 <- prediction(data$Error_7, data$AVLTDEL...deficit)
perf4 <- performance(pred4, "tpr", "fpr")
plot(perf4, colorize = TRUE)
perfAUC4 <- performance (pred4, "tpr", "fpr", measure = "auc")
perfAUC4@y.values

#ROC by group = schizophrenia
pred <- prediction(data$Error_7[data$Group.1.E.2.S. == 1], data$AVLT...deficit[data$Group.1.E.2.S. == 1])
perfAUC <- performance (pred, "tpr", "fpr", measure = "auc")
perfAUC@y.values
perf <- performance (pred, "tpr", "fpr")


pred1 <- prediction(data$Error_9[data$Group.1.E.2.S. == 1], data$AVLT...deficit[data$Group.1.E.2.S. == 1])
perfAUC1 <- performance (pred1, "tpr", "fpr", measure = "auc")
perfAUC1@y.values
perf1 <- performance (pred1, "tpr", "fpr")
png("ploterror9avlt_schizophrenia.png")
plot(perf1, colorize = TRUE,
     main = "Pacienti SZ (N=19)", sub = "AUC = 0.68")
dev.off()

pred2 <- prediction(data$Error_7[data$Group.1.E.2.S. == 1], data$AVLTDEL...deficit[data$Group.1.E.2.S. == 1])
perfAUC2 <- performance (pred2, "tpr", "fpr", measure = "auc")
perfAUC2@y.values
perf2 <- performance (pred2, "tpr", "fpr")

pred3 <- prediction(data$Error_9[data$Group.1.E.2.S. == 1], data$AVLTDEL...deficit[data$Group.1.E.2.S. == 1])
perfAUC3 <- performance (pred3, "tpr", "fpr", measure = "auc")
perfAUC3@y.values
perf3 <- performance (pred3, "tpr", "fpr")
png("ploterror9avltdel_schizophrenia.png",)
plot(perf3, colorize = TRUE,
     main = "Pacienti SZ (N=19)", sub = "AUC = 0.9")
dev.off()
?plot

#ROC by group = young

pred4 <- prediction(data$Error_7[data$Group.1.E.2.S. == 2], data$AVLT...deficit[data$Group.1.E.2.S. == 2])
perfAUC4 <- performance (pred4, "tpr", "fpr", measure = "auc")
perfAUC4@y.values
perf4 <- performance (pred4, "tpr", "fpr")


pred5 <- prediction(data$Error_9[data$Group.1.E.2.S. == 2], data$AVLT...deficit[data$Group.1.E.2.S. == 2])
perfAUC5 <- performance (pred5, "tpr", "fpr", measure = "auc")
perfAUC5@y.values
perf5 <- performance (pred5, "tpr", "fpr")
png("ploterror9avlt_controls.png")
plot(perf5, colorize = TRUE)
dev.off()

pred6 <- prediction(data$Error_7[data$Group.1.E.2.S. == 2], data$AVLTDEL...deficit[data$Group.1.E.2.S. == 2])
perfAUC6 <- performance (pred6, "tpr", "fpr", measure = "auc")
perfAUC6@y.values
perf6 <- performance (pred6, "tpr", "fpr")

pred7 <- prediction(data$Error_9[data$Group.1.E.2.S. == 2], data$AVLTDEL...deficit[data$Group.1.E.2.S. == 2])
perfAUC7 <- performance (pred7, "tpr", "fpr", measure = "auc")
perfAUC7@y.values
perf7 <- performance (pred7, "tpr", "fpr")
png("ploterror9avltdel_controls.png")
plot(perf7, colorize = TRUE)
dev.off()

# pROC nefunguje

pROCobject <- roc(data$AVLT...deficit, data$Error_7,
                  smooth = TRUE, ci = TRUE, ci.alpha = 0.9, stratified = TRUE,
                  plot = TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROCobject)
plot(sens.ci, type="shape", col="lightblue")
?ci.se
?roc

