
### Q 2.2
library(psych)
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)
library(randomForest)

rm(list=ls())
setwd("/Users/Milap/Desktop/CS422_DataMining/")

ILPD <- read.csv("/Users/Milap/Desktop/CS422_DataMining/ILPD.csv", header=T, sep=",", comment.char = '#')
set.seed(100)
index <- sample(1:nrow(ILPD), size = 0.4*nrow(ILPD))
test <- ILPD[index, ]
train <- ILPD[-index, ]
pairs.panels(ILPD)

model <- rpart(label ~ ., method="class", data=train)
rpart.plot(model, extra=104, fallen.leaves = T, type=4)
pred <- predict(model, test, type="class")
confusionMatrix(pred, test[, 11], positive = "1")

plotcp(model)
printcp(model)
model.pruned <- prune(model, cp=0.026)
rpart.plot(model.pruned, extra=104, fallen.leaves = T, type=4)
pred_pruned <- predict(model.pruned, test, type="class")
confusionMatrix(pred_pruned, test[, 11], positive = "1")


a <- c(1,3:10)
ILPD_scale <- scale(ILPD[a])
output.forest <- randomForest(label ~ ., 
                              data = train, importance = T)
print(output.forest)
print(importance(output.forest))
i=importance(output.forest)
pca <- prcomp(ILPD_scale, scale.=T, center=T)
summary(pca)


b <- c(3,6,7,8,9,10)
ILPD_reduced <- ILPD[-b]
index_red <- sample(1:nrow(ILPD_reduced), size = 0.4*nrow(ILPD_reduced))
test_red <- ILPD_reduced[index_red, ]
train_red <- ILPD_reduced[-index_red, ]
model_red <- rpart(label ~., method="class", data=train_red)
pred_red <- predict(model_red, test_red, type="class")
confusionMatrix(pred_red, test_red[, 5])


pred.rocr <- predict(model, newdata=test, type="prob")[,2]
f.pred <- prediction(pred.rocr, test$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]


pred.rocr_red <- predict(model_red, newdata=test_red, type="prob")[,2]
f.pred_red <- prediction(pred.rocr_red, test_red$label)
f.perf_red <- performance(f.pred_red, "tpr", "fpr")
plot(f.perf_red, colorize=T, lwd=3)
auc <- performance(f.pred_red, measure = "auc")
auc@y.values[[1]]




#### Q 2.2

ILPD_missing <- read.csv("/Users/Milap/Desktop/CS422_DataMining/Indian_Liver_Patient_Dataset.csv", header=T, sep=",", comment.char = '#', na.strings = "??")
summary(ILPD_missing)
ILPD_missing[!complete.cases(ILPD_missing),]
ILPD_missing$X0.9[which(is.na(ILPD_missing$X0.9))] <- 0.94
