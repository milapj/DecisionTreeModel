
### title: "422_HW2"
### author: "Milap Jhumkhawala"
### date: "10/2/2017"


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

##### This is the scatter plot matrix of the correlation graph between the attributes

##### Attribute pair with strongest correlation is : Total Bilirubin and Direct Bilirubin with correlation value of 0.87.

##### Attribute pair with weakest correlation is : Sex and Ratio of Albumin to Globulin, with correlation value of 0.00, not at all correlated. Attributes Sgpt Alamine Aminotransferase and Ratio of Albumin to Globulin also have 0 correlation between them(correlation  = 0.00). Pair of Total Proteins and Direct Bilirubin also have a correlation value of 0.00

##### Attribute pair with most negatively correlation is Age and Albumin, with correlation value of -0.27.

##### Attributes that appear to follow Gaussian distribution are: Age, Total Protiens, Albumin, Ratio of Albumin to Globulin.

##### Normalization helps reduce data redundancy.
model <- rpart(label ~ ., method="class", data=train)
rpart.plot(model, extra=104, fallen.leaves = T, type=4)

pred <- predict(model, test, type="class")
confusionMatrix(pred, test[, 11], positive = "1")

##### The Accuracy of the model on out of sample data(test data) is 68.24%
##### The TPR(Sensitivity) of the prediction is 0.809, TNR(Specificity) is 0.3857 and PPV is 0.754.


plotcp(model)
printcp(model)

model.pruned <- prune(model, cp=0.026)
rpart.plot(model.pruned, extra=104, fallen.leaves = T, type=4)

pred_pruned <- predict(model.pruned, test, type="class")
confusionMatrix(pred_pruned, test[, 11], positive = "1")

##### Pruning the tree, results in generalisation. Not pruning sometimes makes the model picks up patterns of the given trained data, which can result in low accuracy when predicted on out of sample data. Thus pruning is important.

## Dimensionality Reduction methods:
a <- c(1,3:10)
ILPD_scale <- scale(ILPD[a])
output.forest <- randomForest(label ~ ., 
                              data = train, importance = T)
print(output.forest)
print(importance(output.forest))
i=importance(output.forest)

pca <- prcomp(ILPD_scale, scale.=T, center=T)
summary(pca)

##### Dimensionality Reduction based on Correlation value between attributes, using RandomForest package which gives most important attributes based on % Mean Square Error and also doing PCA analysis on scaled ILPD dataset.

##### The attributes that are not so useful and can be removed are: 
##### 1. tb (linear with db), 
##### 2. tp, alb, sgpaa, sgoaa = According to PCA it only contributes to about 5% 
##### 4. ag --> %MSE of 1.66


b <- c(3,6,7,8,9,10)
ILPD_reduced <- ILPD[-b]
index_red <- sample(1:nrow(ILPD_reduced), size = 0.4*nrow(ILPD_reduced))
test_red <- ILPD_reduced[index_red, ]
train_red <- ILPD_reduced[-index_red, ]

model_red <- rpart(label ~., method="class", data=train_red)

pred_red <- predict(model_red, test_red, type="class")
confusionMatrix(pred_red, test_red[, 5])

##### The Accuracy of the reduced model is 73.82% which is 5.58% greater. The TPR(Sensitvity) is 0.8571 (greater by 4%) and TNR(Specificity) is 0.4308 (increased by 5%) and PPV is 0.7956 ( increased by 4%).
##### Since the Accuracy, TPR,TNR and PPV of new model have increased by average of 5%, the new reduced model is better.


# ROC and AUC of the model WITHOUT dimenionality reduction:
pred.rocr <- predict(model, newdata=test, type="prob")[,2]
f.pred <- prediction(pred.rocr, test$label)
f.perf <- performance(f.pred, "tpr", "fpr")

plot(f.perf, colorize=T, lwd=3)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]


# ROC and AUC of the reduced model:

pred.rocr_red <- predict(model_red, newdata=test_red, type="prob")[,2]
f.pred_red <- prediction(pred.rocr_red, test_red$label)
f.perf_red <- performance(f.pred_red, "tpr", "fpr")

plot(f.perf_red, colorize=T, lwd=3)
auc <- performance(f.pred_red, measure = "auc")
auc@y.values[[1]]

##### As shown above, the ROC curves of original module and reduced model. The AUC of original model is 0.6771 and AUC of reduced model is 0.710. 
##### The reduced model is better than origianl model since the Area under curve of reduced model is greater.

ILPD_missing <- read.csv("/Users/Milap/Desktop/CS422_DataMining/Indian_Liver_Patient_Dataset.csv", header=T, sep=",", comment.char = '#', na.strings = "??")
summary(ILPD_missing)
ILPD_missing[!complete.cases(ILPD_missing),]

ILPD_missing$X0.9[which(is.na(ILPD_missing$X0.9))] <- 0.94

##### The summary() function tells that Attribute X0.9 has 4 Missing values as 'NA'.
##### using ILPD_missing[!complete.cases(ILPD_missing),], we get all the rows with value 1 for those observation with Misssing Values. Thus observation 209, 241,253 and 312 in the X0.9 attribute have Missing value.
##### The corresponding value inmputed in the dataset used in previous problems is '0.94', which is the mean of X0.9 attribute.
##### ILPD_missing$X0.9[which(is.na(ILPD_missing$X0.9))] <- 0.94, this command imputes the mean of attribute X0.9 to all the missing values in the attribute.

