# -----------------------------------------------------------
    # author = Effie
    # Date: August 2016
    # logistic regression + k-fold cross validation
    # for training a predictive model of social investors
# -----------------------------------------------------------
library(caret)
library(pROC)
library(glmnet)


# use top 7 principle component from PCA
# -----------------------------------------------------------
x <- as.data.frame(pca$x[,1:7])
x <- x[401:nrow(x),]
x$isSocial <- NA

for (i in 1:nrow(x)) {
    index <- rownames(x)[i]
    j <- which(grepl(index,nodelist$node))
    if (nodelist$isGroundTruth[j]==1)
        x$isSocial[i] <- 1
    else if (nodelist$GT_social[j]==1)
        x$isSocial[i] <- 1
    else if (nodelist$GT_non_social[j]==1)
        x$isSocial[i] <- 0
}

# separate known and unknown investors
unknown <- x[which(is.na(x$isSocial)),]
known <- x[which(!is.na(x$isSocial)),]

# set train and test set
train <- known[1:116,]
test <- known[117:nrow(known),]

# use 5-fold cross validation
fitControl <- trainControl(method = "cv",
                           number = 5,
                           classProbs=T,
                           savePredictions = T)

# fitting logistic regression
fit <- train(isSocial~.,
             data=train,
             method="glm",
             family="binomial",
             trControl=fitControl)

# fitting logistic regression with regularization
fit_reg <- train(isSocial~.,
              data=train, 
              method="glmnet",
              metric="RMSE",
              trControl=fitControl)


fit$finalModel$family
head(fit$pred)

# goodness of fit - likelihood ratio test
fit1 <- glm(isSocial~., 
            data=train, 
            family="binomial")
fit2 <- glm(isSocial~PC1+PC2+PC3+PC4+PC5+PC6, 
            data=train, 
            family="binomial")

library(lmtest)
lrtest(fit1,fit2) #significant

# variable importance
varImp(fit)
varImp(fit_reg)

# validation of predicted values
pred <- predict(fit, test)
auc <- roc(test$isSocial,pred)
accuracy <- table(pred, test[,"isSocial"])
sum(diag(accuracy))/sum(accuracy) #60% accuracy

pred2 <- predict(fit_reg,test)
auc2 <- roc(test$isSocial,pred2)
accuracy <- table(pred2, test[,"isSocial"])
sum(diag(accuracy))/sum(accuracy)

# AUC score for fit: 0.56
# AUC score for fit_reg: 0.54

# predict unknown investors
final <- predict(fit, unknown)
