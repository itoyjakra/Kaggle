initialProcess <- function(data)
{
    #data$Happy <- as.factor(data$Happy)
    levels(data$Income) <- c(NA, 100000, 25000, 50000, 75000, 150000, 0)
    data$Income <- as.numeric(levels(data$Income))[data$Income]
    
    levels(data$Income) <- make.names(levels(data$Income), unique=TRUE)
    levels(data$YOB) <- make.names(levels(data$YOB), unique=TRUE)
    
    vars.to.impute <- c("Income", "YOB")
    imputed <- complete(mice(data[vars.to.impute]))
    data[vars.to.impute] <- imputed
    
    cols <- grep("Q", colnames(data))
    cols <- c(cols, which(colnames(data) == "Party"))
    cols <- c(cols, which(colnames(data) == "EducationLevel"))
    cols <- c(cols, which(colnames(data) == "HouseholdStatus"))
    cols <- c(cols, which(colnames(data) == "Gender"))
    for (col in cols)
    {
        levels(data[[col]]) <- c(levels(data[[col]]), "skip")
        x <- which(data[[col]] == "")
        data[[col]][x] <- "skip"
        data[[col]] <- factor(data[[col]])
        levels(data[[col]]) <- make.names(levels(data[[col]]), unique=TRUE)
    }
    
#     skipCount <- rep(0, nrow(data))
#     for (row in 1:nrow(data))
#       skipCount[row] <- length(which(data[row,] == "skip"))/(ncol(data)-1)
#     data$skipCount <- skipCount
    
    return (data)
}

cvAUC <- function(formula, setTrain, k)
{
    set.seed(200)
    folds <- cvFolds(nrow(setTrain), K=k)
    AUC <- c()
    for (i in 1:10)
    {
        train <- setTrain[folds$subsets[folds$which != i], ]
        valid <- setTrain[folds$subsets[folds$which == i], ]
        foldglm <- glm(formula, data=train, family=binomial)
        validauc <- glmAUC(foldglm, valid)
        print (c(i, validauc))
        AUC <- c(AUC, validauc)
    }
    return((mean(AUC)))
}

myTree <- function(dataTrain, dataTest, cp)
{
    tree <- rpart(Happy ~ ., data=dataTrain, cp=cp)
    prp(tree)    
}

myForest<- function(dataTrain, dataTest, nTree)
{
    set.seed(345)
    rf<- randomForest(Happy ~ . , data=dataTrain, ntree=nTree)
    #print (importance(rf))
    
    rfPred <- predict(rf, newdata=dataTest, type="prob")
    rfROC <- prediction(rfPred[,2], dataTest$Happy)
    AUC <- as.numeric(performance(rfROC, "auc")@y.values)
    
    myList <- list("AUC" = AUC, "rf" = rf)
    return (myList)
}

#cvRF <- function(data, nFold, nRepeat, nTree, nTryList)
cvRF <- function(data, cvControl, nTree, nTryList)
{
  #  cvControl <- trainControl(method="repeatedcv", 
  #                            number = nFold, 
  #                            repeats = nRepeat,
  #                            classProbs = TRUE,
  #                            summaryFunction = twoClassSummary)
  #
    if (length(nTryList) > 0)
    {
      rfFit <- train(Happy ~ ., data=data,
                     method = "rf",
                     metric = "ROC",
                     tuneGrid = expand.grid(mtry = nTryList),
                     ntree = nTree,
                     trControl = cvControl)
    }
    else
    {
      rfFit <- train(Happy ~ ., data=data,
                     method = "rf",
                     metric = "ROC",
                     ntree = nTree,
                     trControl = cvControl)
    }
    
    return (rfFit)
}

#cvSVM <- function(data, nFold, nRepeat, nTune, varList)
#   cvControl <- trainControl(method="repeatedcv", 
#                             number = nFold, 
#                             repeats = nRepeat,
#                             classProbs = TRUE,
#                             summaryFunction = twoClassSummary)
cvSVM <- function(data, cvControl, nTune, varList)
{
  
  if (length(varList > 0))
  {
    formula <- as.formula(paste ( "Happy ~ ", paste(varList, collapse=" + ")))
    print(formula)
    svmFit <- train(formula, data=data,
                    method = "svmRadial",
                    metric = "ROC",
                    tuneLength = nTune,
                    preProc = c("center", "scale"),
                    trControl = cvControl)
  }
  else
  {
    svmFit <- train(Happy ~ ., data=data,
                    method = "svmRadial",
                    metric = "ROC",
                    tuneLength = nTune,
                    preProc = c("center", "scale"),
                    trControl = cvControl)
    
  }
  
  return (svmFit)
}


myGLM <- function(dataTrain, dataTest, formula, showSummary)
{
    #myglm <- glm(Happy ~  Party + Q99716 + Gender + Q122769 + HouseholdStatus + Q118237:Q101162 + Q118237 + Q101162 + Q107869 + Q119334 + Q120014 + Q102289 + Q115390 + Q102289 + Q102906 + Q116441 + Q98869 + Q121011, data=dataTrain, family=binomial)
    myglm <- glm(formula, data=dataTrain, family=binomial)
    if (showSummary == TRUE)
    {
        print (summary(myglm))
        #toselect.x <- summary(myglm)$coeff[-1,4] < 0.05
        #relevant.x <- names(toselect.x)[toselect.x == TRUE]
        #sig.formula <- as.formula(paste ( "Happy ~ ", paste(relevant.x, collapse=" + ")))
        #print (sig.formula)
    }
    
    logPred <- predict(myglm, newdata=dataTest, type="response")
    logROCR <- prediction(logPred, dataTest$Happy)
    AUC <- as.numeric(performance(logROCR, "auc")@y.values)
    
    myList <- list("AUC" = AUC, "myglm" = myglm)
    return (myList)
}

myGBM <- function(dataTrain, dataTest, nTree, shrinkParam, nFolds)
{
    mygbm <- gbm(Happy ~ ., data=dataTrain, distribution="gaussian", cv.folds=nFolds, n.trees=nTree, shrinkage=shrinkParam, interaction.depth=3, n.minobsinnode=10)
    best.iter <- gbm.perf(mygbm, method="cv")
    #print(summary(mygbm, n.trees=best.iter))
    gbmPred <- predict(mygbm, newdata=dataTest, n.trees=best.iter)
    gbmROCR <- prediction(plogis(gbmPred), dataTest$Happy)
    AUC <- as.numeric(performance(gbmROCR, "auc")@y.values)
    
    myList <- list("AUC" = AUC, "mygbm" = mygbm, "best" = best.iter)
    return (myList)
}

bestAUC <- function(m1, m2, fRange, dataTest)
{
    
    m1Pred <- predict(m1, newdata=dataTest, type="prob")
    m1ROCR <- prediction(m1Pred[,2], dataTest$Happy)
    AUC1 <- as.numeric(performance(m1ROCR, "auc")@y.values)
    
    m2Pred <- predict(m2, newdata=dataTest, type="response")
    m2ROCR <- prediction(m2Pred, dataTest$Happy)
    AUC2 <- as.numeric(performance(m2ROCR, "auc")@y.values)  
    
    AUC <- rep(length(fRange), 0)
    i <- 1
    for (f in fRange)
    {
        AUC[i] <- f*AUC1 + (1-f)*AUC2
        print (c(i, f, AUC[i]))
        i <- i + 1
    }
    return (AUC)
}

glmAUC <- function(model, data)
{
    pred <- predict(model, newdata=data, type="response")
    roc <- prediction(pred, data$Happy)
    auc <- as.numeric(performance(roc, "auc")@y.values)
    return (auc)
}
bestAUC_glm_gbm <- function(m_glm, m_gbm, fRange, dataTest)
{
    
    gbmPred <- predict(m_gbm$mygbm, newdata=dataTest, n.trees=m_gbm$best)
    gbmROCR <- prediction(plogis(gbmPred), dataTest$Happy)
    gbmAUC <- as.numeric(performance(gbmROCR, "auc")@y.values)
    
    glmPred <- predict(m_glm$myglm, newdata=dataTest, type="response")
    glmROCR <- prediction(glmPred, dataTest$Happy)
    glmAUC <- as.numeric(performance(glmROCR, "auc")@y.values)  
    
    AUC <- rep(length(fRange), 0)
    i <- 1
    for (f in fRange)
    {
        combPred <- f*plogis(gbmPred) + (1-f)*glmPred
        ROC <- prediction(combPred, dataTest$Happy)
        AUC[i] <- as.numeric(performance(ROC, "auc")@y.values)
        print (c(i, f, AUC[i]))
        i <- i + 1
    }
    myList <- list("best.AUC" = max(AUC), "best.f" = fRange[which.max(AUC)])
    return (myList)
}


setLibs <- function()
{
    library(randomForest)
    library(rpart)
    library(rpart.plot)
    library(ROCR)
    library(mice)
    library(caTools)
    library(gbm)
    library(caret)
    library(pROC)
    library(cvTools)
    library(e1071)
}
