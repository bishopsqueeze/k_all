##------------------------------------------------------------------
## The purpose of this script is to:
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)
library(class)  ## for gbm()
library(foreach)
library(doMC)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/raw")

##------------------------------------------------------------------
## Caret tutorial
##------------------------------------------------------------------

## load the data objects:  mdrrClass (=binary classifiers), mdrrDescr (=predictors)
data(mdrr)

## identify nearZeroVariance columns
nzv             <- nearZeroVar(mdrrDescr)
filteredDescr   <- mdrrDescr[ , -nzv]

## identify correlated variables
descrCor        <- cor(filteredDescr)
highlyCorDescr  <- findCorrelation(descrCor, cutoff = 0.75)
filteredDescr   <- filteredDescr[ , -highlyCorDescr]

## define samples
set.seed(1)
inTrain         <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
trainDescr      <- filteredDescr[ inTrain, ]
testDescr       <- filteredDescr[-inTrain, ]
trainMDRR       <- mdrrClass[ inTrain ]
testMDRR        <- mdrrClass[-inTrain ]

## define a trainControl object
fitControl      <- trainControl(
                        method="repeatedcv",
                        number=10,
                        repeats=3,
                        returnResamp="all")

gbmGrid         <- expand.grid(
                            .interaction.depth=c(1,3),
                            .n.trees=c(10, 50, 100, 200, 250, 300),
                            .shrinkage = 0.1
                        )

## fit
system.time({
gbmFit1         <- train(trainDescr, trainMDRR,
                        method="gbm",
                        preProcess=NULL,
                        trControl=fitControl,
                        verbose=FALSE)
})

system.time({
    gbmFit2         <- train(trainDescr, trainMDRR,
    method="gbm",
    preProcess=NULL,
    trControl=fitControl,
    tuneGrid=gbmGrid,
    verbose=FALSE)
})




