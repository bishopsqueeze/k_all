##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### The goal here should be to
### [1] explode the choice parameters
### [3] use caret to identify near-zero variables
### [4] use caret to identify linear combinations of variables
### [5] ensure that the training and test data have identical sets
###     of variables ... dropping those that are mismatched
### [6] ensure that everything in the dataset can be expressed as
###     a numeric/integer variables
### [7] confirm that the format is generally consistent with what
###     is described in the predictive analytics paper
###
### [2] compute interactions between choices ?
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


### ??? maybe load these together to get a sense for what truly
###     is a near-zero variable

### STEP 0 -- remove superfluous variables (now that we have panels ... e.g., ABCDEFG)
### STEP 1 -- remove near-zero variables (to reduce dimensions)
### STEP 2 --

##------------------------------------------------------------------
## Load data (each should contain an object called "panel.list"
##------------------------------------------------------------------
load("X004_allstatePanelData_Train.Rdata")
train.list  <- panel.list; rm(panel.list)

load("X004_allstatePanelData_Test.Rdata")
test.list   <- panel.list; rm(panel.list)

##------------------------------------------------------------------
## One loop for test data (0) and one loop for training data (1)
##------------------------------------------------------------------
panel.test  <- list()
panel.train <- list()

for (i in 2:11) {
    
    ##------------------------------------------------------------------
    ## load the test data
    ##------------------------------------------------------------------
    test.data     <- test.list[[i]]$data
    test.len      <- test.list[[i]]$len
    test.sp       <- test.list[[i]]$sp
    
    ## expand factors into a set of binary variables
    cols.test     <- colnames(test.data)[c(grep("^[A-G][0-9]$", colnames(test.data)),
                                           grep("^[A-G]10$", colnames(test.data)),
                                           grep("^[A-G]11$", colnames(test.data)))]
    
    for (j in 1:length(cols.test)) {
        tmp.mat <- expandFactors(x=test.data[, eval(cols.test[j])], v=eval(cols.test[j]))
        if (j == 1) {
            test.exploded <- tmp.mat
        } else {
            test.exploded <- cbind(test.exploded, tmp.mat)
        }
    }
    all.test    <- cbind(test.data, test.exploded)
    all.test    <- all.test[, -(which(colnames(all.test) %in% cols.test))]

    ## drop the custday key
    all.test$custday_key.u <- NULL
    
    ## remove the superfluous variables
    all.test    <- all.test[ , -grep("ABCDEFG", colnames(all.test)) ]
    all.test    <- all.test[ , -grep("^[A-G]$", colnames(all.test)) ]
    
    ## scale the cost difference variables
    cols    <- colnames(all.test)[grep("^d[A-G]",colnames(all.test))]
    for (j in 1:length(cols)) {
        all.test[, eval(cols[j])] <- scale(as.numeric(all.test[,eval(cols[j])]))
    }

    ## scale the cost difference variables
    cols    <- colnames(all.test)[grep("^n[A-G]",colnames(all.test))]
    for (j in 1:length(cols)) {
        all.test[, eval(cols[j])] <- scale(as.numeric(all.test[,eval(cols[j])]))
    }
    
    ## save the results
    panel.test[[test.sp]]$data  <- all.test
    panel.test[[test.sp]]$sp    <- test.sp
    panel.test[[test.sp]]$len   <- test.len
    
    ##------------------------------------------------------------------
    ## load the training data
    ##------------------------------------------------------------------
    train.data    <- train.list[[i]]$data
    train.len     <- train.list[[i]]$len
    train.sp      <- train.list[[i]]$sp

    ## expand factors into a set of binary variables
    cols.train     <- colnames(train.data)[c(grep("^[A-G][0-9]$", colnames(train.data)),
                                           grep("^[A-G]10$", colnames(train.data)),
                                           grep("^[A-G]11$", colnames(train.data)))]

    for (j in 1:length(cols.train)) {
        tmp.mat <- expandFactors(x=train.data[, eval(cols.train[j])], v=eval(cols.train[j]))
        if (j == 1) {
            train.exploded <- tmp.mat
        } else {
            train.exploded <- cbind(train.exploded, tmp.mat)
        }
    }
    all.train   <- cbind(train.data, train.exploded)
    all.train   <- all.train[, -(which(colnames(all.train) %in% cols.train))]

    ## drop the custday key
    all.train$custday_key.u <- NULL

    ## remove the superfluous variables
    all.train    <- all.train[ , -grep("ABCDEFG", colnames(all.train)) ]
    all.train    <- all.train[ , -grep("^[A-G]$", colnames(all.train)) ]


    ## scale the cost difference variables
    cols    <- colnames(all.train)[grep("^d[A-G]",colnames(all.train))]
    for (j in 1:length(cols)) {
        all.train[, eval(cols[j])] <- scale(as.numeric(all.train[,eval(cols[j])]))
    }

    ## scale the cost difference variables
    cols    <- colnames(all.train)[grep("^n[A-G]",colnames(all.train))]
    for (j in 1:length(cols)) {
        all.train[, eval(cols[j])] <- scale(as.numeric(all.train[,eval(cols[j])]))
    }
    
    ## save the results
    panel.train[[train.sp]]$data  <- all.train
    panel.train[[train.sp]]$sp    <- train.sp
    panel.train[[train.sp]]$len   <- train.len

}


##------------------------------------------------------------------
## save the final test panels
##------------------------------------------------------------------
panel.list  <- panel.test
save(panel.list, file="X005_allstatePanelData_Test.Rdata")

## Write individual panels to separate .Rdata files
panel.names <- names(panel.list)
for (i in 1:length(panel.names)) {
    tmp.panel       <- panel.names[i]
    tmp.filename    <- paste("./panels/X005_allstatePanelData_Test.",tmp.panel,".Rdata",sep="")
    tmp.object      <- panel.list[[tmp.panel]]
    save(tmp.object, file=tmp.filename)
}
rm(panel.list)

##------------------------------------------------------------------
## save the final train panels
##------------------------------------------------------------------
panel.list  <- panel.train
save(panel.list, file="X005_allstatePanelData_Train.Rdata")

## Write individual panels to separate .Rdata files
panel.names <- names(panel.list)
for (i in 1:length(panel.names)) {
    tmp.panel       <- panel.names[i]
    tmp.filename    <- paste("./panels/X005_allstatePanelData_Train.",tmp.panel,".Rdata",sep="")
    tmp.object      <- panel.list[[tmp.panel]]
    save(tmp.object, file=tmp.filename)
}

## save a mock dataset to SP_01 (to preserve consistency with previous scripts)
save(tmp.object, file=paste("./panels/X005_allstatePanelData_Test.","SP_01",".Rdata",sep=""))
save(tmp.object, file=paste("./panels/X005_allstatePanelData_Train.","SP_01",".Rdata",sep=""))
