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
### [7] confirm that the format is generally consistent with what
###     is described in the predictive analytics paper
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    
    ##******************************************************************
    ## load the test data
    ##******************************************************************
    test.data     <- test.list[[i]]$data
    test.len      <- test.list[[i]]$len
    test.sp       <- test.list[[i]]$sp
    
    ##------------------------------------------------------------------
    ## Create an set of interaction terms amongst the last observations
    ##------------------------------------------------------------------
    #cols.0  <- colnames(test.data)[grep("^[A-G][0]$", colnames(test.data))]
    #for (a in 1:(length(cols.0)-1)) {
    #    for (b in (a+1):length(cols.0)) {
    #        tmp.col <- paste(cols.0[a],"x",cols.0[b],sep="")
    #        tmp.fac <- paste(test.data[,cols.0[a]], test.data[,cols.0[b]], sep="")
    #
    #        test.data[,tmp.col] <- as.factor(tmp.fac)
    #    }
    #}

    ## expand factors into a set of binary variables
    cols.test     <- colnames(test.data)[c(grep("^[A-G][0-9]$", colnames(test.data)),
                                           grep("^[A-G]10$", colnames(test.data)),
                                           grep("^[A-G]11$", colnames(test.data)))] ## grep("[A-G][0]x[A-G][0]",colnames(test.data)))]
    
    ## explode factors
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
    
    ## save the results
    panel.test[[test.sp]]$data  <- all.test
    panel.test[[test.sp]]$sp    <- test.sp
    panel.test[[test.sp]]$len   <- test.len
    
    ##******************************************************************
    ## load the training data
    ##******************************************************************
    train.data    <- train.list[[i]]$data
    train.len     <- train.list[[i]]$len
    train.sp      <- train.list[[i]]$sp

    ##------------------------------------------------------------------
    ## Create an set of interaction terms amongst the last observations
    ##------------------------------------------------------------------
    #cols.0  <- colnames(train.data)[grep("^[A-G][0]$", colnames(train.data))]
    #for (a in 1:(length(cols.0)-1)) {
    #    for (b in (a+1):length(cols.0)) {
    #        tmp.col <- paste(cols.0[a],"x",cols.0[b],sep="")
    #        tmp.fac <- paste(train.data[,cols.0[a]], train.data[,cols.0[b]], sep="")
    #
    #        train.data[,tmp.col] <- as.factor(tmp.fac)
    #    }
    #}

    ## expand factors into a set of binary variables
    cols.train     <- colnames(train.data)[c(grep("^[A-G][0-9]$", colnames(train.data)),
                                           grep("^[A-G]10$", colnames(train.data)),
                                           grep("^[A-G]11$", colnames(train.data)))]    ##grep("[A-G][0]x[A-G][0]",colnames(train.data))]

    ## explode factors
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

    ## <X> add targets for testing lq vs. non-lq
    all.train$lq.ne <- 1*(as.character(all.train$ABCDEFG.T) != as.character(all.train$ABCDEFG.0))
    
    ## remove the superfluous variables
    all.train    <- all.train[ , -grep("ABCDEFG", colnames(all.train)) ]
    all.train    <- all.train[ , -grep("^[A-G]$", colnames(all.train)) ]
    
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
