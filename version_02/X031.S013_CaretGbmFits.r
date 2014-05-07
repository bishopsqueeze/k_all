##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)
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
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Define the list of training data panels
##------------------------------------------------------------------

## load the training data
panel.files     <- dir("./panels")[(grep("X005_allstatePanelData_Train", dir("./panels")))]
panel.num       <- length(panel.files)

## load the test data
test.files     <- dir("./panels")[(grep("X005_allstatePanelData_Test", dir("./panels")))]

##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
for (i in 11:2) {
    
    ## get panel filenames
    tmp.filename    <- panel.files[i]
    tmp.testname    <- test.files[i]
    
    ## report the loaded panel
    cat("Loading ... ", tmp.filename, "\n")
    
    ##------------------------------------------------------------------
    ## load files (should generate an oject called tmp.object)
    ##------------------------------------------------------------------
    ## test data
    load(paste("./panels/",tmp.testname,sep=""))
    test.data   <- tmp.object$data
    test.len    <- tmp.object$len
    
    ## training data
    load(paste("./panels/",tmp.filename,sep=""))
    tmp.data    <- tmp.object$data
    tmp.len     <- tmp.object$len
    
    ## define the groups to test
    groups <- c("A","B","C","D","E","F","G")
    
    ##------------------------------------------------------------------
    ## Loop over each (assumed) independent grouping
    ##------------------------------------------------------------------
    #for (j in 1:length(groups)) {
    for (j in 7:7) {
        
        ## report status and clean the fit
        cat("Response Variable ... ", groups[j], "\n")
        tmp.fit <- NULL
        
        ## define the output filename
        tmp.panel    <- paste("SP_", ifelse(i < 10, paste("0",i,sep=""), i), sep="")
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".gbmCaretFit_AllSample_REPCV.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        tmp.lq     <- paste(groups[j],"0",sep="")  ## last-qutoed
        
        ##******************************************************************
        ## define columns to drop
        ##******************************************************************
        drop.cols   <- c(   ## remove non-predictors
                            c("customer_ID", "shopping_pt", "record_type"),
                            ## remove the current-step choices (contained elsewhere)
                            #LETTERS[1:7],
                            ## remove additional non-predictors
                            c("id_fl", "key", "last_fl", "custday_key.u"),
                            ## remove un-scaled versions of variables
                            #c("car_age", "car_age.bin", "age_oldest", "age_youngest"),
                            #c("duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            ## [???] not sure what to do with this
                            #c("location.r"),
                            ## remove cost.s b/c contained elsewhere
                            c("cost.s"),
                            ## remove the intermediate concatenated plans
                            #paste("ABCDEFG.",seq(0,i-1,1),sep=""),
                            ## remove the terminal concatenated plans
                            #paste("ABCDEFG.","T",sep=""),
                            ## remove terminal single choices (will add back the target later)
                            paste(groups,"T",sep=""),
                            ## indicators of static input variable changes
                            #colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            ## remove un-scaled cost variables
                            #colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            #colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))],
                            ## remove terminal, grouped variables
                            #c("AFT","BET","CDT"),
                            ## remove intermediate, grouped variables
                            #colnames(tmp.data)[grep("AF[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("AF10$", colnames(tmp.data))],
                            #colnames(tmp.data)[grep("CD[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("CD10$", colnames(tmp.data))],
                            #colnames(tmp.data)[grep("BE[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("BE10$", colnames(tmp.data))],
                            ## remove the day-change indicator (can be toggled at last shopping_pt)
                            c("day.u"),
                            ## remove the flag used for the lastquoted benchmark testing
                            c("lq.ne"))
        
        ##------------------------------------------------------------------
        ## exchange box-cox transformed data for the previous numeric columns ...
        ## ... but an issue b/c unscaled cost propagated in the split panel step
        ##------------------------------------------------------------------
        #drop.cols <- c(drop.cols, c("age_oldest.s", "age_youngest.s", "rmin.s",
        #                            "age_youngest.bc", "age_oldest.bc", "cost.bc", "car_age.bc", "rmin.bc",
        #                            "cost.bcs"))
        
        ##------------------------------------------------------------------
        ## retain only correlated factors for single-name fits
        ## [!!!] may want to experiment with these
        ##------------------------------------------------------------------
        #if (groups[j] == "A") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[BCDG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[BCDG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "B") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ACDFG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ACDFG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "C") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABEFG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABEFG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "D") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABEFG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABEFG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "E") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[CDFG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[CDFG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "F") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[BCDEG][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[BCDEG]10$", colnames(tmp.data))])
        #} else if (groups[j] == "G") {
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABCDEF][0-9]$", colnames(tmp.data))])
        #    drop.cols <- c(drop.cols, colnames(tmp.data)[grep("^[ABCDEF]10$", colnames(tmp.data))])
        #}
        
        ##------------------------------------------------------------------
        ## for SP_01 drop "n*" and "d*" variables b/c no accumulated history
        ##------------------------------------------------------------------
        #if (i == 1) {
        #    drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        #} else {
        #    drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        #}
        
        ##------------------------------------------------------------------
        ## attempt to remove independent groups & irrelevant groups associated with the current group
        ##------------------------------------------------------------------
        #drop.groups  <- groups[ -which(groups %in% groups[j]) ]
        #for (k in 1:length(drop.groups)) {
        #    drop.groups <- c(drop.groups, colnames(tmp.data)[grep(drop.groups[k], colnames(tmp.data))])
        #}
        
        ##------------------------------------------------------------------
        ## remove current single parameter
        ##------------------------------------------------------------------
        drop.groups  <- groups[ -which(groups %in% groups[j]) ]
        drop.groups  <- c(drop.groups, groups[j])
        
        ##------------------------------------------------------------------
        ## Drop the current group levels
        ##------------------------------------------------------------------
        #drop.cols   <- c(drop.cols, groups)
        drop.cols   <- c(drop.cols, drop.groups)
        
        ##------------------------------------------------------------------
        ## add back the reponse variable
        ##------------------------------------------------------------------
        drop.cols   <- drop.cols[ -which(drop.cols %in% eval(tmp.y)) ]
        
        ##------------------------------------------------------------------
        ## define the BASE regression dataframe
        ##------------------------------------------------------------------
        tmp.reg   <- droplevels(tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ])
        tmp.test  <- droplevels(test.data[ , -which(colnames(test.data) %in% drop.cols) ])
        
        ## split data into the response (Class) and variables (Descr)
        tmpClass  <- tmp.reg[ , tmp.y]
        tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]
        testDescr <- tmp.test
        
        ##------------------------------------------------------------------
        ## remove variables with exactly zero variance
        ##------------------------------------------------------------------
        zeroDescr <- colnames(tmpDescr)[(apply(tmpDescr, 2, sd) == 0)]
        tmpDescr  <-  tmpDescr[ , -which(colnames(tmpDescr) %in% zeroDescr)]
        testDescr <- testDescr[ , -which(colnames(testDescr) %in% zeroDescr)]
        
        ## sync testDescr names with tmpDescr names
        misMatchCols <- which( !(colnames(tmpDescr) %in% colnames(testDescr)) )
        if (length(misMatchCols) > 0) {
            tmpDescr  <- tmpDescr[ , -misMatchCols]
        }
        
        
        ## define a hold-out sample to use
        #numObs      <- nrow(tmpDescr)
        #
        #set.seed(88888888)
        #holdSmp     <- sample.int(nrow(tmpDescr), round(0.20*nrow(tmpDescr))) ## 10% hold-out
        #
        #holdClass   <- tmpClass[holdSmp]
        #holdDescr   <- tmpDescr[holdSmp, ]
        #tmpClass    <- tmpClass[-holdSmp]
        #tmpDescr    <- tmpDescr[-holdSmp, ]
        
        ##------------------------------------------------------------------
        ## Define the samples to be used since there is a danger that
        ## thinly populated classes might cause a sampling failure
        ##------------------------------------------------------------------
        
        ##------------------------------------------------------------------
        ## For the simplicity of doing a sweep of the parameter space, limit
        ## the number of total samples to 10,000 ... but isolate the sample
        ## using stratified sampling on the classes
        ##------------------------------------------------------------------
        #max.reg <- 10000
        #if ( length(tmpClass) > max.reg ) {
        #    reg.p   <- max.reg/length(tmpClass)
        #} else {
        #    reg.p   <- 1
        #}
        #reg.idx    <- createDataPartition(tmpClass, p=reg.p, list=TRUE)
        
        ##------------------------------------------------------------------
        ## create the smaller samples used for exploring the tuning parameters
        ##------------------------------------------------------------------
        #tmpClass  <- tmpClass[reg.idx[[1]]]
        #tmpDescr  <- tmpDescr[reg.idx[[1]], ]
        
        ##------------------------------------------------------------------
        ## create an index of multiple samples for use in the tuning parameter search
        ##------------------------------------------------------------------
        #smp.list    <- createDataPartition(tmpClass, p=0.80, list=TRUE, times=10)
        
        
        ##------------------------------------------------------------------
        ## A configuration parameters
        ##------------------------------------------------------------------
        if (i < 12) {
            gbm.d <- 9
            gbm.n <- 500
        }
        gbmGrid <- expand.grid(.interaction.depth = gbm.d, .n.trees = gbm.n, .shrinkage = 0.01)
        
        ##------------------------------------------------------------------
        ## set-up the fit parameters using the pre-selected (stratified) samples
        ##------------------------------------------------------------------
        
        if (TRUE) {
            
            #num.cv      <- 5
            #num.repeat  <- 5
            #num.total   <- num.cv * num.repeat
            
            #set.seed(123)
            #seeds <- vector(mode = "list", length = (num.total + 1))
            #for(k in 1:num.total) seeds[[k]] <- sample.int(1000, nrow(gbmGrid))
            #seeds[[num.total+1]] <- sample.int(1000, 1)
            
            ## test of repeated CV for G-class
            #fitControl <- trainControl(
            #                    method="repeatedcv",
            #                    number=num.cv,
            #                    repeats=num.repeat,
            #                    seeds=seeds)
            
            ## In cases where the model tuning values are known, train can be used
            ## to fit the model to the entire training dataset without any
            ## resampling or model tuning
            fitControl <- trainControl(method="none")
            
        }
        
        ##------------------------------------------------------------------
        ## Performing LGOCV to ensure sample completeness
        ##------------------------------------------------------------------
        ##system.time({
        ## perform a fit
        tmp.fit <- try(train(   x=tmpDescr,
                                y=tmpClass,
                                method="gbm",
                                trControl=fitControl,
                                verbose=FALSE,
                                tuneGrid=gbmGrid))
        ##})
        
        ##------------------------------------------------------------------
        ## handle fit errors
        ##------------------------------------------------------------------
        if (class(tmp.fit)[1] == "try-error") {
            cat("Error with fit ...", out.filename, "\n")
        } else {
            ## save the results
            cat("Saving fit to file ...", out.filename, "\n")
            #save(tmp.fit, tmpClass, tmpDescr, tmp.data, testDescr, test.data, holdClass, holdDescr, file=out.filename)
            save(tmp.fit, tmpClass, tmpDescr, tmp.data, testDescr, test.data, file=out.filename)
        }
    }
}














