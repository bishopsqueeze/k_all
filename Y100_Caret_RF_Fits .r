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
## Flags for fit type (enable only one at a time)
##------------------------------------------------------------------
DO_PARAMETER_SWEEP  <- FALSE
DO_HOLD_OUT_SAMPLE  <- FALSE
DO_FINAL_FIT        <- TRUE

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
panel.files     <- dir("./panels")[(grep("Y004_allstatePanelData_Train", dir("./panels")))]
panel.num       <- length(panel.files)

## load the test data
test.files     <- dir("./panels")[(grep("Y004_allstatePanelData_Test", dir("./panels")))]

##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
for (i in 2:11) {
    
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
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".rf_CaretFit_AllSample_REPCV.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        
        ## accomodate multi-choice targets (e.g., CG, BG, etc.)
        if (nchar(tmp.y) > 2) {
            for (p in 1:(nchar(tmp.y)-1)) {
                if (p == 1) {
                    tmp.target <- as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")])
                } else {
                    tmp.target <- cbind(tmp.target, as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")]))
                }
            }
            tmp.target <- as.factor(apply(tmp.target, 1, paste, sep="", collapse=""))
        }
        
        ##******************************************************************
        ## define columns to drop
        ##******************************************************************
        drop.cols   <- c(   ## remove non-predictors
                            c("customer_ID", "shopping_pt", "record_type"),
                            ## remove additional non-predictors
                            c("id_fl", "key", "last_fl"),
                            ## remove cost.s b/c contained elsewhere
                            c("cost.s"),
                            ## remove terminal single choices (will add back the target later)
                            paste(groups,"T",sep=""),
                            ## remove terminal
                            paste(LETTERS[1:7],"T",sep=""),
                            ## remove plans
                            colnames(tmp.data)[grep("^ABCDEFG.", colnames(tmp.data))],
                            ## remove all of the .u variables
                            colnames(tmp.data)[grep(".u$", colnames(tmp.data))],
                            ## remove the day-change indicator (can be toggled at last shopping_pt)
                            c("day.u"),
                            ## <X> For this experiment,
                            c("lq.ne"))
        
        ##------------------------------------------------------------------
        ## remove current single parameter
        ##------------------------------------------------------------------
        drop.groups  <- groups[ -which(groups %in% groups[j]) ]
        drop.groups  <- c(drop.groups, groups[j])
        
        ##------------------------------------------------------------------
        ## remove the d[A-G] parameters
        ##------------------------------------------------------------------
        drop.cols <- c(drop.cols, paste("d",LETTERS[1:7],sep=""))
       
        ##------------------------------------------------------------------
        ## Drop the current group levels
        ##------------------------------------------------------------------
        drop.cols   <- c(drop.cols, drop.groups)
        
        ##------------------------------------------------------------------
        ## add back the reponse variable
        ##------------------------------------------------------------------
        drop.cols   <- drop.cols[ -which(drop.cols %in% eval(tmp.y)) ]
        
        ##------------------------------------------------------------------
        ## define the BASE regression dataframe
        ##------------------------------------------------------------------
        tmp.reg   <- droplevels(tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ])
        
        ##------------------------------------------------------------------
        ## split data into the response (Class) and variables (Descr)
        ##------------------------------------------------------------------
        if (nchar(tmp.y) > 2) {
            tmpClass  <- tmp.target
            tmpDescr  <- tmp.reg
        } else {
            tmpClass  <- tmp.reg[ , tmp.y]
            tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]
        }
      
        ##******************************************************************
        ## preprocess the dataset
        ##******************************************************************

        ##------------------------------------------------------------------
        ## remove variables with exactly zero variance
        ##------------------------------------------------------------------
        #zeroDescr <- colnames(tmpDescr)[(apply(tmpDescr, 2, sd) == 0)]
        #if ( !is.na(zeroDescr) ) {
        #    tmpDescr  <- tmpDescr[ , -which(colnames(tmpDescr) %in% zeroDescr)]
        #}

        ##------------------------------------------------------------------
        ## remove variables with near-zero variance
        ##------------------------------------------------------------------
        #nzv       <- nearZeroVar(tmpDescr, freqCut=99/1)
        #tmpDescr  <- tmpDescr[ , -nzv]

        ##------------------------------------------------------------------
        ## check for highly-correlated variables
        ##------------------------------------------------------------------
        #corDescr        <- cor(tmpDescr)
        #highlyCorDescr  <- colnames(tmpDescr)[findCorrelation(corDescr, cutoff = 0.90)]
        #highlyCorDescr  <- highlyCorDescr[ -which(highlyCorDescr %in% highlyCorDescr[grep("^[A-G]", highlyCorDescr)])]
        #tmpDescr        <- tmpDescr[ , -which(colnames(tmpDescr) %in% highlyCorDescr)]

        ##------------------------------------------------------------------
        ## check for linearly-related variables
        ##------------------------------------------------------------------
        #comboDescr      <- findLinearCombos(tmpDescr)
        #comboVars       <- colnames(tmpDescr)[comboDescr$remove]
        #comboVars       <- comboVars[ -grep("[A-G][0-9]", comboVars) ]
        #tmpDescr        <- tmpDescr[, -comboDescr$remove]
        
        ##******************************************************************
        ## Define the samples to be used since there is a danger that
        ## thinly populated classes might cause a sampling failure
        ##******************************************************************
        
        ##------------------------------------------------------------------
        ## smaller dataset size for a parameter sweep
        ##------------------------------------------------------------------
        if (DO_PARAMETER_SWEEP) {
    
            ##------------------------------------------------------------------
            ## For the simplicity of doing a sweep of the parameter space, limit
            ## the number of total samples to 10,000 ... but isolate the sample
            ## using stratified sampling on the classes
            ##------------------------------------------------------------------
            max.reg <- 10000
            if ( length(tmpClass) > max.reg ) {
              reg.p   <- max.reg/length(tmpClass)
            } else {
              reg.p   <- 1
            }
            set.seed(88888888)
            reg.idx    <- createDataPartition(tmpClass, p=reg.p, list=TRUE)
            
            ##------------------------------------------------------------------
            ## create the smaller samples used for exploring the tuning parameters
            ##------------------------------------------------------------------
            holdClass   <- NULL
            holdDescr   <- NULL
            tmpClass    <- tmpClass[ reg.idx[[1]] ]
            tmpDescr    <- tmpDescr[ reg.idx[[1]], ]
            
            ##------------------------------------------------------------------
            ## define the tuning parameters
            ##------------------------------------------------------------------
            ##
            
            
            ## output file
            out.filename <- paste(tmp.panel,".","Group_",groups[j],".Caret_rf_Fit_Sweep.Rdata",sep="")

        ##------------------------------------------------------------------
        ## data for a hold-out sample
        ##------------------------------------------------------------------
        } else if (DO_HOLD_OUT_SAMPLE) {
            
            set.seed(88888888)
            numObs      <- nrow(tmpDescr)
            holdSmp     <- sample.int(nrow(tmpDescr), round(0.10*nrow(tmpDescr)))   ## 10% hold-out

            holdClass   <- droplevels(tmpClass[holdSmp])
            holdDescr   <- tmpDescr[holdSmp, ]
            tmpClass    <- droplevels(tmpClass[-holdSmp])
            tmpDescr    <- tmpDescr[-holdSmp, ]

            ##------------------------------------------------------------------
            ## define the tuning parameters
            ##------------------------------------------------------------------
            ##logit.iter <- c(1000)
            

            ## output file
            out.filename <- paste(tmp.panel,".","Group_",groups[j],".Caret_rf_Fit_HoldOut.Rdata",sep="")

        ##------------------------------------------------------------------
        ## data for the final fit
        ##------------------------------------------------------------------
        } else if (DO_FINAL_FIT) {
            
            holdClass   <- NULL
            holdDescr   <- NULL
            tmpClass    <- droplevels(tmpClass)
            tmpDescr    <- tmpDescr
            
            ##------------------------------------------------------------------
            ## define the tuning parameters
            ##------------------------------------------------------------------
            ##
            
            ## output file
            out.filename <- paste(tmp.panel,".","Group_",groups[j],".Caret_rf_Fit_Final.Rdata",sep="")
        }

        ##------------------------------------------------------------------
        ## define the fit grid
        ##------------------------------------------------------------------
        ##

        ##------------------------------------------------------------------
        ## define the test dataset
        ##------------------------------------------------------------------
        testDescr <- test.data[ , which(colnames(test.data) %in% colnames(tmpDescr)) ]
        
        ##------------------------------------------------------------------
        ## sync testDescr names with tmpDescr names
        ##------------------------------------------------------------------
        misMatchCols <- which( !(colnames(tmpDescr) %in% colnames(testDescr)) )
        if (length(misMatchCols) > 0) {
            tmpDescr  <- tmpDescr[ , -misMatchCols]
        }
        
        
        ##------------------------------------------------------------------
        ## <CRUDE> remove columns with factor mismatches
        ##------------------------------------------------------------------
        fac.col <- which(unlist(lapply(tmpDescr, class)) == "factor")
        num.fac <- length(fac.col)
        bad.col <- c()
        
        ## loop over all factors & search for bad columns
        for (k in 1:num.fac) {
            ## identify variables with mismatches
            if (nlevels(tmpDescr[, fac.col[k]]) != nlevels(testDescr[, fac.col[k]])) {
                cat("mismatch =", colnames(tmpDescr)[fac.col[k]], "\n")
                bad.col <- c(bad.col, fac.col[k])
            }
        }
        
        ## if there are bad columns, prune row from the training data that don't match
        if (!is.null(bad.col)) {
            for (k in 1:length(bad.col)) {
                
                ## identify the bad levels
                bad.levels  <- levels(tmpDescr[, bad.col[k]])[which( !(levels(tmpDescr[, bad.col[k]]) %in% levels(testDescr[, bad.col[k]])) )]
                
                ## echo drops
                cat("Pruning levels ... ", bad.levels, "from variable ...", colnames(tmpDescr)[bad.col[k]], "\n")
                
                ## remove the bad levels from the training data
                tmpBads     <- which(tmpDescr[,bad.col[k]] %in% bad.levels)
                tmpDescr    <- tmpDescr[ -tmpBads, ]
                tmpClass    <- tmpClass[ -tmpBads ]
                
                ## do the same for the holdout data
                if ( !is.null(holdDescr) ) {
                    holdBads    <- which(holdDescr[,bad.col[k]] %in% bad.levels)
                    holdDescr   <- holdDescr[ -holdBads, ]
                    holdClass   <- holdClass[ -holdBads ]
                    holdDescr   <- droplevels(holdDescr)
                }
                
            }
        }
        tmpDescr    <- droplevels(tmpDescr)
        testDescr   <- droplevels(testDescr)
        #tmpDescr    <- tmpDescr[ ,-bad.col]
        #testDescr   <- testDescr[ ,-bad.col]
        
        ##******************************************************************
        ## Do a k-fold cv (or) the final fit
        ##******************************************************************
        
        ##------------------------------------------------------------------
        ## k-fold cross-validation
        ##------------------------------------------------------------------
        if ( DO_PARAMETER_SWEEP ) {
            
            num.cv      <- 5
            num.repeat  <- 1
            num.total   <- num.cv * num.repeat
            
            ## test of repeated CV for G-class
            fitControl <- trainControl(
            method="repeatedcv",
            number=num.cv,
            repeats=num.repeat)
            
        ##------------------------------------------------------------------
        ## hold-out sample
        ##------------------------------------------------------------------
        } else if ( DO_HOLD_OUT_SAMPLE ) {
            
            num.cv      <- 5
            num.repeat  <- 1
            num.total   <- num.cv * num.repeat
            
            ## test of repeated CV for G-class
            fitControl <- trainControl(
            method="repeatedcv",
            number=num.cv,
            repeats=num.repeat)
            
        ##------------------------------------------------------------------
        ## final fit
        ##------------------------------------------------------------------
        } else {
            
            fitControl <- trainControl(method="none")
            
        }

        ##------------------------------------------------------------------
        ## Do the fit
        ##------------------------------------------------------------------
        if ( DO_PARAMETER_SWEEP | DO_HOLD_OUT_SAMPLE ) {
            tmp.fit <- try(train(   x=tmpDescr,
                                    y=tmpClass,
                                    method="rf",
                                    trControl=fitControl,
                                    verbose=FALSE,
                                    metric="Accuracy",
                                    tuneLength=10))
            
        } else {
            tmp.fit <- try(train(   x=tmpDescr,
                                    y=tmpClass,
                                    method="rf",
                                    trControl=fitControl,
                                    verbose=FALSE,
                                    metric="Accuracy",
                                    tuneGrid=expand.grid(mtry=30)))
        }
 
        
        ##------------------------------------------------------------------
        ## save the results w/error handling for bad fits
        ##------------------------------------------------------------------
        if (class(tmp.fit)[1] == "try-error") {
            cat("Error with fit ...", out.filename, "\n")
        } else {
            cat("Saving fit to file ...", out.filename, "\n")
            save(tmp.fit, tmpClass, tmpDescr, tmp.data, testDescr, test.data, holdClass, holdDescr, file=out.filename)
        }
        
    }
}










