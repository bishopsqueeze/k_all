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
panel.files     <- dir("./panels")[(grep("X005_allstatePanelData_Train", dir("./panels")))]
panel.num       <- length(panel.files)

##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
for (i in 11:2) {

    ## get panel filenames
    tmp.filename    <- panel.files[i]

    ## report the loaded panel
    cat("Loading ... ", tmp.filename, "\n")

    ## load file (should generate an oject called tmp.object)
    load(paste("./panels/",tmp.filename,sep=""))

    ## define the dataset
    tmp.data    <- tmp.object$data
    tmp.len     <- tmp.object$len

    ## define the groups to test
    groups <- c("A","B","C","D","E","F","G")
    
    ##------------------------------------------------------------------
    ## Loop over each (assumed) independent grouping
    ##------------------------------------------------------------------
    ##for (j in 1:length(groups)) {
    for (j in 7:7) {
    
        ## report status and clean the fit
        cat("Response Variable ... ", groups[j], "\n")
        tmp.fit <- NULL
    
        ## define the output filename
        tmp.panel    <- paste("SP_", ifelse(i < 10, paste("0",i,sep=""), i), sep="")
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".gbmCaretFit_TwoFactors_REPCV.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        
        ## accomodate multi-choice targets
        if (nchar(tmp.y) > 2) {
            for (p in 1:(nchar(tmp.y)-1)) {
                if (p == 1) {
                    tmp.target <- as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")])
                } else {
                    tmp.target <- cbind(tmp.target, as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")]))
                }
            }
            tmp.target <- apply(tmp.target, 1, paste, sep="", collapse="")
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
        tmpClass  <- tmp.reg[ , tmp.y]
        tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]
        
        
        ##******************************************************************
        ## Define the samples to be used since there is a danger that
        ## thinly populated classes might cause a sampling failure
        ##******************************************************************

        ##------------------------------------------------------------------
        ## For the simplicity of doing a sweep of the parameter space, limit
        ## the number of total samples to 10,000 ... but isolate the sample
        ## using stratified sampling on the classes
        ##------------------------------------------------------------------
        max.reg <- 100000
        if ( length(tmpClass) > max.reg ) {
            reg.p   <- max.reg/length(tmpClass)
        } else {
            reg.p   <- 1
        }
        set.seed(7777777)
        reg.idx    <- createDataPartition(tmpClass, p=reg.p, list=TRUE)

        ##------------------------------------------------------------------
        ## create the smaller samples used for exploring the tuning parameters
        ##------------------------------------------------------------------
        tmpClass  <- tmpClass[reg.idx[[1]]]
        tmpDescr  <- tmpDescr[reg.idx[[1]], ]

        ##------------------------------------------------------------------
        ## remove variables with exactly zero variance
        ##------------------------------------------------------------------
        zeroDescr <- colnames(tmpDescr)[(apply(tmpDescr, 2, sd) == 0)]
        tmpDescr  <- tmpDescr[ , -which(colnames(tmpDescr) %in% zeroDescr)]

        ##------------------------------------------------------------------
        ## remove variables with near-zero variance
        ##------------------------------------------------------------------
        nzv       <- nearZeroVar(tmpDescr)
        tmpDescr  <- tmpDescr[ , -nzv]

        ##------------------------------------------------------------------
        ## set-up the tuning parameters
        ##------------------------------------------------------------------
        if (i < 12) {
            gbmGrid    <- expand.grid(
            .interaction.depth = c(7, 9),
            #.interaction.depth = c(9),
            .n.trees = c(50, 150, 250, 350, 450),
            #.n.trees = c(250),
            .shrinkage = c(0.01))
        }

        ##------------------------------------------------------------------
        ## set-up the fit parameters using the pre-selected (stratified) samples
        ##------------------------------------------------------------------
        num.cv      <- 5
        num.repeat  <- 1
        num.total   <- num.cv * num.repeat
        
        ## define the seeds to be used in the fits
        set.seed(88888888)
        seeds                               <- vector(mode = "list", length = (num.total + 1))
        for(k in 1:num.total) seeds[[k]]    <- sample.int(1000, nrow(gbmGrid))
        seeds[[num.total+1]]                <- sample.int(1000, 1)
        
        ## define the fit parameters
        fitControl <- trainControl(
                            method="repeatedcv",
                            number=num.cv,
                            repeats=num.repeat,
                            seeds=seeds)
    
        ##------------------------------------------------------------------
        ## perform the cross-validation fit
        ##------------------------------------------------------------------
        tmp.fit <- try(train(   x=tmpDescr,
                                y=tmpClass,
                                method="gbm",
                                trControl=fitControl,
                                verbose=FALSE,
                                tuneGrid=gbmGrid))
        
        ##------------------------------------------------------------------
        ## handle fit errors
        ##------------------------------------------------------------------
        if (class(tmp.fit)[1] == "try-error") {
            cat("Error with fit ...", out.filename, "\n")
        } else {
            
            ## save the results
            cat("Saving fit to file ...", out.filename, "\n")
            save(tmp.fit, seeds, file=out.filename) ## tmp.pred, tmp.confusion
        }
    }
}


