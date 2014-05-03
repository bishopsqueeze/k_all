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
for (i in 11:3) {

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
    #groups <- c("A","B","C","D","E","F","G")
    groups <- c("BG")
    
    ##------------------------------------------------------------------
    ## Loop over each (assumed) independent grouping
    ##------------------------------------------------------------------
    ##for (j in 1:length(groups)) {
    for (j in 1:1) {
    
        ## report status and clean the fit
        cat("Response Variable ... ", groups[j], "\n")
        tmp.fit <- NULL
    
        ## define the output filename
        tmp.panel    <- paste("SP_", ifelse(i < 10, paste("0",i,sep=""), i), sep="")
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".gbmCaretFit_TwoFactors_REPCV.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        #tmp.lq     <- paste(groups[j],"0",sep="")  ## last-qutoed
        
        ## accomodate multiclass targets
        if (nchar(tmp.y) > 2) {
            for (p in 1:(nchar(tmp.y)-1)) {
                if (p == 1) {
                    tmp.target <- as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")])
                } else {
                    tmp.target <- cbind(tmp.target, as.character(tmp.data[, paste(substr(tmp.y,p,p),"T",sep="")]))
                }
            }
        }
        tmp.target <- apply(tmp.target, 1, paste, sep="", collapse="")


        ##******************************************************************
        ## define columns to drop
        ##******************************************************************
        drop.cols   <- c(   ## remove non-predictors
                            c("customer_ID", "shopping_pt", "record_type"),
                            ## remove the current-step choices (contained elsewhere)
                            ##LETTERS[1:7],
                            ## remove additional non-predictors
                            c("id_fl", "key", "last_fl"),
                            ## remove un-scaled versions of variables
                            #c("car_age", "car_age.bin", "age_oldest", "age_youngest"),
                            #c("duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            ## [???] not sure what to do with this
                            c("location.r"),
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
                            #colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))],
                            ## remove terminal, grouped variables
                            #c("AFT","BET","CDT"),
                            ## remove intermediate, grouped variables
                            #colnames(tmp.data)[grep("AF[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("AF10$", colnames(tmp.data))],
                            #colnames(tmp.data)[grep("CD[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("CD10$", colnames(tmp.data))],
                            #colnames(tmp.data)[grep("BE[0-9]$", colnames(tmp.data))], colnames(tmp.data)[grep("BE10$", colnames(tmp.data))],
                            ## remove the day-change indicator (can be toggled at last shopping_pt)
                            c("day.u"),
        ## <X> For this experiment
        paste(LETTERS[1:7],"T",sep=""),
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

        ##------------------------------------------------------------------
        ## split data into the response (Class) and variables (Descr)
        ##------------------------------------------------------------------
        #tmpClass  <- tmp.reg[ , tmp.y]
        #tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]
tmpClass  <- as.factor(tmp.target)
tmpDescr  <- tmp.reg

## identify and crop classes that occur < 10% of the time

        ##------------------------------------------------------------------
        ## remove variables with exactly zero variance
        ##------------------------------------------------------------------
        zeroDescr <- colnames(tmpDescr)[(apply(tmpDescr, 2, sd) == 0)]
        tmpDescr  <- tmpDescr[ , -which(colnames(tmpDescr) %in% zeroDescr)]
        
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
        ## create an index of multiple samples for use in the tuning parameter search
        ##------------------------------------------------------------------
        #set.seed(4321)
        #smp.list    <- createDataPartition(tmpClass, p=0.80, list=TRUE, times=10)

        ##------------------------------------------------------------------
        ## set-up the tuning parameters
        ##------------------------------------------------------------------
        if (i < 12) {
            gbmGrid    <- expand.grid(
            .interaction.depth = c(7),
            #.interaction.depth = c(9),
            #.n.trees = c(25, 50, 100, 150, 200, 250, 300, 350, 400, 500),
            .n.trees = c(250),
            .shrinkage = c(0.01))
        }

        ##------------------------------------------------------------------
        ## set-up the fit parameters using the pre-selected (stratified) samples
        ##------------------------------------------------------------------
        num.cv      <- 10
        num.repeat  <- 1
        num.total   <- num.cv * num.repeat
        
        ## define the seeds to be used in the fits
        set.seed(88888888)
        seeds <- vector(mode = "list", length = (num.total + 1))
        for(k in 1:num.total) seeds[[k]] <- sample.int(1000, nrow(gbmGrid))
        seeds[[num.total+1]] <- sample.int(1000, 1)
        
        ## test of repeated CV for G-class
        fitControl <- trainControl(
                            method="repeatedcv",
                            number=num.cv,
                            repeats=num.repeat,
                            seeds=seeds)
    
        ##------------------------------------------------------------------
        ## Notes:
        ## - The CDN variables to the fit caused an error ... prob due to a
        ##   zero observations for rare classes being samples
        ##------------------------------------------------------------------
        #system.time({
        ## perform a fit
        tmp.fit <- try(train(   x=tmpDescr,
                                y=tmpClass,
                                method="gbm",
                                trControl=fitControl,
                                verbose=FALSE,
                                tuneGrid=gbmGrid))
        #})
        
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


