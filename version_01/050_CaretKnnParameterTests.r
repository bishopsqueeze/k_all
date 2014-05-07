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
panel.files     <- dir("./panels")[(grep("Train", dir("./panels")))]
panel.num       <- length(panel.files)

##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
for (i in 2:2) {

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
    groups <- c("AF","BE","CD","G")
    
    ##------------------------------------------------------------------
    ## Loop over each (assumed) independent grouping
    ##------------------------------------------------------------------
    ##for (j in 1:length(groups)) {
    for (j in 4:4) {
    
        ## report status and clean the fit
        cat("Response Variable ... ", groups[j], "\n")
        tmp.fit <- NULL
    
        ## define the output filename
        tmp.panel    <- paste("SP_", ifelse(i < 10, paste("0",i,sep=""), i), sep="")
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".knnCaretFit_LGOCV.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        tmp.lq     <- paste(groups[j],"0",sep="")  ## last-qutoed

        ##------------------------------------------------------------------
        ## define columns to drop
        ##------------------------------------------------------------------
        drop.cols   <- c(   c("customer_ID", "shopping_pt", "record_type"),
                            LETTERS[1:7],
                            c("id_fl", "key", "last_fl", "custday_key.u"),
                            c("car_age", "car_age.bin", "age_oldest", "age_youngest"),
                            c("duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            c("location.r"),
                            c("cost.s"), ## b/c contained in cost.s0
                            paste("ABCDEFG.",seq(0,i-1,1),sep=""),
                            paste("ABCDEFG.","T",sep=""),
                            paste(groups,"T",sep=""),
                            colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))])

        ##------------------------------------------------------------------
        ## for SP_01 drop "n*" and "d*" variables b/c no accumulated history
        ##------------------------------------------------------------------
        if (i == 1) {
            drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        } else {
            drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        }
        
        ##------------------------------------------------------------------
        ## attempt to remove independent groups & irrelevant groups associated with the current group
        ##------------------------------------------------------------------
        drop.groups  <- groups[ -which(groups %in% groups[j]) ]
        for (k in 1:length(drop.groups)) {
            drop.groups <- c(drop.groups, colnames(tmp.data)[grep(drop.groups[k], colnames(tmp.data))])
        }
        drop.groups  <- c(drop.groups, groups[j])
        

        ##------------------------------------------------------------------
        ## Drop the current group levels
        ##------------------------------------------------------------------
        # drop.cols   <- c(drop.cols, groups)
        drop.cols   <- c(drop.cols, drop.groups)
        
        ##------------------------------------------------------------------
        ## don't drop the reponse variable
        ##------------------------------------------------------------------
        drop.cols   <- drop.cols[ -which(drop.cols %in% eval(tmp.y)) ]

        ##------------------------------------------------------------------
        ## define the BASE regression dataframe
        ##------------------------------------------------------------------
        tmp.reg   <- droplevels(tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ])


        ## split data into the response (Class) and variables (Descr)
        tmpClass  <- tmp.reg[ , tmp.y]
        tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]

        ##------------------------------------------------------------------
        ## Define the samples to be used since there is a danger that
        ## thinly populated classes might cause a sampling failure
        ##------------------------------------------------------------------

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
        
        ## force an identical seed
        set.seed(1234)
        reg.idx    <- createDataPartition(tmpClass, p=reg.p, list=TRUE)

        ##------------------------------------------------------------------
        ## create the smaller samples used for exploring the tuning parameters
        ##------------------------------------------------------------------
        tmpClass  <- tmpClass[reg.idx[[1]]]
        tmpDescr  <- tmpDescr[reg.idx[[1]], ]

        ##------------------------------------------------------------------
        ## KNN-specific :: convert factors to numeric to enable scaling
        ##------------------------------------------------------------------
        colsToConvert   <- names(which(unlist(lapply(tmpDescr, class)) == "factor"))
        tmpDescr        <- convert.magic(tmpDescr, colsToConvert, rep("numeric",length(colsToConvert)))
        
        ##------------------------------------------------------------------
        ## create an index of multiple samples for use in the tuning parameter search
        ##------------------------------------------------------------------
        set.seed(4321)
        smp.list    <- createDataPartition(tmpClass, p=0.80, list=TRUE, times=10)

        ##------------------------------------------------------------------
        ## set-up the fit parameters using the pre-selected (stratified) samples
        ##------------------------------------------------------------------
        fitControl <- trainControl(
                        method="LGOCV",
                        #returnData=TRUE,
                        #returnResamp="all",
                        #savePredictions=TRUE,
                        #verboseIter=TRUE,
                        number=10,
                        index=smp.list)

        ##------------------------------------------------------------------
        ## some test configuration parameters
        ##------------------------------------------------------------------
        #if (i < 12) {
        #    knnGrid    <- expand.grid(.k = 1:10)
        #}
    
        ##------------------------------------------------------------------
        ## Notes:
        ## - The CDN variables to the fit caused an error ... prob due to a
        ##   zero observations for rare classes being samples
        ##------------------------------------------------------------------
        system.time({
        ## perform a fit
        tmp.fit <- try(train(   x=tmpDescr[,c("cost.s0","state.num","G0","G1")],
                                y=tmpClass,
                                method="knn",
                                preProcess=c("center", "scale"),
                                trControl=fitControl,
                                tuneLength=20))
        })

        ##------------------------------------------------------------------
        ## handle fit errors
        ##------------------------------------------------------------------
        if (class(tmp.fit)[1] == "try-error") {
            cat("Error with fit ...", out.filename, "\n")
        } else {
            ## compute predicitons on the hold-out data
            #tmp.pred        <- predict(tmp.fit, newdata=testDescr)
            #tmp.confusion   <- confusionMatrix(tmp.pred, testClass)
            ## save the results
            cat("Saving fit to file ...", out.filename, "\n")
#save(tmp.fit, file=out.filename) ## tmp.pred, tmp.confusion
        }

    }
}



