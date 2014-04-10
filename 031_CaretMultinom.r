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
for (i in 9:9) {

    ## get panel filenames
    tmp.filename    <- panel.files[i]

    ## report the loaded panel
    cat("Loading ... ", tmp.filename, "\n")

    ## load file (should generate an oject called tmp.object)
    load(paste("./panels/",tmp.filename,sep=""))

    ## define the dataset
    tmp.data    <- tmp.object$data
    tmp.len     <- tmp.object$len

    ##------------------------------------------------------------------
    ## Loop over each (assumed) independent grouping
    ##------------------------------------------------------------------
    groups <- c("AF","BE","CD","G")
    
    #for (j in 1:length(groups)) {
    for (j in 3:3) {
    
        ## report status and clean the fit
        cat("Response Variable ... ", groups[j], "\n")
        tmp.fit <- NULL
    
        ## define the output filename
        tmp.panel    <- paste("SP_", ifelse(i < 10, paste("0",i,sep=""), i), sep="")
        out.filename <- paste(tmp.panel,".","Group_",groups[j],".gbmCaretFit.Rdata",sep="")
        
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        tmp.lq     <- paste(groups[j],"0",sep="")  ## last-qutoed

        ## define columns to drop
        drop.cols   <- c(   c("customer_ID", "shopping_pt", "record_type"),
                            LETTERS[1:7],
                            c("id_fl", "key", "last_fl", "custday_key.u"),
                            c("car_age", "age_oldest", "age_youngest"),
                            c("duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            c("location.r"),
                            c("cost.s"), ## b/c contained in cost.s0
                            paste("ABCDEFG.",seq(0,i-1,1),sep=""),
                            paste("ABCDEFG.","T",sep=""),
                            paste(groups,"T",sep=""),
                            colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))])

        ## for SP_01 drop "n*" and "d*" variables b/c no accumulated history
        if (i == 1) {
            drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        } else {
            drop.cols <- c(drop.cols, paste("d",LETTERS[1:7],sep=""))
        }

## simplify dataset
drop.cols <- c( drop.cols,
                colnames(tmp.data)[grep("^AF", colnames(tmp.data))],
                colnames(tmp.data)[grep("^BE", colnames(tmp.data))],
                colnames(tmp.data)[grep("^CD", colnames(tmp.data))],
                colnames(tmp.data)[grep("^G",  colnames(tmp.data))]
)


        ## Drop the current group levels
        drop.cols   <- c(drop.cols, groups)
        
        ## don't drop the reponse variable
        drop.cols   <- drop.cols[ -which(drop.cols %in% eval(tmp.y)) ]

        ## define the regression dataframe
        tmp.reg   <- droplevels(tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ])

        ## split data into the response (Class) and variables (Descr)
        tmpClass  <- tmp.reg[ , tmp.y]
        tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]

        ## define a train/test sampling split
        set.seed(123)
        inTrain   <- sample(seq(along = tmpClass), round(0.70*length(tmpClass)))

        ## create the training/test datasets
        trainDescr <- tmpDescr[ inTrain, ]
        testDescr  <- tmpDescr[-inTrain, ]
        trainClass <- tmpClass[ inTrain ]
        testClass  <- tmpClass[-inTrain ]

        ## define a trainControl object
        fitControl <- trainControl(
                    method="repeatedcv",
                    number=10,
                    repeats=3,
                    returnResamp="all")

    
system.time({
        ## perform a fit
        tmp.fit <- train(       x=trainDescr[,c("cost.s0")],
                                y=trainClass,
                                method="multinom",
                                ## try different methods
                                trControl=trainControl(method="boot"))
                                ## multinom specific parameters
                                ##decay=0,
                                ##maxit=1000) ##tuneGrid=gbmGrid


        ## compute predicitons on the hold-out data
        #tmp.pred        <- predict(tmp.fit, newdata=testDescr)
        #tmp.confusion   <- confusionMatrix(tmp.pred, testClass)
})
        
    ## save the results
    cat("Saving fit to file ...", out.filename, "\n")
    # save(tmp.fit, file=out.filename) ## tmp.pred, tmp.confusion

    }
}



