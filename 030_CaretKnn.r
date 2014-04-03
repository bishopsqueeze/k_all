##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(nnet)
library(MASS)
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

##------------------------------------------------------------------
## Define the list of training data panels
##------------------------------------------------------------------
panel.files     <- dir("./panels")[(grep("Train", dir("./panels")))]
panel.num       <- length(panel.files)

##------------------------------------------------------------------
## Set-up
##------------------------------------------------------------------
set.seed(123)
fit.list    <- list()   ## list for output


for (i in 5:5) {

    ## get panel filenames
    tmp.filename    <- panel.files[i]

    ## report the loaded panel
    cat("Loading ... ", tmp.filename, "\n")

    ## load file (should generate an oject called tmp.object)
    load(paste("./panels/",tmp.filename,sep=""))

    ## define the dataset
    tmp.data    <- tmp.object$data
    tmp.len     <- tmp.object$len


    ## loop over each LETTER (A, B, ... , G)
    for (j in 7:7) {
    
        ## report the loaded panel
        cat("Dependent Variable ... ", LETTERS[j], "\n")        ## clear some variables
    
        ## clear data
        fit.raw     <- NULL
        fit.step    <- NULL
    
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(LETTERS[j],"T",sep="")
        tmp.lq     <- paste(LETTERS[j],"0",sep="")  ## last-qutoed

        ## define columns to drop
        drop.cols   <- c(   c("customer_ID", "shopping_pt", "record_type"),
                            c("cost", "id_fl", "last_fl", "time.nrm", "time.num"),
                            c("car_age", "age_oldest", "age_youngest"),
                            c("duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            c("location.r"),
                            LETTERS[1:7],
                            paste(LETTERS[1:7],"T",sep=""),
                            colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))])

        ## for SP_01 drop "n*" and "d*" variables b/c no accumulated history
        if (i == 1) {
            drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        }

        ## don't drop the reponse variable
        drop.cols   <- drop.cols[ -which(drop.cols %in% tmp.y) ]


## define the regression dataframe
tmp.reg   <- tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ]





    }

#    TrainData       <- tmp.data[ , c("GT")]
#    TrainClasses    <- iris[,5]

#knnFit1 <- train(TrainData, TrainClasses,
#method = "knn",
#preProcess = c("center", "scale"),
#tuneLength = 10,
#trControl = trainControl(method = "cv"))


}


