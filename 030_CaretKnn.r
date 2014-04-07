##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
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


##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
for (i in 10:10) {

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
    ## Loop over each independent grouping
    ##------------------------------------------------------------------
    ## loop over each LETTER (A, B, ... , G)
    
    
    
    
    
    groups <- c("AF","BE","CD","G")
    for (j in 4:4) {
    ##for (j in 1:length(groups)) {
    
        ## report the variable
        cat("Response Variable ... ", groups[j], "\n")
    
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
        }

        ## don't drop the reponse variable
        drop.cols   <- drop.cols[ -which(drop.cols %in% eval(tmp.y)) ]

        ## define the regression dataframe
        tmp.reg   <- droplevels(tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ])

        tmpClass  <- tmp.reg[ , tmp.y]
        tmpDescr  <- tmp.reg[ , -which(colnames(tmp.reg) %in% tmp.y)]



        inTrain   <- sample(seq(along = tmpClass), length(tmpClass)/2)

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

## fit
system.time({
            gbmFit1 <- train(   x=trainDescr,
                                y=trainClass,
                                method="gbm",
                                preProcess=NULL,
                                trControl=fitControl,
                                verbose=FALSE)
})





    }


}



createDataPartition(y,
times = 1,
p = 0.5,
list = TRUE,
groups = min(5, length(y)))



data(iris)
model <- train(Species~., iris,
                    method='glmnet',
                    tuneGrid=expand.grid(
                    .alpha=0:1,
                    .lambda=0:30/10))

plot(model)
coef(model$finalModel, s=model$bestTune$.lambda)
 This code will fit both a lasso model (alpha=1) and a ridge regression model (alpha=0). You can also pick an alpha somewhere between the 2 for a mix of lasso and ridge regression. This is called the elastic net.



