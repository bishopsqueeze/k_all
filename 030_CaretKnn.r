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
library(glmnet)
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


##------------------------------------------------------------------
## Loop over each shopping_pt relevant to the test {1 ... 11}
##------------------------------------------------------------------
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

    ##------------------------------------------------------------------
    ## Loop over each independent grouping
    ##------------------------------------------------------------------
    ## loop over each LETTER (A, B, ... , G)
    
    
    
    
    
    groups <- LETTERS[1:7]
    for (j in 7:7) {
    ##for (j in 1:length(groups)) {
    
        ## report the variable
        cat("Response Variable ... ", groups[j], "\n")
    
        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(groups[j],"T",sep="")
        tmp.lq     <- paste(groups[j],"0",sep="")  ## last-qutoed

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



        set.seed(123)
        seeds <- vector(mode = "list", length = 51)
        for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)

        ## For the last model:
        seeds[[51]] <- sample.int(1000, 1)

        ctrl <- trainControl(   method = "repeatedcv",
                                number = 10,    # k-fold
                                repeats = 5,
                                savePredictions = FALSE,
                                classProbs = FALSE,
                                seeds = seeds,
                                allowParallel = TRUE)

        set.seed(1)

        knnFit <- train(Species ~ ., data = iris,
                        method = "knn",
                        tuneLength = 12,
                        preProcess = c("center", "scale")
                        trControl = ctrl)

        knnPred <- predict(knnFit, type="raw")
        
        
        extractPrediction(bothModels, testX = iris[1:10, -5])
        
        confusionMatrix(data, reference, dnn = c("Prediction", "Reference"))




    }

#    TrainData       <- tmp.data[ , c("GT")]
#    TrainClasses    <- iris[,5]

#knnFit1 <- train(TrainData, TrainClasses,
#method = "knn",
#preProcess = c("center", "scale"),
#tuneLength = 10,
#trControl = trainControl(method = "cv"))


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



