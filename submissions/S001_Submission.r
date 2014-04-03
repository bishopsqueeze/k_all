##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Create the raw materials for a panel dataset to be used in a
##     regression model for the insurance purchase targets
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(glmnet)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(MASS)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())


##------------------------------------------------------------------
## <function> - a function to perform a type switch on a data.frame
##------------------------------------------------------------------
convert.magic   <- function(obj, col, type) {
    idx <- which(colnames(obj) %in% col)
    for (i in 1:length(idx)) {
        FUN <- switch(type[i], character = as.character, numeric = as.numeric, factor = as.factor)
        obj[, idx[i]]   <- FUN(obj[, idx[i]])
    }
    return(obj)
}


##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/fits_20140309")

##------------------------------------------------------------------
## Load fit data
##------------------------------------------------------------------
fit.files   <- dir(, pattern="Rdata")
reg.list    <- list


for (i in 1:length(fit.files)) {

   ## clear-out the fit
   tmp.fit  <- NULL
   
   ## get file info
   tmp.file <- fit.files[i]
   tmp.name <- paste(substr(tmp.file, 1, 4), ".fit", sep="")
   
   ## load the fit into a list
   load(tmp.file)
   assign(tmp.name, tmp.fit)
   
}


##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Load data to predict -- te.pred holds the data
##------------------------------------------------------------------
load("004_allstateRawData_Test.Rdata")

##------------------------------------------------------------------
## Re-format some of the columns
##------------------------------------------------------------------

## pick-off letter columns and re-format as factors
tmp.let <- grep("[0-9]", colnames(te.pred))
te.pred <- convert.magic(te.pred, colnames(te.pred)[tmp.let], rep("factor",length(tmp.let)))

## convert remaining factors
te.pred <- convert.magic(te.pred, c("risk_factor", "car_value", "C_previous"), c("factor", "factor", "factor"))

##------------------------------------------------------------------
## First iteration -- loop to be sure
##------------------------------------------------------------------

## create a shell matrix for the fits
final.pred                  <- te.pred[ , c("customer_ID", LETTERS[1:7])]
final.pred[, LETTERS[1:7]]  <- 0


## isolate the unique timesteps
uniq.time    <- unique(te.pred$shopping_pt)

for (i in 1:length(uniq.time))  {
    
    tmp.time    <- uniq.time[i]
    tmp.data    <- te.pred[ which(te.pred$shopping_pt == tmp.time) , ]
    
    ## loop over the set of choices and predict
    for (j in 1:7) {

        tmp.fitname <- paste(LETTERS[j],"_",ifelse(tmp.time < 10, paste("0",tmp.time,sep=""),tmp.time),".fit",sep="")
        tmp.fit     <- get(tmp.fitname)
        tmp.pred    <- predict(tmp.fit, newdata=tmp.data, type="class")
        
        ## load the matrix
        row.idx                             <- which(final.pred[ , c("customer_ID")] %in% tmp.data$customer_ID )
        #        col.idx                             <- rep(which(colnames(final.pred) %in% j), length(row.idx))
        final.pred[row.idx, LETTERS[j]]              <- as.vector(tmp.pred)
        
    }
}


##------------------------------------------------------------------
## Extract the last quoted plan benchmark
##------------------------------------------------------------------
lastquoted.pred  <- as.data.frame(te.pred[ , c(c("customer_ID"), LETTERS[1:7])])


##------------------------------------------------------------------
## Construct submissions
##------------------------------------------------------------------
lastquoted.sub  <- data.frame(
                    customer_ID = as.integer(lastquoted.pred$customer_ID),
                    plan = as.character(apply(lastquoted.pred[, LETTERS[1:7]], 1, paste, collapse="")) )

final.sub       <- data.frame(
                    customer_ID = as.integer(final.pred$customer_ID),
                    plan = as.character(apply(final.pred[, LETTERS[1:7]], 1, paste, collapse="")) )


##------------------------------------------------------------------
## Write submissions to file
##------------------------------------------------------------------
write.csv(lastquoted.sub, file="S000_lastquoted.csv", row.names=FALSE)
write.csv(final.sub, file="S001_initialMultinom.csv", row.names=FALSE)


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
##save(fit.list, file="S001_allstateRawData.Rdata")















