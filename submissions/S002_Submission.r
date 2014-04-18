##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
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

##******************************************************************
## Step 1: Load the complete panel
##******************************************************************

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Load the full set of consolidated train/test observations
##------------------------------------------------------------------
load("003_allstateRawData.Rdata"); rm(all.copy, cost.test, cost.train, hist.test, hist.train)

##------------------------------------------------------------------
## Create a slim data frame
##------------------------------------------------------------------
pred.test   <- all.test[ , c("customer_ID","shopping_pt","record_type","key",LETTERS[1:7])]
pred.test[ , c("AF.pred", "BE.pred", "CD.pred", "G.pred") ] <- "99"


##******************************************************************
## Step 2: Load the raw materials for predictions
##******************************************************************

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored")

##------------------------------------------------------------------
## Load fit data
##------------------------------------------------------------------

fit.files   <- dir(, pattern="Rdata")

for (i in 1:length(fit.files)) {

   ## clear-out the fit
   tmp.fit  <- NULL
   
   ## get file info
   tmp.file <- fit.files[i]
   tmp.name <- gsub(".Rdata", ".fit", tmp.file)
   
   ## define the panle name
   tmp.sp       <- strsplit(tmp.file,"[.]")[[1]][1]
   tmp.group    <- strsplit(strsplit(tmp.file,"[.]")[[1]][2],"_")[[1]][2]
   
   ## load the fit into a list
   load(tmp.file)
   
   ## calculate the predicted group
   tmp.pred <-  predict(tmp.fit, newdata=testDescr, type="raw")
   
   #assign(tmp.name, tmp.fit)
   test.data[ , c(paste(tmp.group,".pred",sep="")) ]    <- tmp.pred
   
   ## map the results into the prediction data frame
   pred.test[  which(pred.test$key %in% test.data$key) , c(paste(tmp.group,".pred",sep="")) ]   <- as.character(tmp.pred)
   
   
}

##------------------------------------------------------------------
## Split the results into the proper columns
##------------------------------------------------------------------
for (i in 1:7) {
    
    if (LETTERS[i] == "A") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$AF.pred,1,1))
    } else if (LETTERS[i] == "B") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$BE.pred,1,1))
    } else if (LETTERS[i] == "C") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$CD.pred,1,1))
    } else if (LETTERS[i] == "D") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$CD.pred,2,2))
    } else if (LETTERS[i] == "E") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$BE.pred,2,2))
    } else if (LETTERS[i] == "F") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$AF.pred,2,2))
    } else if (LETTERS[i] == "G") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$G.pred,1,1))
    }
    
}


##******************************************************************
## Step 3: Consolidate the prediction(s)
##******************************************************************

## collapse individual predicions
pred.test$ABCDEFG.lq    <- apply(pred.test[, LETTERS[1:7]], 1, paste, collapse="")
pred.test$B_ONLY.pred   <- apply(pred.test[, c("A", "B.pred", "C", "D", "E", "F", "G")], 1, paste, collapse="")
pred.test$E_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E.pred", "F", "G")], 1, paste, collapse="")
pred.test$G_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F", "G.pred")], 1, paste, collapse="")
pred.test$ABCDEFG.pred  <- apply(pred.test[, paste(LETTERS[1:7],".pred",sep="")], 1, paste, collapse="")

## isolate the last quote for each customer
sub.idx                 <- tapply(pred.test$shopping_pt, pred.test$customer_ID, function(x){ which(x == max(x)) })
sub.key                 <- paste(names(sub.idx),sub.idx,sep="_")

## create the subsetted data
pred.sub                <- pred.test[ which(pred.test$key %in% sub.key), ]


##******************************************************************
## Step 4: Construct submissions
##******************************************************************

lastquoted.sub  <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$ABCDEFG.lq) )

gbm_bonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$B_ONLY.pred) )

gbm_eonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$E_ONLY.pred) )

gbm_gonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$G_ONLY.pred) )

gbm_all.sub     <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$ABCDEFG.pred) )


##******************************************************************
## Step 5: Write submissions to file
##******************************************************************
write.csv(lastquoted.sub, file="S002_lastquoted.csv", row.names=FALSE)
write.csv(gbm_bonly.sub,  file="S002_gbm_bonly.csv", row.names=FALSE)
write.csv(gbm_eonly.sub,  file="S002_gbm_eonly.csv", row.names=FALSE)
write.csv(gbm_gonly.sub,  file="S002_gbm_gonly.csv", row.names=FALSE)
write.csv(gbm_all.sub,    file="S002_gbm_all.csv", row.names=FALSE)




##******************************************************************
## Results
##******************************************************************

## G-only       == 0.54021
## All          == 0.53967
## Last quoted  == 0.53793
## B-only       == 0.53739
## E-only       == 0.53733

## Next steps would be to check the CD (combined) results ... and then both CD & G (if CD makes an improvement)




