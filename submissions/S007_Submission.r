##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(gbm)
library(plyr)
library(caret)

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
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Load the full set of consolidated train/test observations
##------------------------------------------------------------------
load("003_allstateRawData.Rdata"); rm(all.copy, cost.test, cost.train, hist.test, hist.train)

##------------------------------------------------------------------
## Create a slim data frame
##------------------------------------------------------------------
pred.test   <- all.test[ , c("customer_ID","shopping_pt","record_type","key",LETTERS[1:7])]


##******************************************************************
## Step 2: Load the raw materials for predictions
##******************************************************************

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_S007")

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


##******************************************************************
## Step 3: Consolidate the prediction(s)
##******************************************************************

##------------------------------------------------------------------
## last-quoted benchmark
##------------------------------------------------------------------
pred.test$ABCDEFG.lq    <- apply(pred.test[, LETTERS[1:7]], 1, paste, collapse="")

##------------------------------------------------------------------
## single-name predictions
##------------------------------------------------------------------
#pred.test$A_ONLY.pred   <- apply(pred.test[, c("A.pred", "B", "C", "D", "E", "F", "G")], 1, paste, collapse="")
#pred.test$B_ONLY.pred   <- apply(pred.test[, c("A", "B.pred", "C", "D", "E", "F", "G")], 1, paste, collapse="")
#pred.test$C_ONLY.pred   <- apply(pred.test[, c("A", "B", "C.pred", "D", "E", "F", "G")], 1, paste, collapse="")
#pred.test$D_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D.pred", "E", "F", "G")], 1, paste, collapse="")
#pred.test$E_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E.pred", "F", "G")], 1, paste, collapse="")
#pred.test$F_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F.pred", "G")], 1, paste, collapse="")
pred.test$G_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F", "G.pred")], 1, paste, collapse="")

##------------------------------------------------------------------
## gbm "optimal" predictions include (depending on the shopping_pt):
##------------------------------------------------------------------
##  - A0B0C0D0E0F0GP
##  - A0BPC0D0E0F0GP
##  - A0B0CPD0E0F0GP
##  - ...
##------------------------------------------------------------------
#pred.test$BG_ONLY.pred      <- apply(pred.test[, c("A", "B.pred", "C", "D", "E", "F", "G.pred")], 1, paste, collapse="")
#pred.test$CG_ONLY.pred      <- apply(pred.test[, c("A", "B", "C.pred", "D", "E", "F", "G.pred")], 1, paste, collapse="")
#pred.test$AEFG_ONLY.pred    <- apply(pred.test[, c("A.pred", "B", "C", "D", "E.pred", "F.pred", "G.pred")], 1, paste, collapse="")

##******************************************************************
## Step 4: Isolate the last quote & subset the data
##******************************************************************

## isolate last quote
sub.idx                 <- tapply(pred.test$shopping_pt, pred.test$customer_ID, function(x){ which(x == max(x)) })
sub.key                 <- paste(names(sub.idx),sub.idx,sep="_")

## subset the data
pred.sub                <- pred.test[ which(pred.test$key %in% sub.key), ]


##******************************************************************
## Step 4: Construct submissions
##******************************************************************

##------------------------------------------------------------------
## last-quoted benchmark
##------------------------------------------------------------------
lastquoted.sub  <- data.frame(
                        customer_ID = as.integer(pred.sub$customer_ID),
                        shopping_pt = as.integer(pred.sub$shopping_pt),
                        plan = as.character(pred.sub$ABCDEFG.lq) )

## save in submission format
lastquoted.fin <- lastquoted.sub[ ,c("customer_ID","plan")]

##------------------------------------------------------------------
## single-choice predictions
##------------------------------------------------------------------
gbm_gonly.sub   <- data.frame(
                        customer_ID = as.integer(pred.sub$customer_ID),
                        shopping_pt = as.integer(pred.sub$shopping_pt),
                        key = as.character(pred.sub$key),
                        plan = as.character(pred.sub$G_ONLY.pred) )
## save in submission format
gbm_gonly.fin <- gbm_gonly.sub[ ,c("customer_ID","plan")]

##******************************************************************
## Step 5: Write submissions to file
##******************************************************************
write.csv(lastquoted.fin,                               file="S007_lastquoted.csv", row.names=FALSE)
write.csv(gbm_gonly.fin,                                file="S007_gbm_gonly.csv", row.names=FALSE)


##******************************************************************
## Results
##******************************************************************
