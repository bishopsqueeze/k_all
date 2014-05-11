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
load("Y003_allstateRawData.Rdata"); rm(all.copy, cost.test, cost.train, hist.test, hist.train)

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
setwd("/Users/alexstephens/Development/kaggle/allstate/data/svm_scored_X021")

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


## compensate for multi-choice variables
if (nchar(tmp.group) > 1) {
    for (j in 1:nchar(tmp.group)) {
        tmp.col                 <- paste(substr(tmp.group, j, j),".pred",sep="")
        pred.test[ , tmp.col ]  <- unlist(lapply( pred.test[ , c(paste(tmp.group,".pred",sep="")) ], substr, j, j))
    }
}

## deal with missings from exluded fits
col.check   <- colnames(pred.test)[ grep(".pred", colnames(pred.test)) ]
for (i in 1:length(col.check)) {
    tmp.col     <- col.check[i]
    tmp.lq      <- gsub(".pred", "", tmp.col)
    bad.index   <- is.na(pred.test[, tmp.col])
    pred.test[bad.index, tmp.col] <- pred.test[bad.index, tmp.lq]
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
#pred.test$C_ONLY.pred    <- apply(pred.test[, c("A", "B", "C.pred", "D", "E", "F", "G")], 1, paste, collapse="")
#pred.test$D_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D.pred", "E", "F", "G")], 1, paste, collapse="")
#pred.test$E_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E.pred", "F", "G")], 1, paste, collapse="")
#pred.test$F_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F.pred", "G")], 1, paste, collapse="")
pred.test$G_ONLY.pred    <- apply(pred.test[, c("A", "B", "C", "D", "E", "F", "G.pred")], 1, paste, collapse="")
#pred.test$CG_ONLY.pred   <- apply(pred.test[, c("A", "B", "C.pred", "D", "E", "F", "G.pred")], 1, paste, collapse="")


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
lastquoted.fin <- lastquoted.sub[ ,c("customer_ID", "plan")]

##------------------------------------------------------------------
## single-choice predictions
##------------------------------------------------------------------

## G-only
gbm_gonly.sub   <- data.frame(
                        customer_ID = as.integer(pred.sub$customer_ID),
                        shopping_pt = as.integer(pred.sub$shopping_pt),
                        key = as.character(pred.sub$key),
                        plan = as.character(pred.sub$G_ONLY.pred) )
## save in submission format
gbm_gonly.fin <- gbm_gonly.sub[ ,c("customer_ID", "plan")]


## C-only
#gbm_conly.sub   <- data.frame(
#                        customer_ID = as.integer(pred.sub$customer_ID),
#                        shopping_pt = as.integer(pred.sub$shopping_pt),
#                        key = as.character(pred.sub$key),
#                        plan = as.character(pred.sub$C_ONLY.pred) )
## save in submission format
#gbm_conly.fin <- gbm_conly.sub[ ,c("customer_ID", "plan")]



## C+G
#gbm_cg.sub   <- data.frame(
#                        customer_ID = as.integer(pred.sub$customer_ID),
#                        shopping_pt = as.integer(pred.sub$shopping_pt),
#                        key = as.character(pred.sub$key),
#                        plan = as.character(pred.sub$CG_ONLY.pred) )
## save in submission format
#gbm_cg.fin <- gbm_cg.sub[ ,c("customer_ID", "plan")]



##------------------------------------------------------------------
## Create the X then Y then Z ... submissions
##------------------------------------------------------------------

## index of changes vs. last quoted benchmark
lq.idx  <- (as.character(lastquoted.fin$plan) != as.character(lastquoted.fin$plan))
g.idx   <- (as.character(lastquoted.fin$plan) != as.character(gbm_gonly.fin$plan))
#b.idx   <- (as.character(lastquoted.fin$plan) != as.character(gbm_bonly.fin$plan))
#c.idx   <- (as.character(lastquoted.fin$plan) != as.character(gbm_conly.fin$plan))
#e.idx   <- (as.character(lastquoted.fin$plan) != as.character(gbm_eonly.fin$plan))
#f.idx   <- (as.character(lastquoted.fin$plan) != as.character(gbm_fonly.fin$plan))

## translate plans from factors to characters
lq.ch           <- convert.magic(lastquoted.fin, "plan", "character")
gonly.ch        <- convert.magic(gbm_gonly.fin, "plan", "character")
#bonly.ch        <- convert.magic(gbm_bonly.fin, "plan", "character")
#conly.ch        <- convert.magic(gbm_conly.fin, "plan", "character")
#eonly.ch        <- convert.magic(gbm_eonly.fin, "plan", "character")
#fonly.ch        <- convert.magic(gbm_fonly.fin, "plan", "character")

## create updated plans by selectively adding variables
#gc.idx                <- !(g.idx) & (c.idx)   ## not where g changed from LQ & where c changed from LQ
#gbm_gc.layer          <- gonly.ch
#gbm_gc.layer[gc.idx,] <- conly.ch[gc.idx, ]

## create updated plans by selectively adding variables
#gb.idx          <- !(g.idx) & (b.idx)   ## not where g changed from LQ & where c changed from LQ
#gbm_gb          <- gonly.ch
#gbm_gb[gb.idx,] <- bonly.ch[gb.idx, ]

## create updated plans by selectively adding variables
#ge.idx          <- !(g.idx) & (e.idx)   ## not where g changed from LQ & where c changed from LQ
#gbm_ge          <- gonly.ch
#gbm_ge[ge.idx,] <- eonly.ch[ge.idx, ]

## create updated plans by selectively adding variables
#gf.idx          <- !(g.idx) & (f.idx)   ## not where g changed from LQ & where c changed from LQ
#gbm_gf          <- gonly.ch
#gbm_gf[gf.idx,] <- fonly.ch[gf.idx, ]


##******************************************************************
## Step 5: Write submissions to file
##******************************************************************
write.csv(lastquoted.fin,   file="X021_lastquoted.csv", row.names=FALSE)
write.csv(gbm_gonly.fin,    file="X021_svm_gonly.csv", row.names=FALSE)
#write.csv(gbm_conly.fin,    file="Y015_gbm_conly.csv", row.names=FALSE)
#write.csv(gbm_cg.fin,       file="Y015_gbm_cg.csv", row.names=FALSE)
#write.csv(gbm_gc.layer,     file="Y015_gbm_cg_layer.csv", row.names=FALSE)
#write.csv(gbm_gb,           file="X011_gbm_gb.csv", row.names=FALSE)
#write.csv(gbm_ge,           file="X011_gbm_ge.csv", row.names=FALSE)
#write.csv(gbm_gf,           file="X011_gbm_gf.csv", row.names=FALSE)



##******************************************************************
## Results
##******************************************************************
