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

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_S004")

##------------------------------------------------------------------
## Placeholder lists
##------------------------------------------------------------------
xcheck.list <- list()
xcheck.out  <- list()

##******************************************************************
## Step 1: Load fit data/results for each panel and each choice prediction
##******************************************************************

## loop over all panels
for (i in 2:11) {

    cat("Iteration ",i, " ... of 11 \n")
    
    ## define the panel
    tmp.sp <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")

    ## load the raw panel -- will load tmp.object
    tmp.panel <- paste("/Users/alexstephens/Development/kaggle/allstate/data/panels/004_allstatePanelData_Train.",tmp.sp,".Rdata",sep="")
    load(tmp.panel)

    ## define the raw panel
    tmp.raw <- tmp.object$data[ , c("customer_ID", "shopping_pt", "record_type", paste(LETTERS[1:7],"T",sep=""), paste(LETTERS[1:7],"0",sep="")) ]
    
    ## loop over all choices and append the prediciton to the raw.panel
    for (j in 1:7) {
        
        ## define the choice
        tmp.ch <- LETTERS[j]
    
        ## load the fit data -- will load "tmp.fit"
        tmp.loadfile <- paste(tmp.sp,".Group_",tmp.ch,".gbmCaretFit_AllSample_REPCV.Rdata",sep="")
        load(tmp.loadfile)
    
        ## compute the prediction
        tmp.pred <- predict(tmp.fit, type="raw")
    
        ## combine the fit data and the raw data
        tmp.col  <- paste(tmp.ch,"P",sep="")
        tmp.raw  <- data.frame(tmp.raw, tmp.col=tmp.pred)
        colnames(tmp.raw)[which(colnames(tmp.raw) == "tmp.col")] <- tmp.col
   
     }
    
    ## save results in a list
    xcheck.list[[tmp.sp]] <- tmp.raw
}


##******************************************************************
## Step 2: Expand all possible permutations of choices
##******************************************************************
ch.combos <- expand.grid(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1), e=c(0,1), f=c(0,1), g=c(0,1))
ch.combos <- ch.combos[which(apply(ch.combos, 1, sum) >= 1), ]


##******************************************************************
## Step 3:  Loop over all shopping_pts and choice combos and create
## a predicted plan ... append to the original data so we can do comparisons
##******************************************************************
for (i in 11:2) {
    
    cat("Iteration ",i, " ... of 11 \n")
    
    ## isolate the xcheck data
    tmp.sp      <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")
    tmp.xcheck  <- xcheck.list[[tmp.sp]]

    ## loop over all possible combinations
    for (j in 1:nrow(ch.combos)) {
        
        ## extract the combo, and replicate to the size of the panel
        tmp.idx  <- do.call(rbind, replicate(nrow(tmp.xcheck), ch.combos[j, ], simplify=FALSE))
        
        ## isolate last-quoted and predicted results
        tmp.lq   <- tmp.xcheck[ , paste(LETTERS[1:7],"0",sep="")]
        tmp.pred <- tmp.xcheck[ , paste(LETTERS[1:7],"P",sep="")]
        
        ## convert to matrices
        mat.lq   <- sapply(tmp.lq, function(x) { as.numeric(as.character(x)) } )
        mat.pred <- sapply(tmp.pred, function(x) { as.numeric(as.character(x)) } )
      
        ## combine predicted and last-quoted
        filt.pred           <- mat.pred*(tmp.idx)
        colnames(filt.pred) <- colnames(mat.pred)
        
        filt.lq             <- mat.lq*(!tmp.idx)
        colnames(filt.lq)   <- colnames(mat.lq)
        
        filt.out            <- filt.pred + filt.lq
        colnames(filt.out)  <- colnames(mat.pred)
        colnames(filt.out)[ !as.logical(tmp.idx[1,]) ] <- colnames(filt.lq)[!as.logical(tmp.idx[1,])]
        
        ## create the combined prediciton
        comb.id   <- paste(colnames(filt.out), collapse="")
        comb.pred <- apply(filt.out, 1, paste, collapse="")
        
        tmp.xcheck[ , comb.id] <- comb.pred
    }
    
    tmp.xcheck[, paste(LETTERS[1:7],"T", sep="", collapse="")] <- apply(tmp.xcheck[, paste(LETTERS[1:7],"T",sep="")], 1, paste, collapse="")
    tmp.xcheck[, paste(LETTERS[1:7],"0", sep="", collapse="")] <- apply(tmp.xcheck[, paste(LETTERS[1:7],"0",sep="")], 1, paste, collapse="")
    
    xcheck.out[[tmp.sp]] <- tmp.xcheck
}


##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(xcheck.list, ch.combos, xcheck.out, file="/Users/alexstephens/Development/kaggle/allstate/data/S004_CaretGbmXcheck.Rdata")





