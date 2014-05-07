##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
## What you'll want to do here is use the hold-out data to compute
## predictions for each of the choices in each of the panels.
## Then you'll want to use that data to create all possible
## permutations of choice/submission combo.  Given that the data were
## not trained on these data, it should be a fair reflection of the
## predictive accuracy of the model ... and you can shop amongst the
## permuatations to identify the optimal approach ... same can be
## done using the training data ... but a bias will be present
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(gbm)
library(plyr)
library(caret)
library(foreach)
library(doMC)

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
setwd("/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_S005")

##------------------------------------------------------------------
## Placeholder lists
##------------------------------------------------------------------
xcheck.list <- list()
xcheck.out  <- list()


##******************************************************************
## Step 1: Confirm that there is consistency in the data isolated
##         for hold-out testing (there should be b/c the seed was
##         set prior to sampling ... but confirm that's the case)
##******************************************************************
hold.xcheck <- list()
hold.index  <- list()

## loop over all panels
for (i in 2:11) {
    
    cat("Iteration ",i, " ... of 11 \n")
    
    ## define the panel
    tmp.sp <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")

    ## placeholder for holdout indices
    tmp.hold    <- data.frame()

    ## loop over all choices and append the prediciton to the raw.panel
    for (j in 1:7) {
        
        ## define the choice
        tmp.ch <- LETTERS[j]
        
        ## load the fit data -- will load "tmp.fit"
        tmp.loadfile <- paste(tmp.sp,".Group_",tmp.ch,".gbmCaretFit_AllSample_REPCV.Rdata",sep="")
        load(tmp.loadfile)
        
        ## identify the rows used in the holdout testing
        tmp.holdIndex   <- as.numeric(rownames(holdDescr))
        
        if (j == 1) {
            tmp.hold    <- tmp.holdIndex
        } else {
            tmp.hold    <- cbind(tmp.hold, tmp.holdIndex)
        }
    }

    ## confirm uniformity of the indices and retain the indices
    hold.xcheck[[tmp.sp]]   <- sum( tmp.hold - apply(tmp.hold, 1, mean) )
    hold.index[[tmp.sp]]    <- tmp.holdIndex
}


##******************************************************************
## Step 2: Load each of the panels, and isolate the rows that are
##         associated with the hold-out data.  Then append
##******************************************************************

## loop over all panels
for (i in 2:11) {

    cat("Iteration ",i, " ... of 11 \n")
    
    ## define the panel
    tmp.sp <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")

    ## load the raw panel -- will load tmp.object
    tmp.panel <- paste("/Users/alexstephens/Development/kaggle/allstate/data/panels/004_allstatePanelData_Train.",tmp.sp,".Rdata",sep="")
    load(tmp.panel)

    ## isolate the holdout index
    tmp.holdIndex   <- hold.index[[tmp.sp]]
    tmp.holdOrder   <- order(tmp.holdIndex)
    
    ## define the panel data index
    panel.idx   <- which(rownames(tmp.object$data) %in% tmp.holdIndex)
    
    ## define the raw panel associated with the holdout sample
    tmp.raw <- tmp.object$data[panel.idx, c("customer_ID", "shopping_pt", "record_type", paste(LETTERS[1:7],"T",sep=""), paste(LETTERS[1:7],"0",sep="")) ]
    
    ## loop over all choices and append the prediciton to the raw.panel
    for (j in 1:7) {
        
        ## define the choice
        tmp.ch <- LETTERS[j]
    
        ## load the fit data -- will load "tmp.fit"
        tmp.loadfile <- paste(tmp.sp,".Group_",tmp.ch,".gbmCaretFit_AllSample_REPCV.Rdata",sep="")
        load(tmp.loadfile)
    
        ## sort the holdout data
        
        ## compute the prediction
        tmp.pred <- predict(tmp.fit, newdata=holdDescr[tmp.holdOrder,], type="raw")
    
        ## combine the fit data and the raw data
        tmp.col  <- paste(tmp.ch,"P",sep="")
        #tmp.raw  <- data.frame(tmp.raw, tmp.col=tmp.pred, tmp.class=holdClass[tmp.holdOrder])
        tmp.raw  <- data.frame(tmp.raw, tmp.col=tmp.pred)
        colnames(tmp.raw)[which(colnames(tmp.raw) %in% c("tmp.col"))] <- c(tmp.col)
   
     }
    
    ## save results in a list
    xcheck.list[[tmp.sp]] <- tmp.raw
}


##******************************************************************
## Step 3: Expand all possible permutations of choices
##******************************************************************
ch.combos <- expand.grid(a=c(0,1), b=c(0,1), c=c(0,1), d=c(0,1), e=c(0,1), f=c(0,1), g=c(0,1))
ch.combos <- ch.combos[which(apply(ch.combos, 1, sum) >= 1), ]


##******************************************************************
## Step 4:  Loop over all shopping_pts and choice combos and create
##          the entire universe of predicted plans.  Then append
##          those plans to the original data so we can shop for the
##          best possible combination of predictors
##******************************************************************
for (i in 2:11) {
    
    cat("Iteration ",i, " ... of 11 \n")
    
    ## isolate the xcheck data
    tmp.sp      <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")
    tmp.xcheck  <- xcheck.list[[tmp.sp]]

    ## preallocate a matrix
    perm.mat    <- matrix("character", nrow=dim(tmp.xcheck)[1], ncol=dim(ch.combos)[1])
    perm.col    <- vector("character", length=dim(ch.combos)[1])
    
    ## loop over all possible combinations
    for (j in 1:nrow(ch.combos)) {
    
        cat("Iteration j == ",j, "\n")

        ## extract the combo, and replicate to the size of the panel
        m           <- ch.combos[j, ]
        rownames(m) <- NULL
        tmp.idx  <- m[rep(1, times=nrow(tmp.xcheck)),]
        
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
        
        #tmp.xcheck[ , comb.id] <- comb.pred
        perm.mat[, j]  <- comb.pred
        perm.col[j]    <- comb.id
    }
    
    ## add columns
    colnames(perm.mat) <- perm.col
    
    ## load the results
    tmp.xcheck[, paste(LETTERS[1:7],"T", sep="", collapse="")] <- apply(tmp.xcheck[, paste(LETTERS[1:7],"T",sep="")], 1, paste, collapse="")
    tmp.xcheck[, paste(LETTERS[1:7],"0", sep="", collapse="")] <- apply(tmp.xcheck[, paste(LETTERS[1:7],"0",sep="")], 1, paste, collapse="")
    
    ## save results into a list
    xcheck.out[[tmp.sp]] <- cbind(tmp.xcheck, perm.mat)
}


##******************************************************************
## Step 5: For each shopping_pt ... compute the average accuracy
##******************************************************************

##------------------------------------------------------------------
## Compute accuracies using the full hold-out sample
##------------------------------------------------------------------

## aggregate results by plan permutation and shopping_pt
res.holdout <- data.frame()
for (i in 2:11) {

    ## isolate the data
    tmp.sp      <- paste("SP_",ifelse(i < 10, paste("0",i,sep=""),i),sep="")
    tmp.out     <- xcheck.out[[tmp.sp]][, 25:ncol(xcheck.out[[tmp.sp]])]

    tmp.res     <- vector("numeric", length=ncol(tmp.out))
    for (j in 1:ncol(tmp.out)) {
        tmp.res[j] <- mean(as.character(tmp.out[,1]) == as.character(tmp.out[,j]))
    }
    res.holdout     <- rbind(res.holdout, tmp.res)
    
}
colnames(res.holdout) <- colnames(tmp.out)
rownames(res.holdout) <- names(xcheck.out)[order(names(xcheck.out))]
res.holdout           <- t(res.holdout)
res.holdout           <- res.holdout[2:nrow(res.holdout),]

## identify the optimal approach for each shopping_pt
optimal.plans <- apply(res.holdout, 2, function(x){ which(x == max(x))})



##------------------------------------------------------------------
## Save results
##------------------------------------------------------------------
save(xcheck.list, ch.combos, xcheck.out, res.holdout, optimal.plans,
            file="/Users/alexstephens/Development/kaggle/allstate/data/S004_CaretGbmXcheck_v4.Rdata")

