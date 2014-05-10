##------------------------------------------------------------------
## The purpose of this script is to:
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
#library(foreach)
#library(doMC)
#library(caret)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
#registerDoMC(4)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## <function> Submission Split
##------------------------------------------------------------------
subSplit <- function(s) {
  tmp <- data.frame(customer_ID = s$customer_ID)
  for (i in 1:7) {
    tmp[,LETTERS[i]] <- as.vector(substr(s$plan,i,i))
  }  
  return(tmp)
}

##------------------------------------------------------------------
## G-only:  Read select results from various submissions
##------------------------------------------------------------------

## Read in a confirmed-good last-quoted benchmark
X008.lq <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_X008/X008_lastquoted.csv", 
                    colClasses=c("character", "character"), header=TRUE)

## S006 - (score=0.54152)
S006.g  <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_S006/S006_gbm_gonly.csv", 
                    colClasses=c("character", "character"), header=TRUE)

## X008 - (score=0.54146)
X008.g  <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_X008/X008_gbm_gonly.csv", 
                    colClasses=c("character", "character"), header=TRUE)

## X010 - (score=0.54128)
X010.g  <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored_X010/X010_gbm_gonly.csv", 
                    colClasses=c("character", "character"), header=TRUE)

##------------------------------------------------------------------
## split the submissions into plan constituents
##------------------------------------------------------------------
lq      <- subSplit(s=X008.lq)
g.S006  <- subSplit(s=S006.g)
g.X008  <- subSplit(s=X008.g)
g.X010  <- subSplit(s=X010.g)

##------------------------------------------------------------------
## combine the prediction
##------------------------------------------------------------------
g.comb  <- data.frame(a=g.S006$G, b=g.X008$G, c=g.X010$G)
g.med   <- apply(g.comb, 1, median)




##------------------------------------------------------------------
## create a submission file
##------------------------------------------------------------------
tmp     <- lq
tmp$G   <- as.character(g.med)


paste(tmp[, LETTERS[1:7]], sep="", collapse="")






