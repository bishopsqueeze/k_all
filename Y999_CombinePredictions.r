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

## Y020 - (score=0.54152)
Y020.g  <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/rf_scored_X020/Y020_rf_gonly.csv",
colClasses=c("character", "character"), header=TRUE)

## X021 - (score=0.54045)
X021.g  <- read.csv(file="/Users/alexstephens/Development/kaggle/allstate/data/svm_scored_X021/X021_svm_gonly.csv",
colClasses=c("character", "character"), header=TRUE)


##------------------------------------------------------------------
## split the submissions into plan constituents
##------------------------------------------------------------------
lq      <- subSplit(s=X008.lq)
g.S006  <- subSplit(s=S006.g)
g.X008  <- subSplit(s=X008.g)
g.X010  <- subSplit(s=X010.g)
g.Y020  <- subSplit(s=Y020.g)
g.X021  <- subSplit(s=X021.g)

##------------------------------------------------------------------
## combine the prediction
##------------------------------------------------------------------

## match index
match.index <- ((g.S006$G == g.Y020$G) & (g.S006$G == g.X008$G) &
                (lq$G != g.S006$G) & (lq$G != g.Y020$G) & (lq$G != g.X008$G))

## This yielded the best fit to date
## combine
#g.comb  <- data.frame(  s1=as.integer(g.S006$G),
#                        s2=as.integer(g.X008$G),
#                        s3=as.integer(g.Y020$G))
## estimate the median
#g.med   <- as.character(apply(g.comb, 1, median))


##------------------------------------------------------------------
## create a submission file
##------------------------------------------------------------------
tmp                  <- lq
tmp$G[match.index]   <- g.S006$G[match.index]

sub     <- data.frame(
                customer_ID=tmp$customer_ID,
                plan=apply(tmp[, LETTERS[1:7]], 1, paste, sep="", collapse="")
            )

##------------------------------------------------------------------
## Write the file
##------------------------------------------------------------------
write.csv(sub, file="Y022_S006.EQ.Y020.EQ.X008.NE.LQ_gonly.csv", row.names=FALSE)





