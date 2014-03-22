##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Read the data
##------------------------------------------------------------------
load("001_allstateRawData.Rdata")

## Make copies of the train/test data
tr <- train.raw; rm(train.raw)
te <- test.raw;  rm(test.raw)

## Append an ID flag to each set
tr$id_fl    <- 1
te$id_fl    <- 0

##------------------------------------------------------------------
## Combine and sort on customer ID
##------------------------------------------------------------------
all.data    <- rbind(tr, te)
all.data    <- all.data[ order(all.data$customer_ID, all.data$shopping_pt), ]


##------------------------------------------------------------------
## Identify instances of blanks/NAs in the data
##------------------------------------------------------------------

## n/a
all.na <- apply(all.data, 2, function(x){sum(is.na(x))})
all.na <- all.na[all.na>0]

## blanks
all.bl <- apply(all.data, 2, function(x){sum(ifelse(x=="", 1, 0))})
all.bl <- all.bl[all.bl>0]

## write tables of the missings/blanks for offline use

##------------------------------------------------------------------
##
##------------------------------------------------------------------
cleanMissings   <- function(x) {
}

cleanBlanks     <- function() {
}

##------------------------------------------------------------------
## Clean via a loop
##------------------------------------------------------------------
uniq.cust   <- unique(all.data$customer_ID)
num.cust    <- length(uniq.cust)
na.cols     <- names(all.na)
bl.cols     <- names(all.bl)

for (i in 1:num.cust) {
    
    tmp.cust    <- uniq.cust[i]
    
}
## for each customer block, check to see if there are missings & backfill if incomplete, otherwise make some sort of assumpton


##------------------------------------------------------------------
## Transform all character codes into numeric and save results as a matrix
##------------------------------------------------------------------


##------------------------------------------------------------------
## Replace missings (assumptions galore)
##------------------------------------------------------------------

## train
tr.cl <- tr

## replace missing ints with 0
tr.cl[is.na(tr.cl$C_previous), c("C_previous")] <- 0
## create a new category
tr.cl[is.na(tr.cl$duration_previous), c("duration_previous")] <- 0
## assume zero duration
tr.cl[is.na(tr.cl$risk_factor), c("risk_factor")] <- 0

## replace missing factor with a new factor
tmp.car_value <- as.character(tr.cl$car_value)
tmp.car_value <- ifelse(tmp.car_value == "", "x", tmp.car_value)
tr.cl$car_value <- factor(tmp.car_value)


## test
te.cl <- te

## replace missing ints with 0
te.cl[is.na(te.cl$C_previous), c("C_previous")] <- 0
## create a new category
te.cl[is.na(te.cl$duration_previous), c("duration_previous")] <- 0
## assume zero duration
te.cl[is.na(te.cl$risk_factor), c("risk_factor")] <- 0

## replace missing factor with a new factor
tmp.car_value <- as.character(te.cl$car_value)
tmp.car_value <- ifelse(tmp.car_value == "", "x", tmp.car_value)
te.cl$car_value <- factor(tmp.car_value)


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(train.raw, test.raw, tr, te, tr.cl, te.cl, file="002_allstateRawData.Rdata")











