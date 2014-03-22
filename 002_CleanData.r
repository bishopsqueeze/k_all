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
all.bl <- all.bl[!is.na(all.bl) & (all.bl>0)]

## write tables of the missings/blanks for offline use

##------------------------------------------------------------------
## Replace car_value factors with a numeric code
##------------------------------------------------------------------
tmp.ch          <- as.character(all.data$car_value)
tmp.ch          <- ifelse(tmp.ch == "", "z", tmp.ch)
num.car_value   <- as.vector(unlist(sapply(tmp.ch, function(x){which(letters==x)})))

#all.data$car_val    <- value


##------------------------------------------------------------------
## Replace state factors with a numeric code
##------------------------------------------------------------------
tmp.state   <- as.character(all.data$state)
ref.state   <- c(state.abb, "DC")
num.state   <- as.vector(unlist(sapply(tmp.state, function(x){which(ref.state == x)})))

##------------------------------------------------------------------
## Replace HH:MM with minutes since 00:00
##------------------------------------------------------------------
num.min    <- as.numeric(substr(all.data$time,1,2))*60 + as.numeric(substr(all.data$time,4,5))



##------------------------------------------------------------------
## Create a backup
##------------------------------------------------------------------
all.bkup    <- all.data

all.bkup$car_value  <- NULL
all.bkup$stae       <- NULL
all.bkup$time       <- NULL

all.bkup$car_value  <- num.car_value
all.bkup$state      <- num.state
all.bkup$time       <- num.min

##------------------------------------------------------------------
## Normalize age ranges
## Normalize costs (but keep raw costs)
## Normalize car age
##------------------------------------------------------------------


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

## 10000041 blank
## 10000033 missing

uniq.cust   <- unique(all.data$customer_ID)
num.cust    <- length(uniq.cust)
na.cols     <- names(all.na)
bl.cols     <- names(all.bl)

scrub.cols  <- c(na.cols, bl.cols)
all.bkup    <- all.data

## Loop over all of the columns to scrub
#for (i in 1:length(scrub.cols)) {
for (i in 5:5) {

    ## identify all of the customers with bad column values
    tmp.col <- scrub.cols[i]
    if ( tmp.col != "car_value") {
        bad.idx <- which(is.na(all.data[, tmp.col]))
    } else {
        bad.idx <- which(all.data[, tmp.col] == "")
    }
    
    ## extract all data for those customers with bad column values
    bad.cust <- unique(all.data[bad.idx, c("customer_ID")])
    bad.data <- all.data[ which(all.data$customer_ID %in% bad.cust), ]
    
    ## if there are non-bad data associated with that customer, then
    ## replace the bads for that customer with that value
    
    ## some non-bads exist
    if (nrow(bad.data) != sum(is.na(bad.data[,tmp.col]))) {
        
        for (j in 1:20) {
        #for (j in 1:length(bad.cust)) {
        
            tmp.cust    <- bad.cust[j]
            tmp.idx     <- which(bad.data$customer_ID == tmp.cust)
            tmp.data    <- bad.data[tmp.idx,]
            
            ## deal with NAs
            if (tmp.col != "car_value") {
                
                ## all bad
                if (nrow(tmp.data) == sum(is.na(bad.data[tmp.idx,tmp.col]))) {
                   all.bkup[ which(all.bkup$customer_ID == tmp.cust), tmp.col] <- 9
                ## some good
                } else {
                    tmp.replace <- tmp.data[which(!is.na(tmp.data[,tmp.col])), tmp.col][1]
                    all.bkup[ which(all.bkup$customer_ID == tmp.cust), tmp.col] <- tmp.replace
                }
                
            ## deal with blanks
            } else {
                
                ## all bad
                if (nrow(tmp.data) == sum(bad.data[tmp.idx,tmp.col] == "")) {
                    all.bkup[ which(all.bkup$customer_ID == tmp.cust), tmp.col] <- factor("x")
                    ## some good
                } else {
                    tmp.replace <- tmp.data[which(!(tmp.data[,tmp.col] == "")), tmp.col][1]
                    
                    all.bkup[ which(all.bkup$customer_ID == tmp.cust), tmp.col] <- factor(tmp.replace)
                }
            }
        }

    ## all bad
    } else {
        if (tmp.col == "location") {
            all.bkup[bad.idx, tmp.col] <- 99999
        }
    }
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











