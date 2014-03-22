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
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

##------------------------------------------------------------------
## Read the data
##------------------------------------------------------------------
load("001_allstateRawData.Rdata")

## Make copies of the train/test data
tr <- train.raw; rm(train.raw)
te <- test.raw;  rm(test.raw)

## Append an ID flag to each set
tr$id_fl    <- as.integer(1)
te$id_fl    <- as.integer(0)

##------------------------------------------------------------------
## Combine and sort on customer ID
##------------------------------------------------------------------
all.data    <- rbind(tr, te)
all.data    <- all.data[ order(all.data$customer_ID, all.data$shopping_pt), ]

##------------------------------------------------------------------
## Identify instances of blanks/NAs in the data
##------------------------------------------------------------------

## n/a == c("location", "risk_factor", "C_previous", "duration_previous")
all.na <- apply(all.data, 2, function(x){sum(is.na(x))})
all.na <- all.na[all.na>0]

## blanks == == c("car_value")
all.bl <- apply(all.data, 2, function(x){sum(ifelse(x=="", 1, 0))})
all.bl <- all.bl[!is.na(all.bl) & (all.bl>0)]

##------------------------------------------------------------------
## Replace car_value factors with a numeric code
##------------------------------------------------------------------
tmp.ch          <- as.character(all.data$car_value)
tmp.ch          <- ifelse(tmp.ch == "", "z", tmp.ch)       ## encode blanks as "z"
num.car_value   <- as.vector(unlist(sapply(tmp.ch, function(x){which(letters==x)})))

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
## Create a copy & load the additional data
##------------------------------------------------------------------
all.copy                <- all.data
all.copy$car_value      <- as.character(all.copy$car_value) ##

##------------------------------------------------------------------
## Clean via a loop
##------------------------------------------------------------------
##  Example blank   == 10000041
##  Example missing == 10000033
##------------------------------------------------------------------
scrub.cols   <- c(names(all.na), names(all.bl))

##------------------------------------------------------------------
## Loop over all of the columns to scrub
##------------------------------------------------------------------
for (i in 1:length(scrub.cols)) {

    ## identify the column we're scrubbing
    tmp.col <- scrub.cols[i]
    new.col <- paste(tmp.col,".nona", sep="")
    
    ## backfill NAs/blanks if possible
    if ( tmp.col == "car_value") {
        all.copy[, new.col] <- as.vector(unlist(tapply(as.character(all.copy[,tmp.col]), all.copy$customer_ID, FUN=replaceBads)))
    } else {
        all.copy[, new.col] <- as.vector(unlist(tapply(all.copy[,tmp.col], all.copy$customer_ID, FUN=replaceBads)))
    }
}

## *** there must be cases where there are multiple car values
## *** same for risk_factor
tmp.cust <- unique(all.copy[ is.na(all.copy$risk_factor), c("customer_ID")])
a        <- all.copy[ which(all.copy$customer_ID %in% tmp.cust), ]



rf.var <- tapply(all.copy$risk_factor, all.copy$customer_ID, function(x){length(unique(x))})




##------------------------------------------------------------------
## Normalize age ranges
## Normalize costs (but keep raw costs)
## Normalize car age
##------------------------------------------------------------------
old         <- all.data$age_old
norm.old    <- (old - min(old, na.rm=TRUE)) / (max(old, na.rm=TRUE) - min(old, na.rm=TRUE))

young       <- all.data$age_young
norm.young  <- (young - min(young, na.rm=TRUE)) / (max(young, na.rm=TRUE) - min(young, na.rm=TRUE))

cost        <- all.data$cost
norm.cost   <- (cost - mean(cost, na.rm=TRUE))/sd(cost, na.rm=TRUE)

carage      <- all.data$car_age
norm.carage <- (carage - min(carage, na.rm=TRUE)) / (max(carage, na.rm=TRUE) - min(carage, na.rm=TRUE))


##------------------------------------------------------------------
##
##------------------------------------------------------------------

all.copy$car_value.n    <- num.car_value
all.copy$state.n        <- num.state
all.copy$time.n         <- num.min


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
save(all.na, all.bl file="002_allstateRawData.Rdata")











