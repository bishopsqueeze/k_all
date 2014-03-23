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
all.copy$car_value      <- as.character(all.copy$car_value)

##------------------------------------------------------------------
## Clean bads
##------------------------------------------------------------------
##  Example blank   == 10000041
##  Example missing == 10000033
##  Example multi   == 10056192
##------------------------------------------------------------------

## columns to scrub
scrub.cols   <- c(names(all.na), names(all.bl))

## Loop over all of the columns to scrub
for (i in 1:length(scrub.cols)) {

    ## identify the column we're scrubbing
    tmp.col <- scrub.cols[i]
    new.col <- paste(tmp.col,".r", sep="")
    
    ## backfill NAs/blanks if possible
    if ( tmp.col == "car_value") {
        all.copy[, new.col] <- as.vector(unlist(tapply(as.character(all.copy[,tmp.col]), all.copy$customer_ID, FUN=replaceBads)))
    } else {
        all.copy[, new.col] <- as.vector(unlist(tapply(all.copy[,tmp.col], all.copy$customer_ID, FUN=replaceBads)))
    }
}

##------------------------------------------------------------------
## Replace scrubbed car_value factors with a numeric code
##------------------------------------------------------------------
tmp.ch          <- as.character(all.copy$car_value.r)
tmp.ch          <- ifelse(tmp.ch == "", "z", tmp.ch)       ## encode blanks as "z"
num.car_value   <- as.vector(unlist(sapply(tmp.ch, function(x){which(letters==x)})))

##------------------------------------------------------------------
## Normalize numeric ranges
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
## Append normalized variables
##------------------------------------------------------------------
all.copy$car_value.n    <- num.car_value
all.copy$state.n        <- num.state
all.copy$time.n         <- num.min
all.copy$age_old.n      <- norm.old
all.copy$age_young.n    <- norm.young
all.copy$cost.n         <- norm.cost
all.copy$car_age.n      <- norm.carage

##------------------------------------------------------------------
## Create additional variables
##------------------------------------------------------------------

## customer-day index
#all.copy$cust_day   <- factor(paste(all.copy$customer_ID, all.copy$day, sep="_"))
#
#   ## period-over-period time differences (by customer_ID && day) -- but list names may be reordered ...
#   tmp.list    <- tapply(all.copy$time.n, all.copy$cust_day, calcDiff)
#   tmp.names   <- names(tmp.list)
#
#   ## isolate unique customer-day values
#   uniq.cd     <- as.character(unique(all.copy$cust_day))
#
#    tmp <- unlist(sapply(uniq.cd, function(x){tmp.list[[x]]}))


## period-over-period cost differences (by customer_ID)
all.copy$dcost  <- as.vector(unlist(tapply(all.copy$cost, all.copy$customer_ID, calcDiff)))

## cumulative cost differences (by customer_ID)
all.copy$ccost  <- as.vector(unlist(tapply(all.copy$dcost, all.copy$customer_ID, function(x){cumsum(x)})))

## period-over-period change in each of the selection options
for (i in 1:7) {
    tmp.ch  <- LETTERS[i]
    tmp.d   <- paste("d",tmp.ch,sep="")
    
    all.copy[, tmp.d] <- as.vector(unlist(tapply(all.copy[,tmp.ch], all.copy[,c("customer_ID")], calcDiff)))
}

## rolling total of period-over-period change in each of the selection options
for (i in 1:7) {
    tmp.ch  <- LETTERS[i]
    tmp.d   <- paste("d",tmp.ch,sep="")
    tmp.n   <- paste("n",tmp.ch,sep="")
    
    all.copy[, tmp.n] <- as.vector(unlist(tapply(all.copy[,tmp.d], all.copy[,c("customer_ID")], function(x){cumsum((x!=0))} )))
}

##------------------------------------------------------------------
## Notes
##------------------------------------------------------------------
## can have locations in two states (e.g., neighboring states like KS/MO)


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.na, all.bl, all.copy, file="002_allstateRawData.Rdata")











