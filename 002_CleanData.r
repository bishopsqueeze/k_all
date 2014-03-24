##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Address some of the known issues with the raw data
##  2. Normalize variable ranges
##  3. Create variables so that the entire dataset can be put into
##     a matrix rather than a data frame
##------------------------------------------------------------------

##------------------------------------------------------------------
## Observations
##------------------------------------------------------------------
## can have locations in two states (e.g., neighboring states like KS/MO)

##------------------------------------------------------------------
## Load libraries
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
## Assign state factors a numeric code
##------------------------------------------------------------------
tmp.state   <- as.character(all.data$state)
ref.state   <- c(state.abb, "DC")
num.state   <- as.vector(unlist(sapply(tmp.state, function(x){which(ref.state == x)})))

##------------------------------------------------------------------
## Transform HH:MM to minutes since 00:00
##------------------------------------------------------------------
num.min    <- as.numeric(substr(all.data$time,1,2))*60 + as.numeric(substr(all.data$time,4,5))

##------------------------------------------------------------------
## Create a copy of the data & load the additional data
##------------------------------------------------------------------
all.copy                <- all.data
all.copy$car_value      <- as.character(all.copy$car_value)

##******************************************************************
## From here on, all operations should be on the data copy
##******************************************************************

##------------------------------------------------------------------
## Clean bads
##------------------------------------------------------------------
##  Example blank   == 10000041
##  Example missing == 10000033
##  Example multi   == 10056192 (incl. day change for purchase)
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
tmp.ch          <- ifelse(tmp.ch == "", "z", tmp.ch)       ## encode remaining blanks as "z"
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
all.copy$car_value.num  <- num.car_value
all.copy$state.num      <- num.state
all.copy$time.num       <- num.min
all.copy$dayfrac.nrm    <- num.min / (24*60)
all.copy$dayfrac.diff   <- c(0, diff(all.copy$dayfrac.nrm))
all.copy$age_old.nrm    <- norm.old
all.copy$age_young.nrm  <- norm.young
all.copy$cost.nrm       <- norm.cost
all.copy$car_age.nrm    <- norm.carage

##------------------------------------------------------------------
## Create additional variables
##------------------------------------------------------------------

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

## clean the day fraction difference
all.copy$custday_key    <- paste(all.copy$customer_ID, all.copy$day, sep="_")
all.copy$dayfrac.diff[ !duplicated(all.copy$custday_key) ] <- 0

## create a cumulative dayfrac variable
all.copy$dayfrac.cum    <- as.vector(unlist(tapply(all.copy$dayfrac.diff, all.copy$customer_ID, function(x){cumsum(x)})))

## create a last-observation flag for each customer
num.cust                    <- length(all.copy$customer_ID)
last.idx                    <- unlist( lapply(split(1:num.cust, all.copy$customer_ID), tail, 1) )
all.copy$last_fl            <- 0
all.copy$last_fl[last.idx]  <- 1

##------------------------------------------------------------------
## Re-code variables with missings
##------------------------------------------------------------------
all.copy$risk_factor.r[ which(all.copy$risk_factor.r == -9) ]   <- 5
all.copy$location.r[ which(all.copy$location.r == -9) ]      <- 99999


##------------------------------------------------------------------
## Attempt to identify location-specific clusters
##------------------------------------------------------------------

## create location-specific summary data
cl.data <- data.frame(
                    cost.mean = tapply(all.copy$cost.nrm, all.copy$location.r, mean, na.rm=TRUE),
                    car_age.mean = tapply(all.copy$car_age.nrm, all.copy$location.r, mean, na.rm=TRUE),
                    home.frac = tapply(all.copy$homeowner, all.copy$location.r, function(x){sum(x)/length(x)}),
                    married.frac = tapply(all.copy$married_couple, all.copy$location.r, function(x){sum(x)/length(x)})
                    )

## define the cluster
cl.res  <- hclust(dist(cl.data), method = "ward")
cl.memb <- cutree(cl.res, k = 10)

## load the results
hc <- rep(0, nrow(all.copy))
for (i in 1:length(cl.memb)) {
    tmp.idx <- which( all.copy$location.r == as.numeric(names(cl.memb[i])) )
    hc[tmp.idx] <- cl.memb[i]
}
all.copy$hc <- hc

##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.na, all.bl, all.copy, file="002_allstateRawData.Rdata")











