##------------------------------------------------------------------
## The purpose of this script is to:
##	1. A complete re-think of the structure of the data
##------------------------------------------------------------------

##------------------------------------------------------------------
## Observations
##------------------------------------------------------------------
## can have locations in two states (e.g., neighboring states like KS/MO)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(foreach)
library(doMC)
library(caret)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

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
## Combine and sort on (customer_ID, shopping_pt)
##------------------------------------------------------------------
all.data        <- rbind(tr, te)
all.data        <- all.data[ order(all.data$customer_ID, all.data$shopping_pt), ]
all.data$key    <- as.factor(paste(all.data$customer_ID, all.data$shopping_pt,sep="_"))

##------------------------------------------------------------------
## Create some initial ratios
##------------------------------------------------------------------
all.data$age_ratio  <- (all.data$age_oldest / all.data$age_youngest)

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
    ## Explore zero-variance, filtering, scaling
    ##------------------------------------------------------------------

    ## check for zero variance (!!! none detected !!!)
    ##nzv <- nearZeroVar(all.data)

    ## estimate Box-Cox transformations for numeric variables
    preProc <- preProcess(all.data[,c("age_youngest", "age_oldest", "age_ratio", "cost", "car_age")], method=c("BoxCox"))
    all.data[,c("age_youngest.bc", "age_oldest.bc", "age_ratio.bc", "cost.bc", "car_age.bc")] <- predict(preProc, all.data[,c("age_youngest", "age_oldest", "age_ratio", "cost", "car_age")])

    ## override the car_age.bc xform (as there was no xform applied)
    ##all.data$car_age.bc <- log10(all.data$car_age + 1)

##------------------------------------------------------------------
## Assign state factors a numeric code
##------------------------------------------------------------------
#tmp.state   <- as.character(all.data$state)
#ref.state   <- c(state.abb, "DC")
#num.state   <- as.vector(unlist(sapply(tmp.state, function(x){which(ref.state == x)})))

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
        if (tmp.col == "car_value") {
            all.copy[, new.col] <- as.vector(unlist(tapply(as.character(all.copy[,tmp.col]), all.copy$customer_ID, FUN=replaceBads)))
        } else {
            all.copy[, new.col] <- as.vector(unlist(tapply(all.copy[,tmp.col], all.copy$customer_ID, FUN=replaceBads)))
        }
    }

##------------------------------------------------------------------
## Append variables
##------------------------------------------------------------------
all.copy$time.num       <- num.min

##------------------------------------------------------------------
## Create additional variables
##------------------------------------------------------------------

## create a last-observation flag for each customer
num.cust                     <- length(all.copy$customer_ID)
last.idx                     <- unlist( lapply(split(1:num.cust, all.copy$customer_ID), tail, 1) )
all.copy$last_fl             <- as.integer(0)
all.copy$last_fl[last.idx]   <- 1

## create day-fractions [subset(all.copy, customer_ID %in% c(10149906,10151837))]
all.copy$dayfrac.nrm         <- num.min / (24*60)

## create a key & number of unique days spent shopping per customer
all.copy$custday_key         <- paste(all.copy$customer_ID, all.copy$day, sep="_")
all.copy$custday_key.u       <- as.vector(unlist(tapply(all.copy$custday_key, all.copy$customer_ID, numUnique)))

## estimate the cumulative time spent shopping for plans
##  - compute a period-over-period elapsed time, but ...
##  - ... correct the elapsed time s/t the first observation is 0 and any
##    negative values are set to zero (we might observe a negative elapsed
##    time b/c a late shopping point occured on the say "day of the week"
##    but at an earlier time-of-day
all.copy$dayfrac.diff   <- c(0, diff(all.copy$dayfrac.nrm))
all.copy$dayfrac.diff[ which(all.copy$shopping_pt == 1) ] <- 0
all.copy$dayfrac.diff[ (all.copy$dayfrac.diff < 0) ] <- 0
all.copy$dayfrac.cum    <- as.vector(unlist(tapply(all.copy$dayfrac.diff, all.copy$customer_ID, function(x){cumsum(x)})))

## period-over-period cost differences (by customer_ID)
all.copy$dcost  <- as.vector(unlist(tapply(all.copy$cost, all.copy$customer_ID, calcDiff)))

## cumulative cost differences (by customer_ID)
all.copy$ccost  <- as.vector(unlist(tapply(all.copy$dcost, all.copy$customer_ID, function(x){cumsum(x)})))

## ratio of current cost to minimum/maximum quoted cost (by customer_ID)
all.copy$rmin    <- as.vector(unlist(tapply(all.copy$cost, all.copy$customer_ID, function(x){x/min(x,na.rm=TRUE)})))
all.copy$rmax    <- as.vector(unlist(tapply(all.copy$cost, all.copy$customer_ID, function(x){x/max(x,na.rm=TRUE)})))
all.copy$rmin.bc <- predict(BoxCoxTrans(all.copy$rmin), all.copy$rmin)      ## Box-Cox transform of skewed ratios
all.copy$rmax.bc <- predict(BoxCoxTrans(all.copy$rmax), all.copy$rmax)      ## Box-Cox transform of skewed ratios

## [parallel] period-over-period change in each of the selection options
tmp.res <- foreach(i=1:7, .combine='cbind') %dopar% {
    as.vector(unlist(tapply(all.copy[,LETTERS[i]], all.copy[,c("customer_ID")], calcDiff)))
}
all.copy[, paste("d",LETTERS[1:7],sep="")] <- tmp.res

## [parallel] rolling total of period-over-period change in each of the selection options
tmp.res <- foreach(i=1:7, .combine='cbind') %dopar% {
    as.vector(unlist(tapply(all.copy[,paste("d",LETTERS[i],sep="")], all.copy[,c("customer_ID")], function(x){cumsum((x!=0))} )))
}
all.copy[, paste("n",LETTERS[1:7],sep="")] <- tmp.res

## identify cases where "updates"/"changes" (per customer) occur
#all.copy$day.u               <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$day, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$group_size.u        <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$group_size, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$homeowner.u         <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$homeowner, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$car_age.u           <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$car_age, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$age_oldest.u        <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$age_oldest, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$age_youngest.u      <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$age_youngest, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$married_couple.u    <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$married_couple, all.copy$customer_ID, numUnique))) > 1, 1, 0))
## use scrubbed values for these variables
#all.copy$car_value.u         <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$car_value.r, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$location.u          <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$location.r, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$C_previous.u        <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$C_previous.r, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$duration_previous.u <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$duration_previous.r, all.copy$customer_ID, numUnique))) > 1, 1, 0))
#all.copy$risk_factor.u       <- as.integer(ifelse(as.vector(unlist(tapply(all.copy$risk_factor.r, all.copy$customer_ID, numUnique))) > 1, 1, 0))

##------------------------------------------------------------------
## Re-code variables with missings
##------------------------------------------------------------------
all.copy$risk_factor.r[ which(all.copy$risk_factor.r == -9) ]               <- 99
all.copy$C_previous.r[ which(all.copy$C_previous.r == -9) ]                 <- 99
all.copy$duration_previous.r[ which(all.copy$duration_previous.r == -9) ]   <- 99
all.copy$location.r[ which(all.copy$location.r == -9) ]                     <- 99999
all.copy$car_value.r[ which(all.copy$car_value.r == "z") ]                   <- c("e")   ## median replace this outlier

##------------------------------------------------------------------
## <X> Treat location as a "market-size" variable based on the number
##     of times the location appears in the data
##------------------------------------------------------------------

## create a cumulative distribution of locations
market.tbl <- table(all.copy$location.r[ which(all.copy$location.r != 99999) ])
market.cum <- cumsum(sort(market.tbl))/sum(market.tbl)

## split the locations into twentieths
market.list <- list()
market.seq  <- seq(0, 1, 0.05)
for (i in 2:length(market.seq)) {
    if ( i == 2 ) {
        market.list[[i-1]]  <- names(market.cum)[ (market.cum <= market.seq[i]) ]
    } else {
        market.list[[i-1]]  <- names(market.cum)[ ((market.cum > market.seq[i-1]) & (market.cum <= market.seq[i])) ]
    }
}

## translate locations into names, then quintile proxies
tmp.loc     <- as.character(all.copy$location.r)
tmp.fac     <- rep(0, length(tmp.loc))
for (i in 1:length(market.list)) {
    tmp.fac[ which(tmp.loc %in% market.list[[i]]) ] <- i
}

## median replace the missing locations
tmp.fac[ which(tmp.loc %in% "99999") ] <- median(tmp.fac)

## return the re-coded data to the main dataframe
all.copy$loc <- as.factor(tmp.fac)


##------------------------------------------------------------------
## <Y> Test Box-Cox on the time varibales
##------------------------------------------------------------------
## no effect
#preProc.v2 <- preProcess(all.copy[,c("dayfrac.diff", "dayfrac.cum")], method=c("BoxCox"))
#all.copy[,c("dayfrac.diff.bc", "dayfrac.cum.bc")] <- predict(preProc.v2, all.copy[,c("dayfrac.diff", "dayfrac.cum")])

##------------------------------------------------------------------
## <Y> Translate n[A-G] vectors into binary; then drop
##------------------------------------------------------------------
all.copy$uA <- as.factor(ifelse(all.copy$nA == 0, 0, 1));  all.copy$nA <- NULL
all.copy$uB <- as.factor(ifelse(all.copy$nB == 0, 0, 1));  all.copy$nB <- NULL
all.copy$uC <- as.factor(ifelse(all.copy$nC == 0, 0, 1));  all.copy$nC <- NULL
all.copy$uD <- as.factor(ifelse(all.copy$nD == 0, 0, 1));  all.copy$nD <- NULL
all.copy$uE <- as.factor(ifelse(all.copy$nE == 0, 0, 1));  all.copy$nE <- NULL
all.copy$uF <- as.factor(ifelse(all.copy$nF == 0, 0, 1));  all.copy$nF <- NULL
all.copy$uG <- as.factor(ifelse(all.copy$nG == 0, 0, 1));  all.copy$nG <- NULL


##------------------------------------------------------------------
## <X> Drop the replaced variables
##------------------------------------------------------------------
all.copy$location               <- NULL
all.copy$location.r             <- NULL
all.copy$risk_factor            <- NULL
all.copy$C_previous             <- NULL
all.copy$duration_previous      <- NULL
all.copy$car_value              <- NULL

##------------------------------------------------------------------
## <X> Drop the transformed variables
##------------------------------------------------------------------
all.copy$car_age        <- NULL
all.copy$age_youngest   <- NULL
all.copy$age_oldest     <- NULL
all.copy$age_ratio      <- NULL
all.copy$cost           <- NULL

##------------------------------------------------------------------
## <X> Drop other intermediate variables
##------------------------------------------------------------------
all.copy$rmin   <- NULL
all.copy$rmax   <- NULL

##------------------------------------------------------------------
## <X> Additional drops
##------------------------------------------------------------------
all.copy$time        <- NULL
all.copy$time.num    <- NULL
#all.copy$cost.bc     <- NULL
all.copy$custday_key <- NULL

##------------------------------------------------------------------
## Perform type conversions for factors
##------------------------------------------------------------------
factor.list <- c(   c("day", "state", "group_size", "homeowner", "married_couple"),
                    LETTERS[1:7],
                    c("location.r", "risk_factor.r", "C_previous.r", "car_value.r"),
                    paste("d",LETTERS[1:7],sep=""))

all.copy  <- convert.magic(all.copy, factor.list, rep("factor", length(factor.list)))

##------------------------------------------------------------------
## make a copy and then drop columns
##------------------------------------------------------------------
all.copy.orig   <- all.copy

##------------------------------------------------------------------
##drop superfluous levels
##------------------------------------------------------------------
all.copy    <- droplevels(all.copy)

##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.na, all.bl, all.copy, file="Y002_allstateRawData.Rdata")











