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
    preProc <- preProcess(all.data[,c("car_age", "age_youngest", "age_oldest", "cost")], method=c("BoxCox"))
    all.data[,c("car_age.bc", "age_youngest.bc", "age_oldest.bc", "cost.bc")] <- predict(preProc, all.data[,c("car_age", "age_youngest", "age_oldest", "cost")])

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
        if (tmp.col == "car_value") {
            all.copy[, new.col] <- as.vector(unlist(tapply(as.character(all.copy[,tmp.col]), all.copy$customer_ID, FUN=replaceBads)))
        } else {
            all.copy[, new.col] <- as.vector(unlist(tapply(all.copy[,tmp.col], all.copy$customer_ID, FUN=replaceBads)))
        }
    }

##------------------------------------------------------------------
## Replace scrubbed car_value factors with a numeric code
##------------------------------------------------------------------

## first do a simple replacement
tmp.ch          <- as.character(all.copy$car_value.r)
tmp.ch          <- ifelse(tmp.ch == "", "z", tmp.ch)       ## encode remaining blanks as "z"
num.car_value   <- as.vector(unlist(sapply(tmp.ch, function(x){which(letters==x)})))

## there are still some cars in the test data (only) with "z" ... median replace those
num.car_value[num.car_value == 26] <- median(num.car_value[num.car_value != 26])

##------------------------------------------------------------------
## Bin car ages into a decile factor
##------------------------------------------------------------------
car_age.decile  <- cut(all.copy$car_age, breaks=quantile(all.copy$car_age, probs=seq(0, 1, 0.1)), include.lowest=TRUE)

##------------------------------------------------------------------
## Append variables
##------------------------------------------------------------------
all.copy$car_value.num  <- num.car_value
all.copy$car_age.bin    <- car_age.decile
all.copy$state.num      <- num.state
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

## estimated the cumulative time spent shopping for plans
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

## ratio of current cost to minimum quoted cost (by customer_ID)
all.copy$rmin    <- as.vector(unlist(tapply(all.copy$cost, all.copy$customer_ID, function(x){x/min(x,na.rm=TRUE)})))
all.copy$rmin.bc <- predict(BoxCoxTrans(all.copy$rmin), all.copy$rmin)      ## Box-Cox transform of skewed ratios

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
all.copy$day.u               <- ifelse(as.vector(unlist(tapply(all.copy$day, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$group_size.u        <- ifelse(as.vector(unlist(tapply(all.copy$group_size, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$homeowner.u         <- ifelse(as.vector(unlist(tapply(all.copy$homeowner, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$car_age.u           <- ifelse(as.vector(unlist(tapply(all.copy$car_age, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$age_oldest.u        <- ifelse(as.vector(unlist(tapply(all.copy$age_oldest, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$age_youngest.u      <- ifelse(as.vector(unlist(tapply(all.copy$age_youngest, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$married_couple.u    <- ifelse(as.vector(unlist(tapply(all.copy$married_couple, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$car_value.u         <- ifelse(as.vector(unlist(tapply(all.copy$car_value.num, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$location.u          <- ifelse(as.vector(unlist(tapply(all.copy$location, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$C_previous.u        <- ifelse(as.vector(unlist(tapply(all.copy$C_previous, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$duration_previous.u <- ifelse(as.vector(unlist(tapply(all.copy$duration_previous, all.copy$customer_ID, numUnique))) > 1, 1, 0)
all.copy$risk_factor.u       <- ifelse(as.vector(unlist(tapply(all.copy$risk_factor, all.copy$customer_ID, numUnique))) > 1, 1, 0)

##------------------------------------------------------------------
## Re-code variables with missings
##------------------------------------------------------------------
all.copy$risk_factor.r[ which(all.copy$risk_factor.r == -9) ]   <- 5
all.copy$location.r[ which(all.copy$location.r == -9) ]         <- 99999

##------------------------------------------------------------------
## Normalize variables
##------------------------------------------------------------------
all.copy$car_age.s           <- scale(all.copy$car_age)
all.copy$age_oldest.s        <- scale(all.copy$age_oldest)
all.copy$age_youngest.s      <- scale(all.copy$age_youngest)
all.copy$age_oldest.bcs      <- scale(all.copy$age_oldest.bc)       ## scaled box-cox transformed cost
all.copy$age_youngest.bcs    <- scale(all.copy$age_youngest.bc)     ## scaled box-cox transformed cost
all.copy$duration_previous.s <- scale(all.copy$duration_previous.r)
all.copy$dayfrac.cum.s       <- scale(all.copy$dayfrac.cum)
all.copy$dayfrac.nrm.s       <- scale(all.copy$dayfrac.nrm)

all.copy$cost.s              <- scale(all.copy$cost)     ## scaled cost
all.copy$cost.bcs            <- scale(all.copy$cost.bc)  ## scaled box-cox transformed cost
all.copy$dcost.s             <- scale(all.copy$dcost)    ## scaled change in cost from prior choice
all.copy$ccost.s             <- scale(all.copy$ccost)    ## scaled cumulative change in cost from first choice
all.copy$rmin.s              <- scale(all.copy$rmin)     ## scaled ratio of cost / min(cost)
all.copy$rmin.bcs            <- scale(all.copy$rmin.bc)  ## scaled box-cox transformed ratio of cost / min(cost)

##------------------------------------------------------------------
## Perform type conversions for factors
##------------------------------------------------------------------
factor.list <- c(   c("day", "group_size", "homeowner", "married_couple"),
                    c("location.r", "risk_factor.r", "C_previous.r"),
                    c("car_value.num", "state.num"),
                    LETTERS[1:7],
                    paste("d",LETTERS[1:7],sep=""))

all.copy  <- convert.magic(all.copy, factor.list, rep("factor", length(factor.list)))

##------------------------------------------------------------------
## Drop backfilled/superfluous columns
##------------------------------------------------------------------
drop.cols   <- c(   "time", "state", "location", "car_age", "car_value", "risk_factor",
                    "age_oldest", "age_youngest", "C_previous", "duration_previous",
                    "cost", "car_value.r", "duration_previous.r",
                    "time.num", "custday_key", "dayfrac.diff", "dayfrac.nrm",
                    "dayfrac.cum", "dcost", "ccost", "rmin")

## make a copy and then drop columns
all.copy.orig   <- all.copy
all.copy        <- all.copy.orig[ , -which(colnames(all.copy.orig) %in% drop.cols) ]

##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.na, all.bl, all.copy, all.copy.orig, file="002_allstateRawData.Rdata")











