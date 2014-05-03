##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Create the raw materials for a panel dataset to be used in a
##     regression model for the insurance purchase targets
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(Hmisc)
library(foreach)
library(doMC)
library(plyr)

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
## Load data
##------------------------------------------------------------------
load("X002_allstateRawData.Rdata"); rm("all.bl", "all.na", "all.copy.orig")

##******************************************************************
## Step 1:  Scale combined data (at each shopping_pt stage)
##******************************************************************
all.bkup      <- all.copy
num.sp        <- unique(all.copy$shopping_pt)

## loop over each shopping_pt
for (i in num.sp) {
    
        ## isolate shopping_pt data
        tmp.idx     <- which(all.copy$shopping_pt == i)
        tmp.data    <- all.copy[ tmp.idx, ]
        
        ## scale numeric variables
        tmp.data$car_age.bcs           <- scale(tmp.data$car_age.bc)
        tmp.data$age_youngest.bcs      <- scale(tmp.data$age_youngest.bc)
        tmp.data$age_oldest.bcs        <- scale(tmp.data$age_oldest.bc)
        tmp.data$age_ratio.bcs         <- scale(tmp.data$age_ratio.bc)
        tmp.data$duration_previous.rs  <- scale(tmp.data$duration_previous.r)
        tmp.data$dayfrac.nrms          <- scale(tmp.data$dayfrac.nrm)
        tmp.data$dayfrac.diffs         <- scale(tmp.data$dayfrac.diff)
        tmp.data$dayfrac.cums          <- scale(tmp.data$dayfrac.cum)
        tmp.data$dcost.s               <- scale(tmp.data$dcost)
        tmp.data$ccost.s               <- scale(tmp.data$ccost)
        tmp.data$rmin.bcs              <- scale(tmp.data$rmin.bc)
        tmp.data$rmax.bcs              <- scale(tmp.data$rmax.bc)

        ## name the scaled cost differently (historical reasons, see below)
        tmp.data$cost.s              <- scale(tmp.data$cost.bc)

        ## drop the prior values
        tmp.data$car_age.bc            <- NULL
        tmp.data$age_youngest.bc       <- NULL
        tmp.data$age_oldest.bc         <- NULL
        tmp.data$age_ratio.bc          <- NULL
        tmp.data$duration_previous.r   <- NULL
        tmp.data$dayfrac.nrm           <- NULL
        tmp.data$dayfrac.diff          <- NULL
        tmp.data$dayfrac.cum           <- NULL
        tmp.data$dcost                 <- NULL
        tmp.data$ccost                 <- NULL
        tmp.data$rmin.bc               <- NULL
        tmp.data$rmax.bc               <- NULL
        tmp.data$cost.bc               <- NULL
        
        ## scale the cost difference variables
        cols    <- colnames(tmp.data)[grep("^d[A-G]",colnames(tmp.data))]
        for (j in 1:length(cols)) {
            tmp.data[, eval(cols[j])] <- scale(as.numeric(tmp.data[,eval(cols[j])]))
        }
        
        ## scale the cost difference variables
        cols    <- colnames(tmp.data)[grep("^n[A-G]",colnames(tmp.data))]
        for (j in 1:length(cols)) {
            tmp.data[, eval(cols[j])] <- scale(as.numeric(tmp.data[,eval(cols[j])]))
        }
        
        ## accumulate results
        if (i == 1) {
            all.scaled  <- tmp.data
        } else {
            all.scaled  <- rbind(all.scaled, tmp.data)
        }
}

## sort the combined, scaled results
all.scaled  <- all.scaled[ order(all.scaled$customer_ID, all.scaled$shopping_pt), ]

## clean-up the results (several of the shopping_pt == 1) scaled values were uniform
all.scaled[ is.na(all.scaled$dayfrac.diffs), c("dayfrac.diffs")] <- 0
all.scaled[ is.na(all.scaled$dayfrac.cums), c("dayfrac.cums")] <- 0
all.scaled[ is.na(all.scaled$dcost.s), c("dcost.s")] <- 0
all.scaled[ is.na(all.scaled$ccost.s), c("ccost.s")] <- 0
all.scaled[ which(all.scaled$shopping_pt == 1), colnames(tmp.data)[grep("^d[A-G]",colnames(tmp.data))]] <- 0
all.scaled[ which(all.scaled$shopping_pt == 1), colnames(tmp.data)[grep("^n[A-G]",colnames(tmp.data))]] <- 0

## move the scaled data back to the original name
all.copy <- all.scaled


##------------------------------------------------------------------
## Set-up a sink
##------------------------------------------------------------------
writeLines(c(""), "construct_panel_logfile.txt")
sink("construct_panel_logfile.txt", append=TRUE)

##******************************************************************
## Step 2:  Create "wide" verions of the choice/cost history
## ... n == 0 --> loads the "test" data (57156 custmers)
## ... n == 1 --> loads the "training" data (97009 customers)
##******************************************************************
for (n in 0:1) {

    ##------------------------------------------------------------------
    ## Subset the data via id_fl (0 == test, 1 == train)
    ##------------------------------------------------------------------
    smp  <- subset(all.copy, id_fl == n)

    ##------------------------------------------------------------------
    ## Make a copy of a slim version of the choices (panel) & costs (cost)
    ##------------------------------------------------------------------
    panel <- smp[ , c(c("customer_ID", "record_type"), LETTERS[1:7])]
    cost  <- smp[ ,   c("customer_ID", "record_type", "cost.s")]

    ##------------------------------------------------------------------
    ## Create a single choice column & drop individual letters
    ##------------------------------------------------------------------
    panel$choice <- apply(panel[, LETTERS[1:7]], MARGIN=1, paste, sep="", collapse="")
    for (i in 1:7) {
        panel[,LETTERS[i]] <- NULL
    }
    
    ##------------------------------------------------------------------
    ## get the maximum number of touches in the file
    ##------------------------------------------------------------------
    tmp.freqAgg 	<- table(table(panel[,c("customer_ID")]))
    max.touch		<- as.integer(names(tmp.freqAgg)[length(tmp.freqAgg)])  ## last column

    ##------------------------------------------------------------------
    ## Create a matrix to hold the terminal and all prior choices (ch.hist)
    ##------------------------------------------------------------------
    col.headers <- paste( paste(LETTERS[1:7], sep="", collapse="") ,".T",sep="")
    for (i in 0:(max.touch-1)) {
        tmp.headers	<- paste( paste(LETTERS[1:7], sep="", collapse="") ,".",i,sep="")
        col.headers	<- c(col.headers, tmp.headers)
    }
    ch.hist <- matrix(0, nrow=nrow(panel), ncol=length(col.headers))
    colnames(ch.hist) <- col.headers
    rownames(ch.hist) <- panel[ , c("customer_ID")]

    ##------------------------------------------------------------------
    ## Create a matrix to hold the terminal and all prior costs (ch.cost)
    ##------------------------------------------------------------------
    col.headers	<- paste("cost.s",seq(0,(max.touch-1),1),sep="")
    ch.cost     <- matrix(0, nrow=nrow(panel), ncol=length(col.headers))
    colnames(ch.cost) <- col.headers
    rownames(ch.cost) <- panel[ , c("customer_ID")]

    ##------------------------------------------------------------------
    ## Load a matrix of purchase data (if the train dataset)
    ##------------------------------------------------------------------
    if (n == 1) {
        purch <- panel[ (panel[ ,c("record_type")] == 1) , ]
        purch$record_type <- NULL
    }
    
    ##------------------------------------------------------------------
    ## Remove superfluous columns before heading into the backfill procedure
    ##------------------------------------------------------------------
    cost$record_type  <- NULL
    panel$record_type <- NULL
    
    ##------------------------------------------------------------------
    ## Load the lagged choice/cost data
    ##------------------------------------------------------------------
    hist.list <- varLag(myPanel=panel, myCost=cost, myPurch=purch, myHistSkel=ch.hist, myCostSkel=ch.cost, myType=n, myClass="choice")
    cost.list <- varLag(myPanel=panel, myCost=cost, myPurch=purch, myHistSkel=ch.hist, myCostSkel=ch.cost, myType=n, myClass="cost")

    ##------------------------------------------------------------------
    ## Transform into data frames
    ##------------------------------------------------------------------
    df.cost     <- as.data.frame(do.call(rbind, cost.list))
    df.cost$key <- as.factor(rownames(df.cost))
    
    df.hist     <- as.data.frame(do.call(rbind, hist.list))
    df.hist$key <- as.factor(rownames(df.hist))
    
    ##------------------------------------------------------------------
    ## <X> explode variables separately for training and test panels
    ##------------------------------------------------------------------
    if (n == 1) {

        all.train   <- join(smp, join(df.cost, df.hist, by="key"), by="key")
        hist.train  <- df.cost
        cost.train  <- df.cost
        
        ##------------------------------------------------------------------
        ## explode the non-choice factor variables
        ##------------------------------------------------------------------
        cols <- c("day", "state", "group_size", "homeowner", "married_couple", "risk_factor.r", "C_previous.r", "car_value.r")
        for (j in 1:length(cols)) {
            tmp.mat <- expandFactors(x=all.train[, eval(cols[j])], v=eval(cols[j]))
            if (j == 1) {
                tmp.exploded <- tmp.mat
            } else {
                tmp.exploded <- cbind(tmp.exploded, tmp.mat)
            }
        }
        all.train   <- cbind(all.train, tmp.exploded)
        all.train   <- all.train[, -(which(colnames(all.train) %in% cols))]
        
        save(all.copy, all.train, hist.train, cost.train, file="X003_allstateRawData_Train.Rdata")
        
    } else {
        
        all.test    <- join(smp, join(df.cost, df.hist, by="key"), by="key")
        hist.test   <- df.hist
        cost.test   <- df.cost

        ##------------------------------------------------------------------
        ## explode the non-choice factor variables
        ##------------------------------------------------------------------
        cols <- c("day", "state", "group_size", "homeowner", "married_couple", "risk_factor.r", "C_previous.r", "car_value.r")
        for (j in 1:length(cols)) {
            tmp.mat <- expandFactors(x=all.test[, eval(cols[j])], v=eval(cols[j]))
            if (j == 1) {
                tmp.exploded <- tmp.mat
            } else {
                tmp.exploded <- cbind(tmp.exploded, tmp.mat)
            }
        }
        all.test   <- cbind(all.test, tmp.exploded)
        all.test   <- all.test[, -(which(colnames(all.test) %in% cols))]

        save(all.copy, all.test, hist.test, cost.test, file="X003_allstateRawData_Test.Rdata")
    }

} ## end of the main for-loop


##------------------------------------------------------------------
## Add some additional targets to the train file
##------------------------------------------------------------------
all.train$ne.lq <- 1*(as.character(all.train$ABCDEFG.T) != as.character(all.train$ABCDEFG.0))
for (i in 1:7) {
    all.train[ , paste(LETTERS[i],"T.ne.",LETTERS[i],"0",sep="")] <- 1*(as.character(substr(all.train$ABCDEFG.T,i,i)) != as.character(substr(all.train$ABCDEFG.0,i,i)))
}


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.copy, all.train, all.test, hist.train, cost.train, hist.test, cost.test, all.bkup, file="X003_allstateRawData.Rdata")

##------------------------------------------------------------------
## Close sink
##------------------------------------------------------------------
sink()


