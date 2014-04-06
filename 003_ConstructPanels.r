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
load("002_allstateRawData.Rdata"); rm("all.bl", "all.na", "all.copy.orig")

##------------------------------------------------------------------
## Set-up a sink
##------------------------------------------------------------------
writeLines(c(""), "construct_panel_logfile.txt")
sink("construct_panel_logfile.txt", append=TRUE)

##------------------------------------------------------------------
## Create "wide" verions of the choice/cost history
## ... n == 0 --> loads the "test" data (57156 custmers)
## ... n == 1 --> loads the "training" data (97009 customers)
##------------------------------------------------------------------
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
    ## Clean residual blanks/NAs (doesn't really matter for later panels)
    ##------------------------------------------------------------------
    
    ##------------------------------------------------------------------
    ## write separate training and test panels
    ##------------------------------------------------------------------
    if (n == 1) {
        #all.train   <- cbind(smp, new.cost, new.hist)
        all.train   <- join(smp, join(df.cost, df.hist, by="key"), by="key")
        hist.train  <- df.cost
        cost.train  <- df.cost
        save(all.copy, all.train, hist.train, cost.train, file="003_allstateRawData_Train.Rdata")
    } else {
        #all.test    <- cbind(smp, new.cost, new.hist)
        all.test    <- join(smp, join(df.cost, df.hist, by="key"), by="key")
        hist.test   <- df.hist
        cost.test   <- df.cost
        save(all.copy, all.test, hist.test, cost.test, file="003_allstateRawData_Test.Rdata")
    }

} ## end of the main for-loop


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.copy, all.train, all.test, hist.train, cost.train, hist.test, cost.test ,file="003_allstateRawData.Rdata")

##------------------------------------------------------------------
## Close sink
##------------------------------------------------------------------
sink()

