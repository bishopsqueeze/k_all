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

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## <function> :: varLag
##------------------------------------------------------------------
varLag <- function(myPanel, myCost, myPurch, myHistSkel, myCostSkel, myType=NULL, myClass=NULL) {
    
    ## identify unique customers
    uniq.cust 	<- unique(myPanel[ ,c("customer_ID")])
    num.cust 	<- length(uniq.cust)
    all.letters <- paste(LETTERS[1:7],sep="",collapse="")
    
    ## loop over all the custmomers and populate a matrix with prior choices
    tmp.list <- foreach (i=1:num.cust, .inorder=FALSE) %dopar% {
        
        ## report progress
        if ((i %% 1000) == 0) { cat("Iteration = ", i, "\n") }
        
        ## isolate the rows for each customer
        row.idx <- which(myPanel[, c("customer_ID")] == uniq.cust[i])
        num.idx <- length(row.idx)
        
        ## isolate the terminal data for each customer
        if (myType == 1) {
            tmp.purch <- myPurch[ (myPurch[ , c("customer_ID")] == uniq.cust[i]) , ]
        }
        
        ## isolate the relevant slice of data
        if (myClass == "cost") {
            tmp.smp     <- myCost[ row.idx, ]
            tmp.skel    <- myCostSkel[ row.idx, ]
        } else if (myClass == "choice") {
            tmp.smp     <- myPanel[ row.idx, ]
            tmp.skel    <- myHistSkel[ row.idx, ]
        }
        
        ## loop over each historical row and populate a wide matrix
        for (j in 1:num.idx) {
            
            ## populate the terminal data (for train data only)
            if ( (myType == 1) & (myClass == "choice") & (j == 1)) {
                tmp.skel[, paste(all.letters,".T",sep="")] <- tmp.purch$choice
            }
            
            ## create the lagged histories
            if ( (myClass == "choice") ) {
                tmp.skel[, paste(all.letters,".",j-1,sep="")] <- Lag(tmp.smp$choice, shift=j-1)
            } else {
                tmp.skel[, paste("cost.s",j-1,sep="")]        <- Lag(tmp.smp$cost.s, shift=j-1)
            }
            
        }
        rownames(tmp.skel) <- paste(rownames(tmp.skel),1:nrow(tmp.skel),sep="_")
        
        ## return a the populated skeleton to the %dopar% routine
        tmp.skel
    }
    
    return(tmp.list)
}

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

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
for (n in 1:1) {
#for (n in 0:1) {

    ##------------------------------------------------------------------
    ## Subset the data via id_fl (0 == test, 1 == train)
    ##------------------------------------------------------------------
    all.copy$key    <- paste(all.copy$customer_ID, all.copy$shopping_pt, sep="_")   ## move this to a data load stage
    smp             <- subset(all.copy, id_fl == n)

    ##------------------------------------------------------------------
    ## Make a copy of a slim version of the choices (panel) & costs (cost)
    ##------------------------------------------------------------------
    panel <- smp[ , c(c("customer_ID", "record_type"), LETTERS[1:7])]
    cost  <- smp[ ,   c("customer_ID", "record_type", "cost.s")]    ## propagate the scaled cost

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
    }
    
    ##------------------------------------------------------------------
    ## Load the lagged choice/cost data
    ##------------------------------------------------------------------
    hist.list <- varLag(myPanel=panel, myCost=cost, myPurch=purch, myHistSkel=ch.hist, myCostSkel=ch.cost, myType=n, myClass="choice")
    cost.list <- varLag(myPanel=panel, myCost=cost, myPurch=purch, myHistSkel=ch.hist, myCostSkel=ch.cost, myType=n, myClass="cost")

    ##------------------------------------------------------------------
    ## Transform into data frames
    ##------------------------------------------------------------------
    df.cost     <- as.data.frame(do.call(rbind, cost.list))
    df.cost$key <- rownames(df.cost)
    
    df.hist     <- as.data.frame(do.call(rbind, hist.list))
    df.hist$key <- rownames(df.hist)
    
    ##------------------------------------------------------------------
    ## write separate training and test panels
    ##------------------------------------------------------------------
    #  if (n == 1) {
    #    ## join instead all.train   <- cbind(smp, df.cost, df.hist)
    #    hist.train  <- ch.hist
    #    cost.train  <- ch.cost
    #    #save(all.copy, all.train, hist.train, cost.train, file="005_allstateRawData_Train.Rdata")
    #} else {
    #    all.test    <- cbind(smp, df.cost, df.hist)
    #    hist.test     <- ch.hist
    #    cost.test     <- ch.cost
    #    #        save(all.copy, all.test, hist.test, cost.test, file="005_allstateRawData_Test.Rdata")
    #}

} ## end of the main for-loop


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
##save(all.copy, all.train, all.test, hist.train, cost.train, hist.test, cost.test ,file="005_allstateRawData.Rdata")

##------------------------------------------------------------------
## Close sink
##------------------------------------------------------------------
sink()

