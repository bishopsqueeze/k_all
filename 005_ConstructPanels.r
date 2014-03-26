##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Create the raw materials for a panel dataset to be used in a
##     regression model for the insurance purchase targets
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
## Load data
##------------------------------------------------------------------
load("002_allstateRawData.Rdata"); rm("all.bl", "all.na", "all.copy.orig")

##------------------------------------------------------------------
## Load the panel data
##------------------------------------------------------------------

for (n in 0:1) {
##for (n in 1:1) {
    
    ##------------------------------------------------------------------
    ## Subset the data via id_fl (0 == test, 1 == train)
    ##------------------------------------------------------------------
    smp <- subset(all.copy, id_fl == n)

    ##------------------------------------------------------------------
    ## Make a copy of a slim version of the data (panel)
    ##------------------------------------------------------------------
    panel <- as.matrix(smp[ , c(c("customer_ID", "record_type"), LETTERS[1:7])])
    cost  <- as.matrix(smp[ ,   c("customer_ID", "record_type", "cost")])

    ##------------------------------------------------------------------
    ## get the maximum number of touches in the file
    ##------------------------------------------------------------------
    tmp.freqAgg 	<- table(table(panel[,c("customer_ID")]))
    max.touch		<- as.integer(names(tmp.freqAgg)[length(tmp.freqAgg)])  ## last column

    ##------------------------------------------------------------------
    ## Create a matrix to hold the terminal and all prior choices (ch.hist)
    ##------------------------------------------------------------------
    col.headers <- paste(LETTERS[1:7],"T",sep="")

    for (i in 0:(max.touch-1)) {
        tmp.headers	<- paste(LETTERS[1:7],i,sep="")
        col.headers	<- c(col.headers, tmp.headers)
    }

    ch.hist <- matrix(0, nrow=nrow(panel), ncol=length(col.headers))
    colnames(ch.hist) <- col.headers
    rownames(ch.hist) <- panel[ , c("customer_ID")]

    ##------------------------------------------------------------------
    ## Create a matrix to hold the terminal and all prior costs (ch.cost)
    ##------------------------------------------------------------------
    col.headers	<- paste("cost",seq(0,(max.touch-1),1),sep="")
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
    ## Load the panels -- Slow even though all matrix manipulations
    ##------------------------------------------------------------------

    ## identify unique customers
    uniq.cust 	<- unique(panel[ ,c("customer_ID")])
    num.cust 	<- length(uniq.cust)
    col.letters <- LETTERS[1:7]
    col.numbers <- 1:7

    ## track runtime
    #system.time({
    
    ## loop over all the custmomers and populate the choice history
    for (i in 1:num.cust) {

        ## isolate the rows for each customer
        row.idx <- which(panel[ , c("customer_ID")] == uniq.cust[i])
	
        ## isolate that slice of data
        tmp.smp	<- panel[ row.idx, ]
        tmp.amt <- cost[ row.idx, ]
        num.smp	<- nrow(tmp.smp)
	
        ## isolate the terminal data for each customer
        if (n == 1) {
            tmp.purch <- purch[ (purch[ , c("customer_ID")] == uniq.cust[i]) , ]
        }
    
        ## grab the choice history matrix
        tmp.hist  <- ch.hist[ row.idx, ]
        tmp.cost  <- ch.cost[ row.idx, ]
    
        ## loop over each row in the choice history.  for that row,
        ## load the terminal and all prior choice observations
        for (j in 1:num.smp) {
	
            ## populate the terminal data (for train data only)
            if (n == 1) {
                tmp.hist[j, col.numbers] <- tmp.purch[(col.numbers+2)]
            }
        
            ## walk back from current obs. and populate prior observations
            for (k in 1:j) {
                tmp.hist[j, (7*k + col.numbers)] <- tmp.smp[(j-(k-1)), (col.numbers+2)]
                tmp.cost[j, k]                   <- tmp.amt[(j-(k-1)), 3]
            }
        }
        ch.hist[ row.idx, ] <- tmp.hist
        ch.cost[ row.idx, ] <- tmp.cost
	
        if ((i %% 1000) == 0) { cat("Iteration = ", i, "\n") }
    }
    #}) ## end of system.time


    ##------------------------------------------------------------------
    ## combine the augmeneted data into a single panel
    ##------------------------------------------------------------------
    df.hist             <- as.data.frame(ch.hist)
    df.cost             <- as.data.frame(ch.cost)
    rownames(df.hist)   <- NULL
    rownames(df.cost)   <- NULL
    
    ##------------------------------------------------------------------
    ## write separate training and test panels
    ##------------------------------------------------------------------
    if (n == 1) {
        all.train   <- cbind(smp, df.cost, df.hist)
        hist.train  <- ch.hist
        cost.train  <- ch.cost
        save(all.copy, all.train, hist.train, cost.train, file="005_allstateRawData_Train.Rdata")
    } else {
        all.test    <- cbind(smp, df.cost, df.hist)
        hist.test     <- ch.hist
        cost.test     <- ch.cost
        save(all.copy, all.test, hist.test, cost.test, file="005_allstateRawData_Test.Rdata")
    }

} ## end of the main for-loop


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(all.copy, all.train, all.test, hist.train, cost.train, hist.test, cost.test ,file="005_allstateRawData.Rdata")




