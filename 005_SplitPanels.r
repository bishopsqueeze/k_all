##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Create the raw materials for a panel dataset to be used in a
##     regression model for the insurance purchase targets
##  2. This script will create a separate panel for each combination
##     of shopping_pt (1, 2, 3, ... , N) and choice (A, B, ... , G).
##  3. Terminal (purchase) rows are not included in the panel
##  4. What we end-up with is the entire set of observations from
##     a particular shopping_pt.
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
## One loop for test data (0) and one loop for training data (1)
##------------------------------------------------------------------
for (n in 0:1) {
    
    ##------------------------------------------------------------------
    ## Load data
    ##------------------------------------------------------------------
    if (n == 0) {
        load("005_allstateRawData_Test.Rdata")
        smp <- all.test; rm(all.test, all.copy, cost.test, hist.test)
    } else {
        load("005_allstateRawData_Train.Rdata")
        smp <- all.train; rm(all.train, all.copy, cost.train, hist.train)
    }

    ##------------------------------------------------------------------
    ## Isolate the number of timesteps and choices
    ##------------------------------------------------------------------
    tvec    <- unique(smp$shopping_pt)  ## timesteps
    tnum    <- length(tvec)

    cvec    <- LETTERS[1:7]             ## choices
    cnum    <- length(cvec)

    ##------------------------------------------------------------------
    ## Create a placeholder list
    ##------------------------------------------------------------------
    panel.list  <- list()

    ##------------------------------------------------------------------
    ## Location of the first (of N) choice columns -- this script presumes
    ## that all of prior choice variables lie to the right of this column
    ##------------------------------------------------------------------
    let.idx     <- which(colnames(smp) == "AT")
    cost.idx    <- which(colnames(smp) == "cost0")

    ##------------------------------------------------------------------
    ## Loop over the time/choice combinations and load a separate panel
    ## for each combination [e.g., (shopping_pt == 2) & (the "F" option)]
    ##------------------------------------------------------------------
    for (i in 1:11) {
        cat("Iteration ",i, " ... of 11 \n")
        
        for (j in 1:cnum) {
            
            tmp.time    <- tvec[i]  ## the shopping_pt (1, 2, 3, ...)
            tmp.choice  <- cvec[j]  ## the option (A, B, C, ...)
        
            ## load non-purchase points
            row.idx     <- ( (smp$shopping_pt %in% tmp.time) & (smp$record_type != 1) )
        
            ## add the new cost data (i.e., all prior decision info)
            tmp.dat     <- smp[row.idx, c(1:(cost.idx-1),  (cost.idx):(cost.idx+i-1), let.idx:(let.idx+((i+1)*7)-1))]
        }
    
        ## define the panel ID
        panel_id <- paste("SP_",ifelse(tmp.time < 10, paste("0",tmp.time,sep=""),tmp.time),sep="")
                
        ## load the raw data into a list
        panel.list[[panel_id]]$sp   <- panel_id
        panel.list[[panel_id]]$data <- droplevels(tmp.dat)  ## drop superfluous levels
        panel.list[[panel_id]]$len  <- nrow(tmp.dat)
 
    }

    ##------------------------------------------------------------------
    ## Write the full panel to an .Rdata file
    ##------------------------------------------------------------------
    if (n == 0) {
        save(panel.list, file="006_allstatePanelData_Test.Rdata")
    } else {
        save(panel.list, file="006_allstatePanelData_Train.Rdata")
    }

    ##------------------------------------------------------------------
    ## Write individual panels to separate .Rdata files
    ##------------------------------------------------------------------
    panel.names <- names(panel.list)

    for (i in 1:length(panel.names)) {
        tmp.panel       <- panel.names[i]
        if (n == 0) {
            tmp.filename    <- paste("./panels/006_allstatePanelData_Test.",tmp.panel,".Rdata",sep="")
        } else {
            tmp.filename    <- paste("./panels/006_allstatePanelData_Train.",tmp.panel,".Rdata",sep="")
        }
        tmp.object      <- panel.list[[tmp.panel]]
        save(tmp.object, file=tmp.filename)
    }
    
} ## end of train/test loop


