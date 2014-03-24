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
## Load data
##------------------------------------------------------------------
load("005_allstateRawData_Train.Rdata")

##------------------------------------------------------------------
## Work with a copy
##------------------------------------------------------------------
smp <- all.train; rm(all.train, all.copy, ch.train)

##------------------------------------------------------------------
## Drop columns
##------------------------------------------------------------------

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
        
        row.idx     <- ( (smp$shopping_pt %in% tmp.time) & (smp$record_type != 1) )     ## load non-purchase points
    /*
     * need to rework this b/c of the new cost data
     */
        tmp.dat     <- smp[row.idx, c(1:(let.idx-1), let.idx:(let.idx+((i+1)*7)-1))]    ## load all prior decision info
        
        ## identify terminal choices to drop from this panel
        drop.let    <- paste(LETTERS[which(!(paste(LETTERS[1:7],"T",sep="") %in% paste(tmp.choice,"T",sep="")))],"T", sep="")
        
        ## drop the irrelevant terminal choices
        tmp.dat     <- tmp.dat[, -which(colnames(tmp.dat) %in% drop.let) ]
        
        ## drop the original "letter" variables b/c they're included elsewhere
        tmp.dat     <- tmp.dat[, -which(colnames(tmp.dat) %in% LETTERS[1:7])]
        
        ## drop redundant columns
        tmp.dat     <- tmp.dat[, -which(colnames(tmp.dat) %in% c("record_type"))]
        
        ## define the panel ID
        panel_id    <- paste(   tmp.choice,
                                ifelse(tmp.time < 10, paste("0",tmp.time,sep=""), tmp.time),
                                sep="_")
                                
        ## define the panel ID
        tmp.term    <- paste(tmp.choice, "T", sep="")
        #tmp.colidx  <- which( colnames(tmp.dat) %in% c("customer_ID", "shopping_pt", tmp.term) )
                                
        ## load the raw data
        panel.list[[panel_id]]$term <- tmp.term
        panel.list[[panel_id]]$data <- tmp.dat
        panel.list[[panel_id]]$len  <- nrow(tmp.dat)
 
    }
}

##------------------------------------------------------------------
## Write the full panel to an .Rdata file
##------------------------------------------------------------------
save(panel.list, file="006_allstatePanelData_Train.Rdata")

##------------------------------------------------------------------
## Write individual panels to separate .Rdata files
##------------------------------------------------------------------
panel.names <- names(panel.list)

for (i in 1:length(panel.names)) {
    tmp.panel       <- panel.names[i]
    tmp.filename    <- paste("./panels/006_allstatePanelData_Train.",tmp.panel,".Rdata",sep="")
    tmp.object      <- panel.list[[tmp.panel]]
    save(tmp.object, file=tmp.filename)
}


