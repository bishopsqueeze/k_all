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
## <function> :: expandFactors
##------------------------------------------------------------------
expandFactors   <- function(x, v="v") {
    
    n       <- nlevels(x)
    lvl     <- levels(x)
    mat     <- matrix(, nrow=length(x), ncol=n)
    tmp.v   <- c()
    
    for (i in 1:n) {
        tmp.lvl <- lvl[i]
        tmp.v   <- c(tmp.v, paste(v,".L",tmp.lvl,sep=""))
        mat[,i] <- as.integer((x == tmp.lvl))
    }
    colnames(mat) <- tmp.v
    
return(mat)
}


##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## One loop for test data (0) and one loop for training data (1)
##------------------------------------------------------------------
#for (n in 0:1) {
for (n in 1:1) {
    
    ##------------------------------------------------------------------
    ## Load data (each should contain an object called "panel.list"
    ##------------------------------------------------------------------
    if (n == 0) {
        load("004_allstatePanelData_Test.Rdata")
    } else {
        load("004_allstatePanelData_Train.Rdata")
    }
    
    ## loop over all of the shopping point panels (SP_02 ... SP_11)
    ##for (i in 2:11) {
    for (i in 11:11) {
        
        tmp.data    <- panel.list[[i]]$data
        tmp.len     <- panel.list[[i]]$len
        tmp.sp      <- panel.list[[i]]$sp
        
        ## remove the multi-choice columns
        tmp.data    <- tmp.data[ , -grep("^AF", colnames(tmp.data)) ]
        tmp.data    <- tmp.data[ , -grep("^BE", colnames(tmp.data)) ]
        tmp.data    <- tmp.data[ , -grep("^CD", colnames(tmp.data)) ]
        tmp.data    <- tmp.data[ , -grep("^ABCDEFG", colnames(tmp.data)) ]
        
        
        ## expand factors into a set of binary variables
    }


}

