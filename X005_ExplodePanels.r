##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())


###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### The goal here should be to
### [1] explode the choice parameters
### [2] compute interactions between choices ?
### [3] use caret to identify near-zero variables
### [4] use caret to identify linear combinations of variables
### [5] ensure that the training and test data have identical set
###     of variables ... dropping those that are mismatched
### [6] ensure that everything in the dataset can be expressed as
###     a numeric/integer variables
### [7] confirm that the format is generally consistent with what
###     is described in the predictive analytics paper
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


### ??? maybe load these together to get a sense for what truly
###     is a near-zero variable

### STEP 0 -- remove superfluous variables (now that we have panels ... e.g., ABCDEFG)
### STEP 1 -- remove near-zero variables (to reduce dimensions)
### STEP 2 --
##------------------------------------------------------------------
## Load data (each should contain an object called "panel.list"
##------------------------------------------------------------------
if (n == 0) {
    load("X004_allstatePanelData_Test.Rdata")
} else {
    load("X004_allstatePanelData_Train.Rdata")
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
    

    
    ## loop over all of the shopping point panels (SP_02 ... SP_11)
    ##for (i in 2:11) {
    for (i in 11:11) {
        
        tmp.data    <- panel.list[[i]]$data
        tmp.len     <- panel.list[[i]]$len
        tmp.sp      <- panel.list[[i]]$sp
        
        ## expand factors into a set of binary variables
        cols        <- c(grep("^[A-G][0-9]$", colnames(tmp.data)), grep("^[A-G]10$", colnames(tmp.data)), grep("^[A-G]11$", colnames(tmp.data)))
        
        for (j in 1:length(cols)) {
            tmp.mat <- expandFactors(x=tmp.data[, eval(cols[j])], v=eval(cols[j]))
        }
        
        ##
        ## custday_key.u
    }


}

