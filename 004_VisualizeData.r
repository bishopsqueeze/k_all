##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
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
load("002_allstateRawData.Rdata")

##------------------------------------------------------------------
## Define figure directory
##------------------------------------------------------------------
figdir  <- "/Users/alexstephens/Development/kaggle/allstate/figs/"

##------------------------------------------------------------------
## Boxplots of Cost vs. Category
##------------------------------------------------------------------
for (i in 1:7) {
    ## create the filename/formula
    tmp.filename    <- paste(figdir, "Boxplot","CostVs",LETTERS[i],".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~",LETTERS[i],sep=""))
    ## write the plot to disk
    pdf(tmp.filename)
        boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
}

##------------------------------------------------------------------
## Boxplots of Cost vs. [Original] Other Factors
##------------------------------------------------------------------
for (i in 1:1) {
    
    ## car_age
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","CarAge",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","car_age",sep=""))
    pdf(tmp.filename)
        boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
    ## group_size
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","GroupSize",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","group_size",sep=""))
    pdf(tmp.filename)
        boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
    ## homeowner
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","Homeowner",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","homeowner",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
  
    ## car_value
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","CarValue",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","car_value",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## risk_factor
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","RiskFactor",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","risk_factor",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## age_oldest
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","AgeOldest",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","age_oldest",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## age_yougest
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","AgeYoungest",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","age_youngest",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## married_couple
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","MarriedCouple",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","married_couple",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## C_previous
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","C_Previous",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","C_previous",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()

    ## duration_previous
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","Duration_Previous",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","duration_previous",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
}

##------------------------------------------------------------------
## Boxplots of Cost vs. [Replacement] Other Factors
##------------------------------------------------------------------
for (i in 1:1) {
    
    ## car_value (scrubbed)
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","CarValueScrubbed",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","car_value.r",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
    ## risk_factor (scrubbed)
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","RiskFactorScrubbed",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","risk_factor.r",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
    ## C_previous (scrubbed)
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","C_PreviousScrubbed",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","C_previous.r",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
    
    ## duration_previous (scrubbed)
    tmp.filename    <- paste(figdir, "Boxplot","CostVs","Duration_PreviousScrubbed",".pdf",sep="")
    tmp.frm         <- as.formula(paste("cost~","duration_previous.r",sep=""))
    pdf(tmp.filename)
    boxplot(tmp.frm, data=all.copy, col="steelblue")
    dev.off()
}



