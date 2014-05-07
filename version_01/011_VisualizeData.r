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
## Source utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/allstate/k_all/000_UtilityFunctions.r")

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


##******************************************************************
## Compute other correlation measures using the train panel
##******************************************************************

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
load("005_allstateRawData_Train.Rdata")


##------------------------------------------------------------------
## isolate the purchase point data
##------------------------------------------------------------------
fin.ch  <- subset(all.train, record_type == 1)
fin.ch$u01  <- runif(nrow(fin.ch))
fin.ch$n01  <- rnorm(nrow(fin.ch))

##------------------------------------------------------------------
## define a correlation matrices for choice v. categorical variables
##------------------------------------------------------------------
cat.vars  <- c( c( "day",
                "group_size",
                "homeowner",
                "married_couple",
                "risk_factor.r",
                "C_previous.r",
                "car_value.num",
                "hc",
                "shopping_pt",
                "state.num",
                "location.r"),
                c(paste("n",LETTERS[1:7],sep="")))

cat.num <- length(cat.vars)

tau.cat.xy <- matrix(,nrow=7,ncol=cat.num)
tau.cat.yx <- matrix(,nrow=7,ncol=cat.num)
rownames(tau.cat.xy)   <- LETTERS[1:7]
rownames(tau.cat.yx)   <- LETTERS[1:7]
colnames(tau.cat.xy)   <- cat.vars
colnames(tau.cat.yx)   <- cat.vars
for (i in 1:7) {
    tmp.x   <- as.factor(fin.ch[, LETTERS[i]])
    for (j in 1:cat.num) {
        tmp.y           <- as.factor(fin.ch[, cat.vars[j]])
        tau.cat.xy[i,j]    <- 100*gkTau(tmp.x, tmp.y)
        tau.cat.yx[i,j]    <- 100*gkTau(tmp.y, tmp.x)
    }
}


##------------------------------------------------------------------
## define a correlation matrices for choice v. continuous variables
##------------------------------------------------------------------
con.vars  <- c( c( "car_age",
                "age_oldest",
                "age_youngest",
                "cost",
                "duration_previous.r",
                "car_age.tr",
                "time.num",
                "dayfrac.nrm",
                "dayfrac.diff",
                "dayfrac.cum",
                "dcost",
                "ccost",
                "u01",
                "n01"), c(paste("cost",0:12,sep="")))


con.num <- length(con.vars)

tau.con.xy <- matrix(,nrow=7,ncol=con.num)
tau.con.yx <- matrix(,nrow=7,ncol=con.num)
rownames(tau.con.xy)   <- LETTERS[1:7]
rownames(tau.con.yx)   <- LETTERS[1:7]
colnames(tau.con.xy)   <- con.vars
colnames(tau.con.yx)   <- con.vars
for (i in 1:7) {
    tmp.x   <- as.vector(fin.ch[, LETTERS[i]])
    for (j in 1:con.num) {
        tmp.y           <- as.vector(fin.ch[, con.vars[j]])
        tau.con.xy[i,j]    <- 100*gkTau(tmp.x, tmp.y)
        tau.con.yx[i,j]    <- 100*gkTau(tmp.y, tmp.x)
    }
}




















