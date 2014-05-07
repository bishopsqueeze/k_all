##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Address some of the known issues with the raw data
##  2. Normalize variable ranges
##  3. Create variables so that the entire dataset can be put into
##     a matrix rather than a data frame
##------------------------------------------------------------------

##------------------------------------------------------------------
## Observations
##------------------------------------------------------------------
## can have locations in two states (e.g., neighboring states like KS/MO)

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
#library(foreach)
#library(doMC)
library(cluster)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
#registerDoMC(4)

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
## Read the data
##------------------------------------------------------------------
load("002_allstateRawData.Rdata")

##------------------------------------------------------------------
## K-means clusters
##------------------------------------------------------------------

## isolate purchase points (?)
tmp.purch   <- subset(all.copy, record_type == 1)

## identify variables to use for cluster definition
#purch.dist   <- daisy(
#                    tmp.purch[ , c("cost", "car_age", "age_youngest", "age_oldest")],
#                    metric="euclidean",
#                    stand=TRUE
#                    )

#purch.dist  <- dist(tmp.purch[ , c("cost", "car_age", "age_youngest", "age_oldest")])
#purch.km5   <- kmeans(purch.dist, centers=5)

##------------------------------------------------------------------
## Attempt to identify location-specific clusters
##------------------------------------------------------------------

## create location-specific summary data
#cl.data <- data.frame(
#                    cost.mean = tapply(all.copy$cost, all.copy$location.r, mean, na.rm=TRUE),
#                    rf.mean = tapply(all.copy$risk_factor.r, all.copy$location.r, mean, na.rm=TRUE),
#                    car_age.mean = tapply(all.copy$car_age, all.copy$location.r, mean, na.rm=TRUE),
#                    home.frac = tapply(all.copy$homeowner, all.copy$location.r, function(x){sum(x)/length(x)}),
#                    married.frac = tapply(all.copy$married_couple, all.copy$location.r, function(x){sum(x)/length(x)})  )

## define the cluster
#cl.res  <- hclust(dist(cl.data), method = "ward")
#cl.memb <- cutree(cl.res, k = 10)

## load the results
#hc <- rep(0, nrow(all.copy))
#for (i in 1:length(cl.memb)) {
#    hc[which( all.copy$location.r == as.numeric(names(cl.memb[i])) )] <- cl.memb[i]
#}
#all.copy$hc <- hc



