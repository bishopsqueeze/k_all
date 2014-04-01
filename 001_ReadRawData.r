##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(data.table)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Read in the various files
##------------------------------------------------------------------
#train.raw	<- as.data.table(read.csv("train.csv", header=TRUE))
#test.raw	<- as.data.table(read.csv("test_v2.csv", header=TRUE))
train.raw	<- read.csv("train.csv", header=TRUE)
test.raw	<- read.csv("test_v2.csv", header=TRUE)

##------------------------------------------------------------------
## Write the raw data to an .Rdata file
##------------------------------------------------------------------
save(train.raw, test.raw, file="001_allstateRawData.Rdata")
