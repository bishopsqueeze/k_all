##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(plyr)
library(lsr)

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
## Load raw data
##------------------------------------------------------------------
load("002_allstateRawData.Rdata")


##******************************************************************
## Analysis #1:  Single option choices
##******************************************************************

##------------------------------------------------------------------
## Loop over each of the panels and compute match between each of
## the single plan options (A ... G) selected at a given shopping
## point and the terminal plan option.
##
## Note that the plan options are not independent (see below)
##------------------------------------------------------------------
panel.stats <- list()
for (i in 1:11) {

    ## load the data
    tmp.id          <- paste("SP_",ifelse(i<10, paste("0",i,sep=""),i),sep="")
    tmp.filename    <- paste("./panels/006_allstatePanelData_Train.",tmp.id,".Rdata",sep="")
    load(tmp.filename)

    ## reassign the list to a dataframe
    d   <- tmp.object$data
    len <- tmp.object$len
    
    ## matrix to hold the proportion of prior-to-terminal matches at each state
    match.mat <- matrix(, nrow=7, ncol=i)
    rownames(match.mat) <- LETTERS[1:7]
    colnames(match.mat) <- paste("PREV",0:(i-1),sep="")
  
    count.mat  <- matrix(, nrow=7, ncol=i)
    ## loop over the possible combos as compute proportions of matches
    for (k in 1:7) {
        for (j in 1:i) {
   
        tmp.terminal    <- paste(LETTERS[k],"T",sep="")
        tmp.prior       <- paste(LETTERS[k],j-1,sep="")
    
        ## average match rate at each shopping_pt
        match.mat[k,j]   <- mean(d[ ,tmp.prior] == d[ ,tmp.terminal])
        
        }
    }
    panel.stats[[tmp.id]]$match.mat <- match.mat
    panel.stats[[tmp.id]]$len       <- len
}


##------------------------------------------------------------------
## Plot the probability that prior (single option) selection match the terminal selection
##------------------------------------------------------------------
for (i in 2:11) {
    tmp.id          <- paste("SP_",ifelse(i<10, paste("0",i,sep=""),i),sep="")
    tmp.filename    <- paste("../figs/PriorSelectionMatchProbabilities_",tmp.id,".pdf",sep="")
    
    pdf(tmp.filename)
    for (j in 1:7) {
        if (j == 1) {
            plot(panel.stats[[tmp.id]]$match.mat[j,], type="b", col=j, ylab=c("Match Prob"), xlab=c("Previous Step"), ylim=c(0,1))
        } else {
            points(panel.stats[[tmp.id]]$match.mat[j,], type="b", col=j)
        }
    }
    dev.off()
}

##******************************************************************
## Analysis #2:  Predicted success rate of the last quoted plan
##******************************************************************

##------------------------------------------------------------------
## Compute an estimate of the last quoted benchmarck success rate
## for the test sample based on match rates from the training sample
##------------------------------------------------------------------

## grab the test/train data from the full sample
tmp.test    <- subset(all.copy, id_fl==0)[,c(c("customer_ID","shopping_pt","record_type"),LETTERS[1:7])]
tmp.train   <- subset(all.copy, id_fl==1)[,c(c("customer_ID","shopping_pt","record_type"),LETTERS[1:7])]

## isolate purchase and non-purchase data in the training data
tmp.purch   <- subset(tmp.train, record_type==1)
tmp.shop    <- subset(tmp.train, record_type==0)

## isolate the last observed shopping point for each customer in the *test* data
num.test    <- length(tmp.test$customer_ID)
max.test    <- tmp.test[ unlist(lapply(split(1:num.test, tmp.test$customer_ID), tail, 1)), ]
prop.test   <- table(max.test$shopping_pt)/max(table(max.test$shopping_pt)) ## proportion in each shopping_pint

## isolate the last observed shopping point for each customer in the *training* data
## ... note that all of these will be one-step prior to purchase
num.shop <- length(tmp.shop$customer_ID)
max.shop <- tmp.shop[ unlist(lapply(split(1:num.shop, tmp.shop$customer_ID), tail, 1)), ]

## to estimate the probability that the last quoted plan will match the chosen
## plan using a mixture of customers (i.e., those that make a purchase in
## _at least_ one subsequent shopping points, use the full training sample
all.shop <- subset(tmp.shop, shopping_pt > 1)

## create a data.frame to use for joining terminal choices to the shopping point data
tmp.join        <- tmp.purch
names(tmp.join)[which(names(tmp.join) %in% LETTERS[1:7])] <-paste(LETTERS[1:7],"T",sep="")  ## rename

## do the join
all.shop        <- join(all.shop, tmp.join, by="customer_ID")
max.shop        <- join(max.shop, tmp.join, by="customer_ID")

## create a vector of the "full" plan combos in the purchase and non-purchase training data
tmp.purch$plan  <- apply(tmp.purch[,LETTERS[1:7]], 1, paste, sep="", collapse="")
max.shop$plan   <- apply(max.shop[,LETTERS[1:7]], 1, paste, sep="", collapse="")
max.shop$plan.T <- apply(max.shop[,paste(LETTERS[1:7],"T",sep="")], 1, paste, sep="", collapse="")
all.shop$plan   <- apply(all.shop[,LETTERS[1:7]], 1, paste, sep="", collapse="")
all.shop$plan.T <- apply(all.shop[,paste(LETTERS[1:7],"T",sep="")], 1, paste, sep="", collapse="")

## compute a frequency table for the last shopping_pt in the test and non-purchase training data
max.test.tbl    <- table(max.test$shopping_pt)
max.shop.tbl    <- table(max.shop$shopping_pt)
all.shop.tbl    <- table(all.shop$shopping_pt)

## plot a comparison of frequencies within the datasets
pdf("../figs/TrainTestShoppingPointFrequencies.pdf")
    plot(2:12, as.vector(max.shop.tbl/sum(max.shop.tbl)), col="blue", type="b", xlab=c("Max Shopping Point"), ylab=c("Prob"), ylim=c(0.0,0.35))
    points(2:11, as.vector(max.test.tbl/sum(max.test.tbl)), col="red", type="b")
    points(2:12, as.vector(all.shop.tbl/sum(all.shop.tbl)), col="darkgreen", type="b")
dev.off()

## compute the prob a single plan option (at a given shopping point) matches the terminal selection for that option
max.prob  <- matrix(, nrow=7, ncol=12)
for (i in 1:7) {
    for (j in 2:12) {
        max.idx         <- which(max.shop$shopping_pt == j)
        max.prob[i,j]   <- mean(max.shop[max.idx,LETTERS[i]] == max.shop[max.idx,paste(LETTERS[i],"T",sep="")])
    }
}
colnames(max.prob) <- as.character(1:12)
rownames(max.prob) <- LETTERS[1:7]

## compute the prob all plan options (at a given shopping point) matches the terminal plan
## using only the last-quoted data
max.set  <- matrix(, nrow=1, ncol=12)
for (j in 2:12) {
    max.idx         <- which(max.shop$shopping_pt == j)
    max.set[1,j]    <- mean(max.shop[max.idx,c("plan")] == max.shop[max.idx,c("plan.T")])
}
colnames(max.set) <- as.character(1:12)
rownames(max.set) <- c("all")

## compute the joint probability of matching across the indiviudal categories
match.prob  <- apply(max.prob, 2, prod)

## compute the prob all plan options (at a given shopping point) match the terminal plan
## using all available data
all.set  <- matrix(, nrow=1, ncol=12)
for (j in 2:12) {
    all.idx         <- which(all.shop$shopping_pt == j)
    all.set[1,j]    <- mean(all.shop[all.idx,c("plan")] == all.shop[all.idx,c("plan.T")])
}
colnames(all.set) <- as.character(1:12)
rownames(all.set) <- c("all")


##------------------------------------------------------------------
## compute the estimated last-quoted plan benchmarks ...
##------------------------------------------------------------------

## ... with training sample hit-rates estimated using only the last quoted plan
estimated.max <- (max.test.tbl %*% max.set[which(colnames(max.set) %in% names(max.test.tbl))] )/sum(max.test.tbl)

## ... with training sample hit-rates estimated using all data
estimated.all <- (max.test.tbl %*% all.set[which(colnames(all.set) %in% names(max.test.tbl))] )/sum(max.test.tbl)

## plot a comparison of frequencies within the datasets
pdf("../figs/LastQuotedBenchmarkMatchProbabilities.pdf")
    plot(1:12, match.prob, col="blue", type="b", xlab="Max Shopping Point", ylab="Prob", ylim=c(0.3,0.8))
    points(1:12, max.set, col="red", type="b")
    points(1:12, all.set, col="green", type="b")
dev.off()

##------------------------------------------------------------------
## Results:
##------------------------------------------------------------------
##  1. Using the last observed quote before purchase in the
##     training set, we estimate one could obtain a 60.03318% plan/set
##     match rate using the last quoted plan (if [!!!] the test
##     data were truncated one-step prior to purchase)
##
##  2. Using the full set of training data (b/c there is no reason
##     to do otherwise ... as in example 2) ... we estimate one
##     could obtain a 53.63147% plan/set match rate.  This is the best
##     example as it uses all of the train data to determine the
##     match probability using the last quoted plan from a variety
##     of customers who later make a purchase.
##
##  3. Note that the kaggle last quoted benchmark for the test
##     set is 0.53793 = 53.793%
##
##  4. So these values give us a limit on the likely ranges of
##     possible results (if we were to restrict ourselves to the
##     last quoted plan as a predictor).
##      - Knowing that the data are truncated at the step prior to
##        purchase suggests an upper limit of ~60%
##      - An aribitrarily truncated dataset will yield a lower
##        limit of ~53.7%
##
##  5. Note how the two methods converge as the shopping_pt
##     nears its upper bound.  This makes sense because as you
##     reach the largest possible shopping point, you only have
##     cases where there is a single shopping point left until
##     purchase.
##------------------------------------------------------------------


##******************************************************************
## Analysis #3:  Dependence of choices
##******************************************************************

##------------------------------------------------------------------
## Test the gkTau measure of dependence
##------------------------------------------------------------------

## should be little dependence between a, b, and c
set.seed(123)
a <- sample(1:4, 1000, replace=TRUE)
b <- sample(1:4, 1000, replace=TRUE)
c <- sample(0:1, 1000, replace=TRUE)

##------------------------------------------------------------------
## Results confirm that the function works as expected
##------------------------------------------------------------------
##> gkTau(a,b)
##[1] 0.004258624
##> gkTau(b,a)
##[1] 0.004245837
##> gkTau(a,a)
##[1] 1
##> gkTau(a,runif(1000))
##[1] 0.003003003
##> gkTau(a,c)
##[1] 0.002874097
##> gkTau(c,a)
##[1] 0.0009491164

##------------------------------------------------------------------
## Compute the gkTau relationship amongst each pair of final choices
##------------------------------------------------------------------
tmp.train   <- droplevels(subset(all.copy, id_fl==1)[,c(c("customer_ID","shopping_pt","record_type"),LETTERS[1:7])])
tmp.purch   <- droplevels(subset(tmp.train, record_type==1))

## define matrices
tau.mat.xy  <- matrix(,nrow=7,ncol=7)
tau.mat.yx  <- matrix(,nrow=7,ncol=7)
chi.sqr.xy  <- matrix(,nrow=7,ncol=7)
phi.sqr.xy  <- matrix(,nrow=7,ncol=7)
cramersV.xy <- matrix(,nrow=7,ncol=7)

## label matrices
rownames(tau.mat.xy)   <- paste(LETTERS[1:7], "T", sep="")
colnames(tau.mat.xy)   <- paste(LETTERS[1:7], "T", sep="")
rownames(tau.mat.yx)   <- paste(LETTERS[1:7], "T", sep="")
colnames(tau.mat.yx)   <- paste(LETTERS[1:7], "T", sep="")
rownames(chi.sqr.xy)   <- paste(LETTERS[1:7], "T", sep="")
colnames(chi.sqr.xy)   <- paste(LETTERS[1:7], "T", sep="")
rownames(phi.sqr.xy)   <- paste(LETTERS[1:7], "T", sep="")
colnames(phi.sqr.xy)   <- paste(LETTERS[1:7], "T", sep="")
rownames(cramersV.xy)   <- paste(LETTERS[1:7], "T", sep="")
colnames(cramersV.xy)   <- paste(LETTERS[1:7], "T", sep="")

## compute (a,b) and (b,a) estiamtes
for (i in 1:7) {
    tmp.x   <- as.factor(tmp.purch[, LETTERS[i]])
    for (j in 1:7) {
        
        ## compute gkTau
        tmp.y              <- as.factor(tmp.purch[, LETTERS[j]])
        tau.mat.xy[i,j]    <- 100*gkTau(tmp.x, tmp.y)
        tau.mat.yx[i,j]    <- 100*gkTau(tmp.y, tmp.x)
        
        ## compute Chi-squared & Cramer's V tests
        xy.table         <- table(tmp.x,tmp.y)
        xy.Xsq           <- chisq.test(xy.table)
        chi.sqr.xy[i,j]  <- xy.Xsq$p.value
        phi.sqr.xy[i,j]  <- xy.Xsq$statistic/length(tmp.y)
        #cramersV.xy[i,j] <- 100*(sqrt(phi.sqr.xy[i,j]) / min(nrow(xy.table)-1,ncol(xy.table)-1)) ??
        cramersV.xy[i,j] <- cramersV(xy.table)
    }
}



##------------------------------------------------------------------
## create new groups and test the dependence
##------------------------------------------------------------------

## GROUP_1 :: {A,F}, {B,E}, {C,D} {G}
group_1 <- c("AF","BE","CD","G")

tmp.purch$AF <- as.factor(paste(tmp.purch$A, tmp.purch$F, sep=""))
tmp.purch$BE <- as.factor(paste(tmp.purch$B, tmp.purch$E, sep=""))
tmp.purch$CD <- as.factor(paste(tmp.purch$C, tmp.purch$D, sep=""))

tau.mat.group_1.xy  <- matrix(,nrow=4,ncol=4)
tau.mat.group_1.yx  <- matrix(,nrow=4,ncol=4)
colnames(tau.mat.group_1.xy) <- group_1
rownames(tau.mat.group_1.xy) <- group_1

for (i in 1:length(group_1)) {
    tmp.x <- tmp.purch[, group_1[i]]
    for (j in 1:length(group_1)) {
        tmp.y                   <- tmp.purch[, group_1[j]]
        tau.mat.group_1.xy[i,j] <- 100*gkTau(tmp.x, tmp.y)
    }
}


## GROUP_2 :: {A,B,E,F}, {C,D} {G}
group_2 <- c("ABEF","CD","G")

tmp.purch$ABEF <- as.factor(paste(tmp.purch$A, tmp.purch$B, tmp.purch$E, tmp.purch$F, sep=""))

tau.mat.group_2.xy  <- matrix(,nrow=3,ncol=3)
tau.mat.group_2.yx  <- matrix(,nrow=3,ncol=3)
colnames(tau.mat.group_2.xy) <- group_2
rownames(tau.mat.group_2.xy) <- group_2

for (i in 1:length(group_2)) {
    tmp.x <- tmp.purch[, group_2[i]]
    for (j in 1:length(group_2)) {
        tmp.y                   <- tmp.purch[, group_2[j]]
        tau.mat.group_2.xy[i,j] <- 100*gkTau(tmp.x, tmp.y)
    }
}




####################################################################

##------------------------------------------------------------------
## Determine if there are any forbidden transitions
##------------------------------------------------------------------
tmp.train   <- subset(all.copy, id_fl==1)
tmp.purch   <- subset(tmp.train, record_type==1)

choice.grid     <- expand.grid(A=c(0,1,2), B=c(0,1), C=c(1,2,3,4), D=c(1,2,3), E=c(0,1), F=c(0,1,2,3), G=c(1,2,3,4))
choice.vec      <- apply(choice.grid, 1, function(x){paste(x,collapse="")})
purchase.vec    <- apply(tmp.train[(tmp.purch$record_type == 1), LETTERS[1:7]], 1, function(x){paste(x,collapse="")})

purchase.tbl        <- table(purchase.vec)
choice.tbl          <- 0*vector(,length=length(choice.vec))
names(choice.tbl)   <- choice.vec

choice.tbl[ which(choice.vec %in% names(purchase.tbl)) ] <- purchase.tbl
choice.tbl  <- choice.tbl / sum(choice.tbl)



