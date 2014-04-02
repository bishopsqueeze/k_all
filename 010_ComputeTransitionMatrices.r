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
## <function> :: column lookup
##------------------------------------------------------------------
columnLookup <- function(x, t) { return(which(t %in% x)) }

##------------------------------------------------------------------
## Step 1:  Create a matrix that contains a flag indicating
## which choice for each of the seven options is selected at each
## shopping point.  This is basically a less compact way of
## representing the A-G columns.
##------------------------------------------------------------------
tmp.index   <- (all.copy$id_fl == 1)
tmp.train   <- all.copy[tmp.index, ]

nr          <- sum(tmp.index)
fl          <- matrix(0, nrow=nr, ncol=22)

colnames(fl) <- c(	paste("A", 0:2, sep=""),
					paste("B", 0:1, sep=""),
					paste("C", 1:4, sep=""),
					paste("D", 1:3, sep=""),
					paste("E", 0:1, sep=""),
					paste("F", 0:3, sep=""),
					paste("G", 1:4, sep=""))

rownames(fl) <- all.copy[tmp.index, c("customer_ID")]

## find the corresponding column foreach variable
system.time({
    aj <- sapply(paste("A", tmp.train$A, sep=""), FUN=columnLookup, t=colnames(fl))
    bj <- sapply(paste("B", tmp.train$B, sep=""), FUN=columnLookup, t=colnames(fl))
    cj <- sapply(paste("C", tmp.train$C, sep=""), FUN=columnLookup, t=colnames(fl))
    dj <- sapply(paste("D", tmp.train$D, sep=""), FUN=columnLookup, t=colnames(fl))
    ej <- sapply(paste("E", tmp.train$E, sep=""), FUN=columnLookup, t=colnames(fl))
    fj <- sapply(paste("F", tmp.train$F, sep=""), FUN=columnLookup, t=colnames(fl))
    gj <- sapply(paste("G", tmp.train$G, sep=""), FUN=columnLookup, t=colnames(fl))
})

## populate the matrix with an indicator for each variable
system.time({
    fl[ cbind(1:nr, as.vector(aj)) ] <- 1
    fl[ cbind(1:nr, as.vector(bj)) ] <- 1
    fl[ cbind(1:nr, as.vector(cj)) ] <- 1
    fl[ cbind(1:nr, as.vector(dj)) ] <- 1
    fl[ cbind(1:nr, as.vector(ej)) ] <- 1
    fl[ cbind(1:nr, as.vector(fj)) ] <- 1
    fl[ cbind(1:nr, as.vector(gj)) ] <- 1
})

##------------------------------------------------------------------
## Step 2:  Create a matrix to hold "match statistics" for each
## shopping point associated with each customer.  This allows us
## to see which of the intermediate selections match either [1]
## the prior selection or [2] the final purchase.
##------------------------------------------------------------------
tr.match            <- matrix(data=0, nrow=nrow(tmp.train), ncol=4)
colnames(tr.match)  <- c("customer_ID","shopping_pt","match_prev","match_fin")

tr.match[, c("customer_ID", "shopping_pt")] <- as.matrix(tmp.train[, c("customer_ID", "shopping_pt")])

##------------------------------------------------------------------
## Step 3: Loop over each customer and for each shopping point in the
## customer-specific dataset, period-over-period transition matrix
## and the jump-to-purchase transition matrix.
##------------------------------------------------------------------

## get a customer count
uniq.cust <- unique(tr.match[,c("customer_ID")])
num.cust  <- length(uniq.cust)

## compute the range of customer interactions
tr.freqByCust   <- table(tr.match[,c("customer_ID")])
tr.freqAgg      <- table(table(tr.match[,c("customer_ID")]))

##------------------------------------------------------------------
## create one-step and jump-to-purchase transition matrices
##------------------------------------------------------------------

## period-over-period transition matrix
tarr 			<- array(0, dim=c(ncol(fl), ncol(fl), 13))
colnames(tarr) 	<- colnames(fl)
rownames(tarr) 	<- colnames(fl)

## jump-to-purchase transition matrix
parr			<- array(0, dim=c(ncol(fl), ncol(fl), 13))
colnames(parr) 	<- colnames(fl)
rownames(parr) 	<- colnames(fl)

## column name set-up data
col.names 		<- colnames(fl)
col.idx 		<- which(colnames(fl) %in% col.names)
col.num			<- length(col.idx)

##------------------------------------------------------------------
## Build the transition matrices
##------------------------------------------------------------------
for (i in 1:num.cust) {

	row.idx		<- which(tr.match[,c("customer_ID")] == uniq.cust[i])
	tmp.tr		<- fl[ row.idx, col.idx ]
    
	n.touch 	<- nrow(tmp.tr)
	tmp.prev	<- 0*vector("numeric", length=n.touch)
	tmp.fin		<- 0*vector("numeric", length=n.touch)
	
	##------------------------------------------------------------------
    ## timestep base transition matrix
	##------------------------------------------------------------------

	## grab the data
	ts.idx		<- 2:n.touch
	t0			<- tmp.tr[ts.idx-1, ]
	t1			<- tmp.tr[ts.idx, ]
	
	## register a match
	tmp.prev[ts.idx] <- 1*(apply((t1-t0), 1, function(x){sum((x==0))}) == col.num)
	tr.match[row.idx, c("match_prev")] <- tmp.prev
	
	## isolate hit positions in each vector
	ts.row <- apply(t0, 1, function(x){which(x==1)})
	ts.col <- apply(t1, 1, function(x){which(x==1)})
	ts.mat <- t(replicate(nrow(ts.row), ts.idx))
	
	##------------------------------------------------------------------
    ## jump-to-purchase transition matrix
	##------------------------------------------------------------------

	## grab the data
	jp.idx		<- 1:(n.touch-1)
	t0			<- tmp.tr[jp.idx, ]
	t1			<- tmp.tr[rep(n.touch, length(jp.idx)), ]
	
	## register a match
	tmp.fin[jp.idx] <- 1*(apply((t1-t0), 1, function(x){sum((x==0))}) == col.num)
	tr.match[row.idx, c("match_fin")] <- tmp.fin
	
	## isolate hit positions in each vector
	jp.row <- apply(t0, 1, function(x){which(x==1)})
	jp.col <- apply(t1, 1, function(x){which(x==1)})
	jp.mat <- t(replicate(nrow(jp.row), jp.idx))	
	
	## incrementally add to the transition matrix
	
	if ( ncol(jp.row) == ncol(ts.row) ) {
	  for (j in 1:ncol(jp.row)) {
	  
	    idx.ts <- cbind(ts.row[ ,j], ts.col[ ,j], ts.mat[ ,j])
	    tarr[idx.ts] <- tarr[idx.ts] + 1
	    
	    idx.jp <- cbind(jp.row[ ,j], jp.col[ ,j], jp.mat[ ,j])
	    parr[idx.jp] <- parr[idx.jp] + 1
	  }
	
	} else {
		warning("column mismatch")
	}

if ((i %% 100) == 0) { cat("Iteration =", i, "\n")}
}

##------------------------------------------------------------------
## Transform the tr.match matrix into a data frame
##------------------------------------------------------------------
tr.match                <- as.data.frame(tr.match)
tr.match$customer_ID    <- as.integer(tr.match$customer_ID)

####################################################################

##------------------------------------------------------------------
## Create normalized transition matrices - Not sure what this gets me
##------------------------------------------------------------------

## grab the number of shopping points
sh_pt       <- subset(tmp.train, record_type == 0)
sh_pt.tbl   <- table(sh_pt$shopping_pt)
sh_pt.num   <- length(sh_pt.tbl)

## compute the transition probabilities for each choice
tarr.n      <- tarr
parr.n      <- parr
for (i in 1:(sh_pt.num)) {
    
    ## tarr has non-zero values between 2:13
    tarr.n[,,i+1] <- tarr[,,i+1] / sh_pt.tbl[i]
    
    ## parr has non-zero values between 1:12
    parr.n[,,i] <- parr[,,i] / sh_pt.tbl[i]
    
}

##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
save(fl, tr.match, tarr, parr, file="003_allstateRawData.Rdata")
