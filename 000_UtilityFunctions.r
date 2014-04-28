##------------------------------------------------------------------
## <function> :: replaceBads
##------------------------------------------------------------------
## This fucntion takes as input a vector of values that we need to
## search for replacements (x) and an index to parse the data.
##------------------------------------------------------------------
replaceBads <- function(x) {
    
    nx  <- length(x)
    
    ## identify number of "bads" in x
    if (class(x) == "character") {
        nbad <- sum((x == ""))
    } else {
        nbad <- sum(is.na(x))
    }
    
    ## no bads
    if (nbad == 0) {
        
        return(x)
        
    ## all bad
    } else if (nx == nbad) {
        
        if (class(x) == "character") {
            return(rep("z", nx))
        } else {
            return(rep(-9, nx))
        }
        
    ## some bad, some good
    } else {
        
        ## use a table to isolate cases where multiple values exist,
        ## and also use the majority value as the replacement
        if (class(x) == "character") {
            x.tbl   <- table(x[x != ""])
            swap.x  <- as.character(names(x.tbl[ which(x.tbl == max(x.tbl)) ])[1])
        } else {
            x.tbl   <- table(x[!is.na(x)])
            swap.x <- as.numeric(names(x.tbl[ which(x.tbl == max(x.tbl)) ])[1])
        }
        return(rep(swap.x, nx))
    }
    
}

##------------------------------------------------------------------
## <function> :: numUnique
##------------------------------------------------------------------
## Compute the number of unique elements in a vector, then return
## that count as a vector equal in length to the input vector
##------------------------------------------------------------------
numUnique    <- function(x) {
    y <- length(unique(x))
    return( rep(y,length(x)) )
}

##------------------------------------------------------------------
## <function> :: calcDiff
##------------------------------------------------------------------
## Calculate a vector difference and prepend a zero to the output
##------------------------------------------------------------------
calcDiff    <- function(x) {
    nx  <- length(x)
    if (nx == 1) {
        return(0)
    } else {
        return(c(0, diff(x)))
    }
}

##------------------------------------------------------------------
## <function> :: convert.magic
##------------------------------------------------------------------
## A function to perform a type switch on a data.frame
##------------------------------------------------------------------
convert.magic   <- function(obj, col, type) {
    
    ## isolate the columns to convert
    idx <- which(colnames(obj) %in% col)
    
    ## loop over the columns and convert via a swtich()
    for (i in 1:length(idx)) {
        FUN <- switch(type[i], character = as.character, numeric = as.numeric, factor = as.factor)
        obj[, idx[i]]   <- FUN(obj[, idx[i]])
    }    
    return(obj)
}

##------------------------------------------------------------------
## <function> :: gkTau
##------------------------------------------------------------------
## Compute (asymmetric) correlations for nominal variables.  The
## result of gkTau(a,b) implies a predicts b to a degree specified
## by the returned correlation value [0,1]
##------------------------------------------------------------------
gkTau    <- function(x, y) {

    ## convert into a joint probability contingency table
    Pij <- prop.table(table(x, y, useNA="ifany"))
    
    ## compute marginal probabilities
    Pi  <- apply(Pij, 1, sum)
    Pj  <- apply(Pij, 2, sum)
    
    ## compute V(y)
    Vy  <- 1 - sum(Pj^2)
    
    ## compute V(x|y)
    innerSum  <- apply(Pij^2, 1, sum)
    Vybarx    <- 1 - sum(innerSum/Pi)
    
    ## compute the Goodman & Kruskal tau
    return((Vy - Vybarx)/Vy)
  
}


##------------------------------------------------------------------
## <function> :: varLag
##------------------------------------------------------------------
## The purpose of this script is very specific -- to, in effect,
## transpose a longitudinal choice/cost history into a single row
## for each customer ... at each step in the shopping_point history.
##------------------------------------------------------------------
varLag <- function(myPanel, myCost, myPurch, myHistSkel, myCostSkel, myType=NULL, myClass=NULL) {
    
    ## identify unique customers
    uniq.cust 	<- unique(myPanel[ ,c("customer_ID")])
    num.cust 	<- length(uniq.cust)
    all.letters <- paste(LETTERS[1:7],sep="",collapse="")
    
    ## loop over all the custmomers and populate a matrix with prior choices
    tmp.list <- foreach (i=1:num.cust, .inorder=FALSE) %dopar% {
        
        ## report progress
        if ((i %% 1000) == 0) { cat("Iteration = ", i, "\n") }
        
        ## isolate the rows for each customer
        row.idx <- which(myPanel[, c("customer_ID")] == uniq.cust[i])
        num.idx <- length(row.idx)
        
        ## isolate the terminal data for each customer
        if (myType == 1) {
            tmp.purch <- myPurch[ (myPurch[ , c("customer_ID")] == uniq.cust[i]) , ]
        }
        
        ## isolate the relevant slice of data
        if (myClass == "cost") {
            tmp.smp     <- myCost[ row.idx, ]
            tmp.skel    <- myCostSkel[ row.idx, ]
        } else if (myClass == "choice") {
            tmp.smp     <- myPanel[ row.idx, ]
            tmp.skel    <- myHistSkel[ row.idx, ]
        }
        
        ## loop over each historical row and populate a wide matrix
        for (j in 1:num.idx) {
            
            ## populate the terminal data (for train data only)
            if ( (myType == 1) & (myClass == "choice") & (j == 1)) {
                tmp.skel[, paste(all.letters,".T",sep="")] <- tmp.purch$choice
            }
            
            ## create the lagged histories
            if ( (myClass == "choice") ) {
                tmp.skel[, paste(all.letters,".",j-1,sep="")] <- Lag(tmp.smp$choice, shift=j-1)
            } else {
                tmp.skel[, paste("cost.s",j-1,sep="")]        <- Lag(tmp.smp$cost.s, shift=j-1)
            }
            
        }
        rownames(tmp.skel) <- paste(rownames(tmp.skel),1:nrow(tmp.skel),sep="_")
        
        ## return a the populated skeleton to the %dopar% routine
        tmp.skel
    }
    
    return(tmp.list)
}


##------------------------------------------------------------------
## <function> :: expandFactors
##------------------------------------------------------------------
## Takes a factor and explodes it into a set of binary indicator
## variables
##------------------------------------------------------------------
expandFactors   <- function(x, v="v") {
    
    n       <- nlevels(x)
    lvl     <- levels(x)
    mat     <- matrix(, nrow=length(x), ncol=n)
    tmp.v   <- c()
    
    for (i in 1:n) {
        tmp.lvl <- lvl[i]
        tmp.v   <- c(tmp.v, paste(v,".",tmp.lvl,sep=""))
        mat[,i] <- as.integer((x == tmp.lvl))
    }
    colnames(mat) <- tmp.v
    
    return(mat)
}



