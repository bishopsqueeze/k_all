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
## <function> :: calcDiff
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








