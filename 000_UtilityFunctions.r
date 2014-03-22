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
            return(rep("x", nx))
        } else {
            return(rep(-9, nx))
        }
        
    ## some bad, some good
    } else {
        
        ## use a table to isolate cases where multiple values exist
        x.tbl <- table(x)
        
        ## use majority value as the replacement
        if (class(x) == "character") {
            swap.x <- as.character(names(x.tbl[ which(x.tbl == max(x.tbl)) ])[1])
        } else {
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
