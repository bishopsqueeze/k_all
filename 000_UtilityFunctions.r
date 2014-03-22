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
            return(rep(-99999, nx))
        }
    ## some bad, some good
    } else {
        if (class(x) == "character") {
            swap.x <- x[(x != "")][1]
        } else {
            swap.x <- x[!is.na(x)][1]
        }
        return(rep(swap.x, nx))
    }
}

