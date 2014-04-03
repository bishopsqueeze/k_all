##------------------------------------------------------------------
## The purpose of this script is to:
##	1. To do a very simple fit of the data using only the prior
##     observations available at a given timestep for a given
##     purchase category:
##          AT = f(A0, A1, A2, ... , AN)
##          BT = f(B0, B1, B2, ... , BN)
##          ...
##          GT = f(G0, G1, G2, ... , GN)
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(nnet)
library(MASS)

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
## Define the list of training data panels
##------------------------------------------------------------------
panel.files     <- dir("./panels")[(grep("Train", dir("./panels")))]
panel.num       <- length(panel.files)

##------------------------------------------------------------------
## Set-up
##------------------------------------------------------------------
set.seed(123)
fit.list    <- list()   ## list for output

##------------------------------------------------------------------
## Loop over each panel and use multinom to fit
##------------------------------------------------------------------
for (i in 2:2) {
    #for (i in 1:11) {
    
    ## get panel filenames
    tmp.filename    <- panel.files[i]
    
    ## report the loaded panel
    cat("Loading ... ", tmp.filename, "\n")
    
    ## load file (should generate an oject called tmp.object)
    load(paste("./panels/",tmp.filename,sep=""))

    ## define the dataset
    tmp.data    <- tmp.object$data
    tmp.len     <- tmp.object$len
    
    ## define the cross validation
    cv.k        <- 4

    ## loop over each LETTER (A, B, ... , G)
    for (j in 7:7) {
    
        ## report the loaded panel
        cat("Dependent Variable ... ", LETTERS[j], "\n")        ## clear some variables
        
        ## clear data
        fit.raw     <- NULL
        fit.step    <- NULL

        ## define the dependent variable and the last-quoted benchmark
        tmp.y      <- paste(LETTERS[j],"T",sep="")
        tmp.lq     <- paste(LETTERS[j],"0",sep="")  ## last-qutoed
        
        ## define columns to drop
        drop.cols   <- c(   c("customer_ID", "shopping_pt", "record_type", "cost", "id_fl", "last_fl", "time.nrm", "time.num"),
                            c("car_age", "age_oldest", "age_youngest", "duration_previous.r", "dcost", "ccost", "dayfrac.diff"),
                            c("location.r"),
                            LETTERS[1:7],
                            paste(LETTERS[1:7],"T",sep=""),
                            colnames(tmp.data)[grep("u$",colnames(tmp.data))],
                            colnames(tmp.data)[grep("cost[0-9]$", colnames(tmp.data))])
        
        ## for SP_01 drop "n*" and "d*" variables b/c no accumulated history
        if (i == 1) {
            drop.cols <- c(drop.cols, paste("n",LETTERS[1:7],sep=""), paste("d",LETTERS[1:7],sep=""))
        }
                    
        ## don't drop the reponse variable
        drop.cols   <- drop.cols[ -which(drop.cols %in% tmp.y) ]
                        

        ## define the regression dataframe
        tmp.reg   <- tmp.data[ , -which(colnames(tmp.data) %in% drop.cols) ]
        
        ##------------------------------------------------------------------
        ## set-up manual cross validation
        ##------------------------------------------------------------------
        err.mat <- matrix(,nrow=cv.k, ncol=2)
        colnames(err.mat) <- c("lqb","oos")
        for (k in 1:cv.k) {
     
            ## report the loaded panel
            cat("Cross Validation Iteration ... ", k, "\n")
            
            ## create the development samples via k-fold validation
            val.range   <- round(((k-1)*(tmp.len/cv.k)+1)):round((k*(tmp.len/cv.k)))
            dev         <- tmp.reg[ -val.range, ]
            val         <- tmp.reg[  val.range, ]
 
            ## define the formula
            tmp.frm     <- as.formula(paste(eval(tmp.y), "~ .",sep=""))

            ## compute the initial fit
            tmp.fit.2     <- multinom(tmp.frm, data = dev, maxit = 1000, trace=TRUE)
    
            ### perform a stepwise search
            #tmp.fit.2   <- stepAIC(tmp.fit, trace=2)
    
            ## compute some fit statistics
            #tmp.sum     <- summary(tmp.fit.2)
            #tmp.z       <- tmp.sum$coefficients/tmp.sum$standard.errors
            #tmp.p       <- (1 - pnorm(abs(tmp.z), 0, 1))*2
    
            ## in-sample fit
            fitted.ins  <- predict(tmp.fit.2, newdata=dev, type="probs")
            class.ins   <- predict(tmp.fit.2, newdata=dev, type="class")
    
            ## out-of-sample fit
            fitted.oos  <- predict(tmp.fit.2, newdata=val, type="probs")
            class.oos   <- predict(tmp.fit.2, newdata=val, type="class")
            lqbench.oos <- val[, tmp.lq]
    
            ## compute out-of-sample error rates
            lqbench.err <- sum(lqbench.oos != val[, tmp.y])/nrow(val)
            pred.err    <- sum(class.oos != val[, tmp.y])/nrow(val)
            err.mat[k,] <- c(lqbench.err, pred.err)
            
            ## save the results
            fit.l1    <- paste("SP_",ifelse(i<10, paste("0",i,sep=""),i),sep="")
            fit.l2    <- paste(tmp.y,sep="")
            fit.l3    <- paste("CV_",ifelse(k<10, paste("0",k,sep=""),k),sep="")
            fit.list[[fit.l1]][[fit.l2]][[fit.l3]] <- list(coef=tmp.sum$coefficients, sderr=tmp.sum$standard.errors)
        }
        ## archive the misclassification matrix
        fit.list[[fit.l1]][[fit.l2]]$err.mat <- err.mat
    }
}


##------------------------------------------------------------------
## Write the data to an .Rdata file
##------------------------------------------------------------------
##save(fit.list, file="010_SingleChoiceMultinomFit.Rdata")
