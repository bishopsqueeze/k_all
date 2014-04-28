##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Create the raw materials for a panel dataset to be used in a
##     regression model for the insurance purchase targets
##  2. This script will create a separate panel for each combination
##     of shopping_pt (1, 2, 3, ... , N) and choice (A, B, ... , G).
##  3. Terminal (purchase) rows are not included in the panel
##  4. What we end-up with is the entire set of observations from
##     a particular shopping_pt.
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
## One loop for test data (0) and one loop for training data (1)
##------------------------------------------------------------------
for (n in 0:1) {
    
    ##------------------------------------------------------------------
    ## Load data
    ##------------------------------------------------------------------
    if (n == 0) {
        load("X003_allstateRawData_Test.Rdata")
        smp <- all.test; rm(all.test, all.copy, cost.test, hist.test)
    } else {
        load("X003_allstateRawData_Train.Rdata")
        smp <- all.train; rm(all.train, all.copy, cost.train, hist.train)
    }

    ##------------------------------------------------------------------
    ## Isolate the number of timesteps and choices
    ##------------------------------------------------------------------
    tvec    <- unique(smp$shopping_pt)  ## timesteps
    tnum    <- length(tvec)

    ##------------------------------------------------------------------
    ## Create a placeholder list
    ##------------------------------------------------------------------
    panel.list  <- list()

    ##------------------------------------------------------------------
    ## Location of the first (of N) choice columns -- this script presumes
    ## that all of prior choice variables lie to the right of this column
    ##------------------------------------------------------------------
    choice.idx     <- which(colnames(smp) == "ABCDEFG.T")
    cost.idx       <- which(colnames(smp) == "cost.s0")

    ##------------------------------------------------------------------
    ## Loop over the time/choice combinations and load a separate panel
    ## for each combination [e.g., (shopping_pt == 2) & (the "F" option)]
    ##------------------------------------------------------------------
    for (i in 1:11) {
        cat("Iteration ",i, " ... of 11 \n")
        
        tmp.time    <- tvec[i]  ## the shopping_pt (1, 2, 3, ...)
        
        ## load non-purchase points
        row.idx     <- ( (smp$shopping_pt %in% tmp.time) & (smp$record_type != 1) )
        
        ## add the new cost data (i.e., all prior decision info)
        tmp.dat     <- smp[row.idx, c( 1:(cost.idx-1), (cost.idx):(cost.idx+i-1), (choice.idx):(choice.idx+i) )]
     
        ## define the panel ID
        panel_id <- paste("SP_",ifelse(tmp.time < 10, paste("0",tmp.time,sep=""),tmp.time),sep="")
                
        ## load the raw data into a list
        panel.list[[panel_id]]$sp   <- panel_id
        panel.list[[panel_id]]$data <- droplevels(tmp.dat)  ## drop superfluous levels
        panel.list[[panel_id]]$len  <- nrow(tmp.dat)
    }

    ##------------------------------------------------------------------
    ## Create choice histories for ...
    ##  - single choices    {"A","B","C","D","E","F","G"}
    ##  - group choices     {"AF","BE","CD","G"}
    ##------------------------------------------------------------------
    ## Locations:   ABCDEFG
    ##              1234567
    ##------------------------------------------------------------------
    panel.names  <- names(panel.list)
    #group.names  <- c("AF","BE","CD","G")
    single.names <- c("A","B","C","D","E","F","G")
    
    ## loop over each panel and populate the prior choice & choice cluster histories
    for (i in 1:length(panel.names)) {
        
        cat("Panel Update Iteration ",i, " ... of 11 \n")
    
        panel_id    <- panel.names[i]
        tmp.sp      <- as.integer(substr(panel.list[[panel_id]]$sp,4,5))
        tmp.dat     <- panel.list[[panel_id]]$data

        ##------------------------------------------------------------------
        ## extract single-name non-termainal results
        ##------------------------------------------------------------------
        for (j in 1:length(single.names)) {
    
            tmp.single  <- single.names[j]

            ## for each of the prior, concatenate non-terminal results
            for (k in 1:(tmp.sp)) {
                if (tmp.single == "A") {
                    tmp.AN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],1,1)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.AN)
                } else if (tmp.single == "B") {
                    tmp.BN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],2,2)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.BN)
                } else if (tmp.single == "C") {
                    tmp.CN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],3,3)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.CN)
                } else if (tmp.single == "D") {
                    tmp.DN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],4,4)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.DN)
                } else if (tmp.single == "E") {
                    tmp.EN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],5,5)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.EN)
                } else if (tmp.single == "F") {
                    tmp.FN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],6,6)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.FN)
                } else if (tmp.single == "G") {
                    tmp.GN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],7,7)
                    tmp.dat[ , paste(tmp.single,k-1,sep="")] <- as.factor(tmp.GN)
                }
            }

            ## concatenate terminal results for training data only
            if (n == 1) {
                if (tmp.single == "A") {
                    tmp.AT                              <- substr(tmp.dat$ABCDEFG.T,1,1)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.AT)
                } else if (tmp.single == "B") {
                    tmp.BT                              <- substr(tmp.dat$ABCDEFG.T,2,2)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.BT)
                } else if (tmp.single == "C") {
                    tmp.CT                              <- substr(tmp.dat$ABCDEFG.T,3,3)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.CT)
                } else if (tmp.single == "D") {
                    tmp.DT                              <- substr(tmp.dat$ABCDEFG.T,4,4)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.DT)
                } else if (tmp.single == "E") {
                    tmp.ET                              <- substr(tmp.dat$ABCDEFG.T,5,5)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.ET)
                } else if (tmp.single == "F") {
                    tmp.FT                              <- substr(tmp.dat$ABCDEFG.T,6,6)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.FT)
                } else if (tmp.single == "G") {
                    tmp.GT                              <- substr(tmp.dat$ABCDEFG.T,7,7)
                    tmp.dat[, paste(tmp.single,"T",sep="")] <- as.factor(tmp.GT)
                }
            }
            
        } ## /// end of single parameter loop
        

        ##------------------------------------------------------------------
        ## <X> extract group/cluster names
        ##------------------------------------------------------------------
        #for (j in 1:length(group.names)) {
        #
        #    tmp.gp  <- group.names[j]
        #
        #    ## concatenate non-terminal results (redundant ????)
        #    #if (tmp.gp == "AF") {
        #    #    tmp.dat[ , tmp.gp] <- as.factor(cbind(paste(tmp.dat$A,tmp.dat$F,sep="")))
        #    #} else if (tmp.gp == "BE") {
        #    #    tmp.dat[ , tmp.gp] <- as.factor(cbind(paste(tmp.dat$B,tmp.dat$E,sep="")))
        #    #} else if (tmp.gp == "CD") {
        #    #    tmp.dat[ , tmp.gp] <- as.factor(cbind(paste(tmp.dat$C,tmp.dat$D,sep="")))
        #    #} else if (tmp.gp == "G") {
        #    #    #tmp.dat[ , tmp.gp] <- as.factor(tmp.dat$G)
        #    #}
        #
        #    ## concatenate terminal results for training data only
        #    if (n == 1) {
        #        if (tmp.gp == "AF") {
        #            tmp.AT                              <- substr(tmp.dat$ABCDEFG.T,1,1)
        #            tmp.FT                              <- substr(tmp.dat$ABCDEFG.T,6,6)
        #            tmp.dat[, paste(tmp.gp,"T",sep="")] <- as.factor(cbind(paste(tmp.AT,tmp.FT,sep="")))
        #        } else if (tmp.gp == "BE") {
        #            tmp.BT                              <- substr(tmp.dat$ABCDEFG.T,2,2)
        #            tmp.ET                              <- substr(tmp.dat$ABCDEFG.T,5,5)
        #            tmp.dat[, paste(tmp.gp,"T",sep="")] <- as.factor(cbind(paste(tmp.BT,tmp.ET,sep="")))
        #        } else if (tmp.gp == "CD") {
        #            tmp.CT                              <- substr(tmp.dat$ABCDEFG.T,3,3)
        #            tmp.DT                              <- substr(tmp.dat$ABCDEFG.T,4,4)
        #            tmp.dat[, paste(tmp.gp,"T",sep="")] <- as.factor(cbind(paste(tmp.CT,tmp.DT,sep="")))
        #        } else if (tmp.gp == "G") {
        #            tmp.G                               <- substr(tmp.dat$ABCDEFG.T,7,7)
        #            tmp.dat[, paste(tmp.gp,"T",sep="")] <- as.factor(tmp.G)
        #       }
        #    }
        #
        #    ## for each of the priors, concatenate non-terminal results
        #    for (k in 1:(tmp.sp)) {
        #       if (tmp.gp == "AF") {
        #            tmp.AN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],1,1)
        #            tmp.FN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],6,6)
        #            tmp.dat[ , paste(tmp.gp,k-1,sep="")] <- as.factor(cbind(paste(tmp.AN,tmp.FN,sep="")))
        #       } else if (tmp.gp == "BE") {
        #            tmp.BN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],2,2)
        #            tmp.EN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],5,5)
        #            tmp.dat[ , paste(tmp.gp,k-1,sep="")] <- as.factor(cbind(paste(tmp.BN,tmp.EN,sep="")))
        #       } else if (tmp.gp == "CD") {
        #            tmp.CN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],3,3)
        #            tmp.DN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],4,4)
        #            tmp.dat[ , paste(tmp.gp,k-1,sep="")] <- as.factor(cbind(paste(tmp.CN,tmp.DN,sep="")))
        #        } else if (tmp.gp == "G") {
        #            tmp.GN <- substr(tmp.dat[,paste("ABCDEFG.",k-1,sep="")],7,7)
        #            tmp.dat[ , paste(tmp.gp,k-1,sep="")] <- as.factor(cbind(tmp.GN))
        #       }
        #   }
        #
        #} ## /// end of group parameter loop
        
        ## update panel with augmented data
        panel.list[[panel_id]]$data <- tmp.dat
    }

    ##------------------------------------------------------------------
    ## Write the full panel to an .Rdata file
    ##------------------------------------------------------------------
    if (n == 0) {
        save(panel.list, file="X004_allstatePanelData_Test.Rdata")
    } else {
        save(panel.list, file="X004_allstatePanelData_Train.Rdata")
    }

    ##------------------------------------------------------------------
    ## Write individual panels to separate .Rdata files
    ##------------------------------------------------------------------
    panel.names <- names(panel.list)

    for (i in 1:length(panel.names)) {
        tmp.panel       <- panel.names[i]
        if (n == 0) {
            tmp.filename    <- paste("./panels/X004_allstatePanelData_Test.",tmp.panel,".Rdata",sep="")
        } else {
            tmp.filename    <- paste("./panels/X004_allstatePanelData_Train.",tmp.panel,".Rdata",sep="")
        }
        tmp.object      <- panel.list[[tmp.panel]]
        save(tmp.object, file=tmp.filename)
    }
    
} ## end of train/test loop


