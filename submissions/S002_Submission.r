##------------------------------------------------------------------
## The purpose of this script is to:
##	1.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(gbm)
library(plyr)
library(caret)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##******************************************************************
## Step 1: Load the complete panel
##******************************************************************

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data")

##------------------------------------------------------------------
## Load the full set of consolidated train/test observations
##------------------------------------------------------------------
load("003_allstateRawData.Rdata"); rm(all.copy, cost.test, cost.train, hist.test, hist.train)

##------------------------------------------------------------------
## Create a slim data frame
##------------------------------------------------------------------
pred.test   <- all.test[ , c("customer_ID","shopping_pt","record_type","key",LETTERS[1:7])]
pred.test[ , c("AF.pred", "BE.pred", "CD.pred", "G.pred") ] <- "99"


##******************************************************************
## Step 2: Load the raw materials for predictions
##******************************************************************

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/allstate/data/gbm_scored")

##------------------------------------------------------------------
## Load fit data
##------------------------------------------------------------------

fit.files   <- dir(, pattern="Rdata")

for (i in 1:length(fit.files)) {

   ## clear-out the fit
   tmp.fit  <- NULL
   
   ## get file info
   tmp.file <- fit.files[i]
   tmp.name <- gsub(".Rdata", ".fit", tmp.file)
   
   ## define the panle name
   tmp.sp       <- strsplit(tmp.file,"[.]")[[1]][1]
   tmp.group    <- strsplit(strsplit(tmp.file,"[.]")[[1]][2],"_")[[1]][2]
   
   ## load the fit into a list
   load(tmp.file)
   
   ## calculate the predicted group
   tmp.pred <-  predict(tmp.fit, newdata=testDescr, type="raw")
   
   #assign(tmp.name, tmp.fit)
   test.data[ , c(paste(tmp.group,".pred",sep="")) ]    <- tmp.pred
   
   ## map the results into the prediction data frame
   pred.test[  which(pred.test$key %in% test.data$key) , c(paste(tmp.group,".pred",sep="")) ]   <- as.character(tmp.pred)
}

##------------------------------------------------------------------
## Split the results into the proper columns
##------------------------------------------------------------------
for (i in 1:7) {
    
    if (LETTERS[i] == "A") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$AF.pred,1,1))
    } else if (LETTERS[i] == "B") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$BE.pred,1,1))
    } else if (LETTERS[i] == "C") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$CD.pred,1,1))
    } else if (LETTERS[i] == "D") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$CD.pred,2,2))
    } else if (LETTERS[i] == "E") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$BE.pred,2,2))
    } else if (LETTERS[i] == "F") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$AF.pred,2,2))
    } else if (LETTERS[i] == "G") {
        pred.test[ , paste(LETTERS[i],".pred",sep="")] <- as.integer(substr(pred.test$G.pred,1,1))
    }
    
}


##******************************************************************
## Step 3: Consolidate the prediction(s)
##******************************************************************

## collapse individual predicions
pred.test$ABCDEFG.lq    <- apply(pred.test[, LETTERS[1:7]], 1, paste, collapse="")
pred.test$A_ONLY.pred   <- apply(pred.test[, c("A.pred", "B", "C", "D", "E", "F", "G")], 1, paste, collapse="")
pred.test$B_ONLY.pred   <- apply(pred.test[, c("A", "B.pred", "C", "D", "E", "F", "G")], 1, paste, collapse="")
pred.test$C_ONLY.pred   <- apply(pred.test[, c("A", "B", "C.pred", "D", "E", "F", "G")], 1, paste, collapse="")
pred.test$D_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D.pred", "E", "F", "G")], 1, paste, collapse="")
pred.test$E_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E.pred", "F", "G")], 1, paste, collapse="")
pred.test$F_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F.pred", "G")], 1, paste, collapse="")
pred.test$G_ONLY.pred   <- apply(pred.test[, c("A", "B", "C", "D", "E", "F", "G.pred")], 1, paste, collapse="")
pred.test$ABCDEFG.pred  <- apply(pred.test[, paste(LETTERS[1:7],".pred",sep="")], 1, paste, collapse="")

## collapse paired predictions
pred.test$CD_ONLY.pred   <- apply(pred.test[, c("A", "B", "C.pred", "D.pred", "E", "F", "G")], 1, paste, collapse="")
pred.test$AF_ONLY.pred   <- apply(pred.test[, c("A.pred", "B", "C", "D", "E", "F.pred", "G")], 1, paste, collapse="")
pred.test$BE_ONLY.pred   <- apply(pred.test[, c("A", "B.pred", "C", "D", "E.pred", "F", "G")], 1, paste, collapse="")


## isolate the last quote for each customer
sub.idx                 <- tapply(pred.test$shopping_pt, pred.test$customer_ID, function(x){ which(x == max(x)) })
sub.key                 <- paste(names(sub.idx),sub.idx,sep="_")

## create the subsetted data
pred.sub                <- pred.test[ which(pred.test$key %in% sub.key), ]


##******************************************************************
## Step 4: Construct submissions
##******************************************************************

lastquoted.sub  <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$ABCDEFG.lq) )

## single predictions
gbm_aonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$A_ONLY.pred) )


gbm_bonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$B_ONLY.pred) )

gbm_conly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$C_ONLY.pred) )

gbm_donly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$D_ONLY.pred) )

gbm_eonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$E_ONLY.pred) )

gbm_fonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$F_ONLY.pred) )

gbm_gonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$G_ONLY.pred) )

## paired predictions
gbm_cdonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$CD_ONLY.pred) )

gbm_afonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$AF_ONLY.pred) )

gbm_beonly.sub   <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$BE_ONLY.pred) )

## everything together
gbm_all.sub     <- data.frame(
                    customer_ID = as.integer(pred.sub$customer_ID),
                    plan = as.character(pred.sub$ABCDEFG.pred) )


##******************************************************************
## Step 5: Write submissions to file
##******************************************************************
#write.csv(lastquoted.sub, file="S002_lastquoted.csv", row.names=FALSE)
write.csv(gbm_aonly.sub,  file="S002_gbm_aonly.csv", row.names=FALSE)
#write.csv(gbm_bonly.sub,  file="S002_gbm_bonly.csv", row.names=FALSE)
write.csv(gbm_conly.sub,  file="S002_gbm_conly.csv", row.names=FALSE)
write.csv(gbm_donly.sub,  file="S002_gbm_donly.csv", row.names=FALSE)
#write.csv(gbm_eonly.sub,  file="S002_gbm_eonly.csv", row.names=FALSE)
write.csv(gbm_fonly.sub,  file="S002_gbm_fonly.csv", row.names=FALSE)
#write.csv(gbm_gonly.sub,  file="S002_gbm_gonly.csv", row.names=FALSE)
#write.csv(gbm_all.sub,    file="S002_gbm_all.csv", row.names=FALSE)

#write.csv(gbm_cdonly.sub, file="S002_gbm_cdonly.csv", row.names=FALSE)
#write.csv(gbm_afonly.sub, file="S002_gbm_afonly.csv", row.names=FALSE)
#write.csv(gbm_beonly.sub, file="S002_gbm_beonly.csv", row.names=FALSE)



##******************************************************************
## Results
##******************************************************************

## G-only       == 0.54021
## All          == 0.53967
## Last quoted  == 0.53793
## B-only       == 0.53739
## E-only       == 0.53733

## Next steps would be to check the CD (combined) results ... and then both CD & G (if CD makes an improvement)

##******************************************************************
## Post-mortem
##******************************************************************

## try to identify those customers that you think are either correct or wrong

##------------------------------------------------------------------
## single parameter changes ... but really only the g choice was modeled
## in isolation ... as the others were part of a modeled pair (AF, CD, BE)
##------------------------------------------------------------------

## gmb_bonly (of these 138 differences a net of -9 are correct)
#tmp.sub             <- cbind(lastquoted.sub, gbm_bonly.sub$plan)
#colnames(tmp.sub)   <- c("customer_ID", "lq", "b_only")
#diff.b_only         <- subset(tmp.sub, as.character(lq) != as.character(b_only))

## gmb_eonly (of these 141 differences a net of -10 are correct)
#tmp.sub             <- cbind(lastquoted.sub, gbm_eonly.sub$plan)
#colnames(tmp.sub)   <- c("customer_ID", "lq", "e_only")
#diff.e_only         <- subset(tmp.sub, as.character(lq) != as.character(e_only))

## gmb_gonly (of these 902 differences a net of +38 are correct)
tmp.sub             <- cbind(lastquoted.sub, gbm_gonly.sub$plan)
colnames(tmp.sub)   <- c("customer_ID", "lq", "g_only")
diff.g_only         <- subset(tmp.sub, as.character(lq) != as.character(g_only))

##------------------------------------------------------------------
## paired parameter changes
##------------------------------------------------------------------

## gmb_gonly (of these 938 differences a net of -12 are correct)
tmp.sub             <- cbind(lastquoted.sub, gbm_afonly.sub$plan)
colnames(tmp.sub)   <- c("customer_ID", "lq", "af_only")
diff.af_only         <- subset(tmp.sub, as.character(lq) != as.character(af_only))

## gmb_gonly (of these 684 differences a net of -0 are correct)
tmp.sub             <- cbind(lastquoted.sub, gbm_cdonly.sub$plan)
colnames(tmp.sub)   <- c("customer_ID", "lq", "cd_only")
diff.cd_only         <- subset(tmp.sub, as.character(lq) != as.character(cd_only))

## gmb_gonly (of these 141 differences a net of -9 are correct)
tmp.sub             <- cbind(lastquoted.sub, gbm_beonly.sub$plan)
colnames(tmp.sub)   <- c("customer_ID", "lq", "be_only")
diff.be_only         <- subset(tmp.sub, as.character(lq) != as.character(be_only))

## gbm_all (of these 2263 are correct)
tmp.sub             <- cbind(lastquoted.sub, gbm_all.sub$plan)
colnames(tmp.sub)   <- c("customer_ID", "lq", "all")
diff.all            <- subset(tmp.sub, as.character(lq) != as.character(all))







