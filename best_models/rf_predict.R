source("../set_environment.R", echo=F)
source("get_mfcc_features.R", echo=F)

setwd("models/")

rf.fit <- readRDS("bm.rf.form1.th1e7.rds")

args <- commandArgs(trailingOnly=TRUE)

predict_rf <- function( filepath){
    frames_feat <- get_features( filepath, thrs=1e7)
    pred_frames <- predict( rf.fit, frames_feat)
    pred <- ifelse( mean(pred_frames=="machine") > 0.5, "machine", "human")
    return(pred)
}


for(i in 1:length(args)){
    pred <- predict_rf( args[i])
    print( paste0(args[i], " : ", pred))

}

