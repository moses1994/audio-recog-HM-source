source("../set_environment.R", echo=F)
source("get_mfcc_features.R", echo=F)

setwd("./best_models/")

qda.fit <- readRDS("bm.qda.form1.th1e9.rds")

args <- commandArgs(trailingOnly=TRUE)

predict_qda <- function( filepath){
    frames_feat <- get_features( filepath, thrs=1e9)
    pred_frames <- predict( qda.fit, frames_feat)
    pred <- ifelse( mean(pred_frames$class=="machine") > 0.5, "machine", "human")
    return(pred)
}


for(i in 1:length(args)){
    pred <- predict_qda( args[i])
    print( paste0(args[i], " : ", pred))
    
}

