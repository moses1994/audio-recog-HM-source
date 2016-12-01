##------------------------------------------------------------------------------------------------
##------------------------------ EXTRACT MFCC FEATURES -------------------------------------------
##------------------------------------------------------------------------------------------------


#' Get MFCC and derivates from an audio sample.
#' 
#' @param filepath the path to audio sample
#' @param thrs the power on a frame required
#' @return data.frame with frame features from audio sample
#' 
get_mfcc <- function( filepath, thrs){
    x_label = unlist(strsplit(basename(filepath), "_"))[1]
    
    x_snd = readWave( filepath)
    x_mfcc = melfcc( x_snd)
    
    x_mfcc <- x_mfcc
    # apply threshold if needed
    if (thrs > 0){
        pow_spec <- powspec(x_snd@left)
        pot_frames <- apply(pow_spec, 2, FUN=function(x){ sum(x)})
    
        logical <- pot_frames>thrs
        x_mfcc <- x_mfcc[logical,]
    }
    
    x_deltas =deltas( x_mfcc)
    x_ddeltas =deltas( x_deltas)
    
    x_ts = seq(0, (nrow(x_mfcc)-1)*0.01, 0.01)
    
    return(
        data.frame("ts"=x_ts, "label"=x_label,"mfcc"=x_mfcc, "delta"=x_deltas, "ddelta"=x_ddeltas)
    )
}

get_features <- function(files, thrs=0){
  features = do.call("rbind", lapply(files, FUN=get_mfcc, thrs))
  return( features)
}
