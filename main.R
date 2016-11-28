##------------------------------------------------------------------------------------------------
##----------------------- CAPTURE THE GENERAL ERROR RATES ----------------------------------------
##------------------------------------------------------------------------------------------------

source("set_environment.R")
source("get_metadata.R")
source("get_mfcc_features.R")
source("utils.R")


#----------------------------------------------------------------------------------------------
#' For a specific power frame threshold and formula ('delta'), it computes the mean error and
#'  its standard deviation based on cross-validation n-fold process
#'  
#'  In one turn, it computes the error rates for the methods: 'LDA', 'QDA', 'RF' and 'KNN'
#'  
#' @param thrs the threshold power require for a frame.
#' @param delta if the formula must contain the MFCC derivatives or not
#' @return a data table of the error rates of all methods
#' 
get_test_err_delta <- function(delta=" ", thrs=0){
    
    test.err <- data.table(method=c("lda", "rf"), "err.rate.mu"=0, "err.rate.sd"=0)
    
    # As 'qda' only works without the derivatives, it has an exception to be added only in this situation
    if (delta == " "){
        test.err <- rbindlist(list(test.err, data.frame("qda", 0, 0)))
    }
    
    # decide which formula use based if it contains or not the MFCC derivativs
    form <- switch(delta,
                   " " = form.1,
                   "d" = form.2,
                   "dd" = form.3)
            
    
    # compute LDA method
    test.err.lda <- sapply(1:nfold, get_fold_error, form, "lda", thrs)
    test.err[method=="lda", c("err.rate.mu", "err.rate.sd") := list(mean(test.err.lda), sd(test.err.lda))] 
    
    # compute QDA method
    # the 'qda' quirk
    if (delta == " "){
        test.err.qda <- sapply(1:nfold, get_fold_error, form, "qda", thrs)
        test.err[method=="qda", c("err.rate.mu", "err.rate.sd") := list(mean(test.err.qda), sd(test.err.qda))] 
    }
    
    # compute Random Forest method
    test.err.rf <- sapply(1:nfold, get_fold_error, form, "rf", thrs)
    test.err[method=="rf", c("err.rate.mu", "err.rate.sd") := list(mean(test.err.rf), sd(test.err.rf))] 
    
    # compute KNN method
    ## determine the 'k', number of neighboors
    k.cv = c(1:5, 10, 15, 20, 25)
    test.err.knn <- data.table("method"=paste0("knn-", k.cv), "err.rate.mu"=0, "err.rate.sd"=0) 
    
    test.err.knn.cv <- sapply(1:nfold, get_fold_error, form, "knn.cv", k.cv, thrs)
    test.err.knn[, c("err.rate.mu", "err.rate.sd") :=
                     list(apply(test.err.knn.cv, 1, mean), apply(test.err.knn.cv, 1, sd))] 
    
    
    test.err <- rbindlist(list(test.err, test.err.knn))
    
    # add the test-error metadata
    test.err$mfcc <- TRUE
    test.err$delta <- ifelse( delta %in% c("d", "dd"), TRUE, FALSE)
    test.err$ddelta <- ifelse( delta=="dd", TRUE, FALSE)
    test.err$thrs <- thrs
    
    return( test.err)

}


#' Computes the error rates for a formula with MFCC and
#'  no derivate, or first derivative, or until second derivative
#'  
#'  @param thrs the minimum power frame required
#'  @return a data table with the results for all option of formulas
#'  
get_test_err_formulas <- function(thrs=0){
    delta_set = c(" ", "d", "dd")
    return(
        rbindlist( lapply(delta_set, get_test_err_delta, thrs))
        )
}


#' Computes the test error rate for all options of formulas 
#'  for a threshold of set.
#'  
#'  The threshold intends to avoid the frames of silence in a 
#'  conversation
#'  
#'  @return a data table with all required test error rat.es
#'  
get_test_err_thrs <- function(thrs_set){
    return(
        rbindlist( lapply(thrs_set, get_test_err_formulas))
        )
}

thrs_set = c(0,1e6,1e7,1e8,1e9,1e10)
test.err <- get_test_err_thrs(thrs_set)

saveRDS(test.err, "general_test_err_5folds.rds")
