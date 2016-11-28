##------------------------------------------------------------------------------------------------
##------------------------------ UTILITIES -------------------------------------------------------
##------------------------------------------------------------------------------------------------


##----------------------------------------------------------------------------------------------

## ASSIGN GLOBAL VARIABLES

##  the formulas 'response ~ predictors' that are gonna be used through the modelings
form.1 = "label ~ mfcc.1+mfcc.2+mfcc.3+mfcc.4+mfcc.5+mfcc.6+ mfcc.7+mfcc.8+mfcc.9+mfcc.10+mfcc.11+mfcc.12"
form.2 = paste0("label ~ ", 
                paste0(paste0("mfcc.",1:12), collapse=" + "), 
                paste0(" + ", paste0(paste0("delta.",1:12), collapse=" + ")))
form.3 = paste0("label ~ ", 
                paste0(paste0("mfcc.",1:12), collapse=" + "), 
                paste0(" + ", paste0(paste0("delta.",1:12), collapse=" + ")),
                paste0(" + ", paste0(paste0("ddelta.",1:12), collapse=" + ")))




#----------------------------------------------------------------------------------------------
#------------------------------ MODELING FUNCTIONS --------------------------------------------
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#' Classify the "source" class of the file
#' 
#'   Useful to Discriminant Analysis Models (LDA and QDA) and
#'   the Random Forest Model
#' 
#' @param testfile Path to the file
#' @param model The model responsible for the classification
#' @param thrs minimum threshold spected by frame power
#' @return the prediction label, "machine" or "gender"
#' 
get_prediction <- function(testfile, model, thrs){
    
    testfeat <- get_features(testfile, thrs)
    pred <- predict( model, newdata=testfeat)
    
    pred.label <- NA
    if( sum( class(model) %in% c("lda", "qda"))){
        pred.label <- ifelse( mean( pred$class=="machine")>.5, "machine", "human")
        # print( table(pred$class))
    }
    if( sum( class(model) %in% c("randomForest"))){
        pred.label <- ifelse( mean( pred=="machine")>.5, "machine", "human")
        # print( pred)
    }
    return( pred.label)
}


#----------------------------------------------------------------------------------------------
#' Classify the "source class of the file for the KNN models
#' 
#' @param testfile fullpath of the file to be tested
#' @param model the data set used as the neighbors
#' @param k.cv a vector of which 'k's are to be used
#' @param form the predictor to be applied
#' @param thrs minimum threshold spected by frame power
#' @return a vector with the test error rates for each selected 'k'
#' 
get_prediction_knn <- function(testfile, model, k.cv, form, thrs){

    testfeat <- get_features(testfile, thrs)
    
    # it is a monster, i know, but it works. It's simpler let it as is than make a simpler code.
    ## it descontruct the quote formula to a vector with the colnames that i want get from dataset
    ## it was needed by the KNN quirk
    form.knn <- unlist(strsplit(gsub(unlist(strsplit(form, '~'))[-1], pattern=" ", replacement=""), "\\+"))
    
    # apply a prediction for a specific 'k' value
    apply_knn_cv <- function(k){
        knn.pred <- knn(train=model[, form.knn], test=testfeat[, form.knn], cl=model$label, k=k)
        knn.pred.label <- ifelse( mean( knn.pred=="machine")>.5, "machine", "human")
        #print( knn.pred.label)
    }
    
    test.err.knn.cv <- sapply(k.cv, apply_knn_cv)
    names(test.err.knn.cv) <- paste0("k",k.cv)
    
    return(test.err.knn.cv)
}


#----------------------------------------------------------------------------------------------
#' Evaluate the test error based on the cross-validation n-fold method
#' 
#'   Previously, a 'audio' dataset is marked with a column 'fold'. The function computed
#'   a model using (n-1) fold, afterwards computes the error rate on the ramainder fold
#'   
#' @param fold_nr the fold number separated as test set
#' @param form the 'response ~ predictor' formula
#' @param technique which method to be used: lda, qda, rf, knn.cv
#' @param k.cv argument only needed by 'knn.cv', vector with the 'k's wanted
#' @return the error rate for the test case
#'
get_fold_error <- function(fold_nr, form, technique, k.cv, thrs=0){
    
    # the train set is the features from all frames of all samples mixtured
    train <- get_features( audios[fold_nr!=fold, path], thrs)
    
    # the test set is, at the moment, only the path of the sample files
    test <- audios[fold_nr==fold, .("label"=source, path, "pred"=NA)]
    
    # it's created a model variable, which represent the fitting for the chosen method
    switch( technique,
            "lda" = { model <- lda( eval(parse(text=form)), data=train)},
            "qda" = { model <- qda( eval(parse(text=form)), data=train)},
            "rf"  = { model <- randomForest( eval(parse(text=form)), data=train)},
            "knn.cv" = { model <- train}
    )
    
    
    # computes the predictions over the test fold
    ## the "KNN-CV" has a special session of code, because its particularity of be many models at once.
    ## each "K number" has its own non-parametrical model
    if( technique %in% c("lda", "qda", "rf")){
        test$pred <-lapply(X=test[, path], get_prediction, model, thrs)
        err.rate <- test[, mean(label!=pred)]
        
        return(err.rate)
    }
    if( technique %in% c("knn.cv")){
        # get results for various "k"
        pred.knn = lapply(test[, path], get_prediction_knn, model, k.cv, form, thrs)
        # adequate the correct order of the results and transform it in a data.table
        pred.knn <- data.table( matrix( unlist(pred.knn), byrow=T, ncol=length(k.cv)))
        names(pred.knn) <-  paste0("pred_k",k.cv)
        
        # apply mean function to compare all columns of K's
        err.rate <- apply(X = pred.knn, 2, function(x){ mean(test$label!=x)})
        return(err.rate)
    }
    else{ 
        warning(paste0(technique, " IS NOT A VALID OPTION"))
        return(NA)
    }
    
    
}


