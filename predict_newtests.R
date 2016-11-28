source("set_environment.R")
source("get_metadata.R")
source("get_mfcc_features.R")
source("utils.R")

dir = "./audit_newrecords/answering_machine_cutted/"
test.files = get_metadata(dir)


get_newtest_error <- function(technique, form, thrs=0, train, k.cv=4){
    # If is assigned to QDA any formula, except 'form1', we bypass the function
    if( technique=="qda" & form!=form.1){
        return(NA)    
    }
    
    
    # the test set is, at the moment, only the path of the sample files
    test <- test.files
    test$pred <- ""
    
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
        err.rate <- test[, mean(source!=pred)]
        
        return(err.rate)
    }
    if( technique %in% c("knn.cv")){
        # get results for various "k"
        pred.knn = lapply(test[, path], get_prediction_knn, model, k.cv, form, thrs)
        # adequate the correct order of the results and transform it in a data.table
        pred.knn <- data.table( matrix( unlist(pred.knn), byrow=T, ncol=length(k.cv)))
        
        # apply mean function to compare all columns of K's
        err.rate <- apply(X = pred.knn, 2, function(x){ mean(test$source!=x)})
        return(err.rate)
    }
    else{ 
        warning(paste0(technique, " IS NOT A VALID OPTION"))
        return(NA)
    }
    
}



get_method_error <- function(tech_set, form, thrs, train){
    err.rate <- sapply(tech_set, get_newtest_error, form, thrs, train)
    return(
        data.table(
            "method" = tech_set,
            "formula" = ifelse(form==form.1, "form.1", ifelse(form==form.2, "form.2", "form.3")),
            "threshold" = thrs,
            "err.rate" = err.rate
        ))
}

get_form_method_error <- function(tech_set, form_set, thrs){
    # the train set is the features from all frames of all samples mixtured
    train <- get_features( audios[, path], thrs)
    rbindlist(
        lapply(form_set, get_method_error, tech_set=tech_set, thrs, train)
    )
}

get_thrs_form_method_error <- function(tech_set, form_set, thrs_set){
    rbindlist(
        lapply(thrs_set, get_form_method_error, tech_set=tech_set, form_set=form_set)
    )
}



tech_set <- c("lda", "qda", "rf", "knn.cv")
form_set <- c(form.1, form.2, form.3)
names(form_set) <- c("form.1", "form.2", "form.3")
thrs_set <- c(0,1e6,1e7,1e8,1e9)

newtests_err <- get_thrs_form_method_error(tech_set, form_set, thrs_set)
saveRDS(newtests_err, file = "newtests_err_2.rds")

