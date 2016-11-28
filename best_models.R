source("set_environment.R")
source("get_metadata.R")
source("get_mfcc_features.R")
source("utils.R")

## BEST MODEL CHOSEN

### QDA - FORMULA 1 - THRESHOLD 1e9
### RANDOM FOREST - FORMULA 1 - THRESHOLD 1e7
train <- get_features( audios[, path], thrs=1e9)
bm.qda <- qda( eval(parse(text=form.1)), train)

train <- get_features( audios[, path], thrs=1e7)
bm.rf <- randomForest( eval(parse(text=form.1)), train)


predict_qda <- function( test_path, model=bm.qda){
    test_feat <- get_features( test_path, thrs=1e9)
    preds <- predict( model, test_feat)
    new_pred <- ifelse( mean(preds$class=="machine") > 0.5, "machine", "human")
    return(new_pred)
}


predict_rf <- function( test_path, model=bm.rf){
    test_feat <- get_features( test_path, thrs=1e7)
    preds <- predict( model, test_feat)
    new_pred <- ifelse( mean(preds=="machine") > 0.5, "machine", "human")
    return(new_pred)
}

testdir_set <-  c("./audit_newrecords_2/cutted_carrier_message/",
                  "./audit_newrecords_2/cutted_human_answer/",
                  "./audit_newrecords_2/cutted_unknown/")

testfiles <- rbindlist( lapply(testdir_set, get_metadata))

testfiles[, "pred_qda":=sapply( testfiles[, path], predict_qda)]
testfiles[, "pred_rf":=sapply( testfiles[, path], predict_rf)]

testfiles[, table(source, pred_qda)]
testfiles[, table(source, pred_rf)]

testfiles[, list("acc_qda" = mean(source==pred_qda), "acc_rf" = mean(source==pred_rf))]

