source("../set_environment.R")
source("get_metadata.R")
source("get_mfcc_features.R")
source("utils.R")



## THIS SCRIPT ATTEMPS TO CREATE THE BEST MODEL SET
### BUT THIS TIME, USING THE WHOLE DATA, EVEN THE ONE USED AS THE FIRST TEST SET
### NOW, I'LL TEST THESE MORE "ROBUST" MODELS IN A NEW TESTSET

addr <- paste0(normalizePath(list.dirs("./data/general/", recursive = T)[-1],"/"))
# capture all available files
audios <- rbindlist( lapply( addr, FUN=get_metadata))


### QDA - FORMULA 1 - THRESHOLD 1e9
train <- get_features( audios[, path], thrs=1e9)
bm.qda.1 <- qda( eval(parse(text=form.1)), train)
saveRDS(bm.qda.1, "./create_general_best_models/models/bm.qda.form1.th1e9.rds")
#bm.qda <- readRDS("./create_general_best_models/models/bm.qda.form1.th1e9.rds")

train <- get_features( audios[, path], thrs=0)
bm.qda.2 <- qda( eval(parse(text=form.1)), train)
saveRDS(bm.qda.2, "./create_general_best_models/models/bm.qda.form1.th0.rds")

### RANDOM FOREST - FORMULA 1 - THRESHOLD 1e7
train <- get_features( audios[, path], thrs=1e7)
bm.rf.1 <- randomForest( eval(parse(text=form.1)), train)
saveRDS(bm.rf.1, "./create_general_best_models/models/bm.rf.form1.th1e7.rds")
#bm.rf <- readRDS("./create_general_best_models/models/bm.rf.form1.th1e7.rds")

train <- get_features( audios[, path], thrs=0)
bm.rf.2 <- randomForest( eval(parse(text=form.1)), train)
saveRDS(bm.rf.2, "./create_general_best_models/models/bm.rf.form1.th0.rds")

