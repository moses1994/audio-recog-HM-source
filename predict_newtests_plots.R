source("set_environment.R")

library(ggplot2)

newtests_err <- data.table(readRDS("newtests_err.rds"))
newtests_err[, threshold:=as.factor(threshold)]
newtests_err[, "featset":=paste0(formula,"::",threshold)]

###########################################
############ PLOT RESUTLS
###########################################
library(ggplot2)
  
# Error Rate Range by Method
ggplot(newtests_err, aes(x=method, y=err.rate, na.rm=T)) +
    geom_boxplot(aes(fill=method)) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Method")
    

# Error Rate Range by Formula
ggplot(newtests_err, aes(x=formula, y=err.rate, na.rm=T)) +
    geom_boxplot(aes(fill=formula)) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Formula")

ggplot(newtests_err, aes(x=formula, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=formula), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Formula") +
    facet_wrap(~ method)

## The formulas from 1 to 3 increase the number of predictors.
### to 'KNN' it does a large difference, it worsens until 30%.
### to 'LDA' it doesn't change the accuracy
### to 'RF', it enhances a bit the model, until 5%



# Error Rate Range by Threshold
ggplot(newtests_err, aes(x=threshold, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=threshold), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Threshold")

ggplot(newtests_err, aes(x=threshold, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=threshold), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Threshold") +
    facet_grid(~ method)

ggplot(newtests_err, aes(x=threshold, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=method, shape=method), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Threshold") +
    facet_grid(~ formula)

## The threshold doesn't seem to change so much the performance, except by  
### the 'Random Forest', which has its best performances on threshold = 1e7, and
### the 'QDA', which has the best group performance with the highest threshold, 1e9. 

# Error Rate Range by Featset
ggplot(newtests_err, aes(x=featset, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=featset), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    labs(title="Error Rate Range by Featset") +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle=45))

ggplot(newtests_err, aes(x=featset, y=err.rate, na.rm=T)) +
    geom_bar(aes(fill=featset), stat="identity", position=position_dodge()) +
    scale_y_continuous(breaks=seq(0,.55,.05), limits=c(0.0,.55)) + 
    scale_x_discrete() +
    labs(title="Error Rate Range by Featset") +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle=90)) +
    facet_wrap(~ method)



############################################
## THE TOP-12 MODELS 
############################################
top12 <- newtests_err[order(err.rate, decreasing=F)][1:12]

top12[, table(method)]
# knn.cv    qda     rf 
# 3      3      6 

top12[, table(formula)]
# form.1 form.2 form.3 
# 7      3      2 

top12[, table(threshold)]
# 0 1e+06 1e+07 1e+08 1e+09 
# 0     1     5     4     2 


ggplot(top12, aes(x=threshold, y=err.rate, na.rm=T)) +
    geom_point(aes(colour=method, shape=method), size=3) +
    scale_y_continuous(breaks=seq(0,.55,.025), limits=c(0.1,.2)) + 
    labs(title="Best 12 Models - Error Rate Range by Featset") +
    facet_grid(~ formula)


ggplot(top12, aes(x=featset, y=err.rate, na.rm=T)) +
    geom_bar(aes(fill=method), stat="identity", position="dodge", width=.5) +
    scale_y_continuous(breaks=seq(0,.55,.025), limits=c(0,.2)) + 
    labs(title="Best 12 Models - Error Rate Range by Featset") +
    theme_minimal()
    



##############################################
## BEST MODEL CHOSEN

### QDA - FORMULA 1 - THRESHOLD 1e9
### RANDOM FOREST - FORMULA 1 - THRESHOLD 1e7










