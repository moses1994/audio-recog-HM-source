source("set_environment.R")

test.err = readRDS("general_test_err.rds")
test.err[, "mmethod":=ifelse( grepl("knn", method), "knn", as.character(method))]
test.err$form <- ""
test.err[mfcc==TRUE & delta==FALSE & ddelta==FALSE, form:="form1"]
test.err[mfcc==TRUE & delta==TRUE & ddelta==FALSE, form:="form2"]
test.err[mfcc==TRUE & delta==TRUE & ddelta==TRUE, form:="form3"]

# plot
library(ggplot2)


# capture the error rate distribution for QDA, LDA, RF and KNN(k=4) algorithms
dt <- test.err[mmethod!="knn" | method=="knn-4", .(method, "feat_set"=paste0(form, "::",thrs), 
                    form,  "thrs"=as.factor(thrs), err.rate.mu)]
dt[, "singular":=paste0(method, "::", feat_set)]



# compare general performance of the formulas
ggplot(dt, aes(x=form, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=form)) +
    scale_y_continuous(breaks=seq(0,.4,.05)) +
    facet_wrap(~ method)

## KNN for far was the worst approach

# i took off the knn for a better measurement of the other methods
dt <- dt[method!="knn-4"]

# Error rate by Methods
ggplot(dt, aes(x=method, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=method)) +
    labs(x="Method", title="Error Rate Range")


# Error rate by Formulas
ggplot(dt, aes(x=form, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=form)) +
    scale_y_continuous(breaks=seq(0,.05,.005)) +
    scale_fill_discrete(labels=c("MFCC", "MFCC+DELTA", "MFCC-DDELTA")) +
    labs(x="Formula", title="Error Rate Range")

ggplot(dt, aes(x=form, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=form)) +
    scale_y_continuous(breaks=seq(0,.05,.005)) +
    scale_fill_discrete(labels=c("MFCC", "MFCC+DELTA", "MFCC-DDELTA")) +
    labs(x="Formula", title="Error Rate Range") +
    facet_wrap(~ method)

## The formulas 2 and 3 got the best minimum to the Random Forest method.
## QDA presented a good performance for 'form1'


# Error rate by Power Threshold
ggplot(dt, aes(x=thrs, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=thrs)) +
    scale_y_continuous(breaks=seq(0,.05,.005)) +
    labs(x="Power Threshold", title="Error Rate Range")

ggplot(dt, aes(x=thrs, y=err.rate.mu)) + 
    geom_boxplot(aes(fill=thrs)) +
    labs(x="Power Threshold", title="Error Rate Range") + 
    scale_y_continuous(breaks=seq(0,.05,.005)) +
    facet_wrap(~ method)

## The Threshold presents a very similar performance


# Error rate by Formula and Threshold combination
ggplot(dt, aes(x=feat_set, y=err.rate.mu)) +
    geom_boxplot(aes(fill=feat_set)) +
    scale_y_continuous(breaks=seq(0,0.05,0.005), limits=c(0,0.05)) +
    labs(x="Feature Set", title="Error Range for Each Feature Set") +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle=45))


ggplot(dt, aes(x=feat_set, y=err.rate.mu)) +
    geom_boxplot(aes(fill=feat_set)) +
    scale_y_continuous(breaks=seq(0,0.05,0.005), limits=c(0,0.05)) +
    labs(x="Feature Set", title="Error Range for Each Feature Set") +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle=45)) +
    facet_wrap(~ method)

## The error rates were so small that i'll disregard any analysis. The range between [0.01 and 0.05] of accuracy is too small
## to this value be considered distinct one of the other