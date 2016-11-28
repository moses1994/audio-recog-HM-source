source("set_environment.R")

test.err = readRDS("general_test_err.rds")
test.err[, "mmethod":=ifelse( grepl("knn", method), "knn", as.character(method))]
test.err$form <- ""
test.err[mfcc==TRUE & delta==FALSE & ddelta==FALSE, form:="form1"]
test.err[mfcc==TRUE & delta==TRUE & ddelta==FALSE, form:="form2"]
test.err[mfcc==TRUE & delta==TRUE & ddelta==TRUE, form:="form3"]

# plot
library(ggplot2)


## DEALING WITH THE NON-PARAMETRICAL APPROACH K-NEAREST NEIGHBORS

# capture the error rate distribution of each k-tech
dt <- test.err[mmethod=="knn", .(method, "feat_set"=paste0(form, "::",thrs), 
                    form,  "thrs"=as.factor(thrs), err.rate.mu)]
dt[, "singular":=paste0(method, "::", feat_set)]
ggplot(dt, aes(x=err.rate.mu)) + 
    geom_density(aes(fill=method, colour=method), alpha=.05) +
    scale_x_continuous(breaks=seq(0,0.5,0.05)) +
    labs(title="KNN - Error Rate Distribution Along the 'Features X Threshold' Combinations", x="Mean Fold Error Rate")

ggplot(dt, aes(x=method, y=err.rate.mu)) +
    geom_boxplot(aes(fill=method)) +
    scale_y_continuous(breaks=seq(0,0.5,0.05)) +
    labs(title="KNN - Error Rate Distribution Along the 'Features X Threshold' Combinations", x="Mean Fold Error Rate")


# get the best feature set - formula - for KNN approach
ggplot(dt, aes(x=err.rate.mu)) +
    geom_density(aes(fill=form, colour=form), alpha=0.1) +
    scale_fill_discrete(name="Formula", labels=c("MFCC", "MFCC + DELTA", "MFCC + DDELTA")) +
    scale_colour_discrete(name="Formula", labels=c("MFCC", "MFCC + DELTA", "MFCC + DDELTA")) +
    scale_x_continuous(breaks=seq(0,0.5,0.05)) + 
    labs(title="KNN - Error Rate along the Distinct Formulas")

ggplot(dt, aes(y=err.rate.mu)) +
    geom_boxplot(aes(x=form, fill=form)) +
    scale_fill_discrete(name="Formula", labels=c("MFCC", "MFCC + DELTA", "MFCC + DDELTA")) +
    scale_y_continuous(breaks=seq(0,0.5,0.05)) + 
    labs(title="KNN - Error Rate along the Distinct Formulas")
## Formula "MFCC" seems to have the best approach for KNN!


# get the best threshold value for KNN approach
ggplot(dt, aes(x=err.rate.mu)) +
    geom_density(aes(fill=thrs, colour=thrs), alpha=0.075) +
    scale_fill_discrete(name="Threshold") +
    scale_colour_discrete(name="Threshold") +
    scale_x_continuous(breaks=seq(0,0.5,0.05)) + 
    labs(title="KNN - Error Rate along the Distinct Threshold")

ggplot(dt, aes(y=err.rate.mu)) +
    geom_boxplot(aes(x=thrs, fill=thrs)) +
    scale_fill_discrete(name="Threshold") +
    scale_y_continuous(breaks=seq(0,0.5,0.05)) + 
    labs(title="KNN - Error Rate along the Distinct Threshold")

# The minimum thresholds, 0 and 1e6, got the best results

## General Overview of KNN
ggplot(dt, aes(x=err.rate.mu)) +
    geom_histogram(aes(fill=feat_set), alpha=1, binwidth = 0.025) +
    scale_x_continuous(breaks=seq(0,.5,0.05)) +
    facet_wrap(~ method)

ggplot(dt, aes(x=err.rate.mu)) +
    geom_density(aes(fill=feat_set, colour=feat_set), alpha=0.075) +
    facet_wrap(~ feat_set)

# General overview of the performance for each combination of Formula and Threshold
ggplot(dt) + geom_boxplot(aes(x=feat_set, y=err.rate.mu, fill=feat_set)) +
    scale_y_continuous(breaks=seq(0,.5,.05), labels=seq(0,.5,.05), limits=c(0,.45)) +
    labs(x="Formula Combination", title="The Performance of Specific Combination between Formula and Threshold") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45)) 

ggplot(dt[order(err.rate.mu, decreasing = F)][1:10]) + geom_bar(aes(x=singular, y=err.rate.mu, fill=singular), stat="identity")


# GENERAL PERFORMANCE BY K-VALUE
ggplot(dt) + geom_boxplot(aes(x=method, y=err.rate.mu, fill=method)) +
    scale_y_continuous(breaks=seq(0,.4,.05), labels=seq(0,.4,.05)) +
    labs(x="K Neighbors", title="General Performance of the KNN models")

# GENERAL PERFORMANCE BY FEAT_SET AND METHOD
ggplot(dt) + geom_boxplot(aes(x=form, y=err.rate.mu, fill=form)) +
    scale_y_continuous(breaks=seq(0,.45,.05), labels=seq(0,.45,.05), limits=c(0,.45)) +
    labs(x="K Neighbors", title="General Performance of the KNN models") +
    facet_wrap(~ method)

## The formula 1 "MFCC" showed as the best in every K-neighbors


# GENERAL PERFORMANCE BY THRESHOLD AND METHOD
ggplot(dt) + geom_boxplot(aes(x=thrs, y=err.rate.mu, fill=thrs)) +
    scale_y_continuous(breaks=seq(0,.4,.05), labels=seq(0,.4,.05)) +
    labs(x="K Neighbors", title="General Performance of the KNN models") +
    facet_wrap(~ method)

## The small thresholds - 0 and 1e6 - got a best performance in every approachs



dt[order(err.rate.mu, decreasing = F)][1:10]


########################
## Based on Previous Results, i'm gonna constrain the dataset only to formula-1 and threshold of 0 and 1e6,
##  which were the best combinations
dt.2 <- dt[thrs%in%c(0,1e6) & form=="form1"]

ggplot(dt.2) + geom_bar(aes(x=feat_set, y=err.rate.mu, fill=method, width=.5), 
                        stat="identity", position=position_dodge(width=.75)) +
    scale_y_continuous(breaks=seq(0,.06,.01), limits=c(0,0.06)) +
    labs(x="Feature::Threshold Combination", title="Performance between the K approachs")

## The K=4 outperforms the other approachs