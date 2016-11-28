##------------------------------------------------------------------------------------------------
##------------------------------ SET ENVIRONMENT -------------------------------------------------
##------------------------------------------------------------------------------------------------

# set workdir
setwd("~/workspace/khomp/lab-recog-HM/")

# set randomness
set.seed(1)

# modeling classes
library(MASS)
library(class)
library(randomForest)

# data.table
library(data.table)

# audio techniques classes
library(seewave)
library(tuneR)
