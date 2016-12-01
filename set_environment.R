##------------------------------------------------------------------------------------------------
##------------------------------ SET ENVIRONMENT -------------------------------------------------
##------------------------------------------------------------------------------------------------

# set workdir
#setwd("~/workGit/audio-recog-HM-source/")
setwd("~/workspace/audio-recog-HM-source/")

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
