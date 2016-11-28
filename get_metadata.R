#------------------------------ GET DATA --------------------------------------------


#----------------------------------------------------------------------------------------------
#' Captures files information: labels and fullpath
#' 
#' @param addrx Directory address where are the files
#' @return A list with the file labels (gender and source) and its fullpath
#' 
get_metadata <- function(addrx){
    files <- list()
    
    files$names <- list.files(addrx)
    files$labels <- matrix(unlist(strsplit(files$names, split="_")), ncol=3, byrow=T)[, 1:2]
    files$fullpath <- paste0(addrx, files$names)
    
    dt <- data.table(files$labels, files$fullpath)
    names(dt) <- c("source", "gender", "path")
    
    return(dt)
}

#----------------------------------------------------------------------------------------------
## ASSIGN GLOBAL VARIABLES
# get the four data directories 
addr <- paste0(normalizePath(list.dirs("./data", recursive = T)[2:5]),"/")

# capture all available files
audios <- rbindlist( lapply( addr, FUN=get_metadata))

# assign each sample into folds
nfold = 5
audios[, "fold":=( (as.numeric(row.names(.SD)) - 1)%%nfold + 1)]

