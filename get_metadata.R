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

