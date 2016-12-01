source("~/workGit/audio-recog-HM-source/set_environment.R")
source("get_mfcc_features.R")

library(reshape2)
library(ggplot2)

frame.step = 0.01

files <- data.table("path"=list.files("./plots_mfcc/data", full.names = T))
files[, "source":=ifelse(grepl(path, pattern="machine"), "machine", "human")]
files[, "gender":=ifelse(grepl(path, pattern="female"), "female", "male")]



plot_mfcc_obj <- function(dt){
    g <- ggplot(dt) + geom_raster(aes(x=ts, y=mfcc, fill=coef), hjust=0, vjust=0) +
            scale_fill_gradientn(colours=c("red", "yellow", "green","blue", "darkblue"), 
                                 breaks=seq(-50,125,25), labels=format(seq(-50,125,25)), limits=c(-50,125),
                                 name="") +
            labs(x="time", y="") +
            theme_minimal() +
            facet_wrap(~ basename)
    
    return(g)
}


plot_mfcc <- function(dt){
  plot(
      ggplot(dt) + geom_raster(aes(x=ts, y=mfcc, fill=coef), hjust=0, vjust=0) +
          scale_fill_gradientn(colours=c("red", "yellow", "green","blue", "darkblue"), 
                               breaks=seq(-50,125,25), labels=format(seq(-50,125,25)), limits=c(-50,125),
                             name="") +
          labs(x="time", y="") +
          theme_minimal() +
          facet_grid(source + gender ~ basename)
  )
}


make_dataset <- function( filepath){
    source <- ifelse(grepl(filepath, pattern="machine"), "machine", "human")
    gender <- ifelse(grepl(filepath, pattern="female"), "female", "male")
    
    frames.mfcc <- data.table(get_mfcc(filepath, thrs=0), 
                              "basename"=basename(filepath),
                              "source"=source,
                              "gender"=gender)
    frames.mfcc[, "ts":=seq(1,nrow(.SD),1)*frame.step]
    frames.mfcc.m <- melt(frames.mfcc, id.vars=c("source", "gender", "basename","ts"), measure.vars=paste0("mfcc.",1:12), value.name="coef", variable.name="mfcc")
    return(frames.mfcc.m)
}


main <- function(file){
    return(plot_mfcc( make_dataset( file), basename(file)))
}


main(files[1])


library(cowplot)
p1 <- main_obj( files[source=="machine" & gender=="male", path][1])
p2 <- main_obj( files[source=="machine" & gender=="male", path][2])
p3 <- main_obj( files[source=="machine" & gender=="female", path][1])
p4 <- main_obj( files[source=="machine" & gender=="female", path][2])
p5 <- main_obj( files[source=="human" & gender=="male", path][1])
p6 <- main_obj( files[source=="human" & gender=="male", path][2])
p7 <- main_obj( files[source=="human" & gender=="female", path][1])
p8 <- main_obj( files[source=="human" & gender=="female", path][2])
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8)


############################################################################

# random sort to choose the classes
selected <- c(1,2,   
              11,12,
              22,23,
              36,37)

super.dt <- rbindlist( lapply(files[selected, path], make_dataset))
plot_mfcc(super.dt)




files.sub <- files[source=="machine" & gender=="female", path][1:3]
super.dt <- rbindlist( lapply(files.sub, make_dataset))
p1 <- plot_mfcc_obj(super.dt)

files.sub <- files[source=="machine" & gender=="male", path][1:3]
super.dt <- rbindlist( lapply(files.sub, make_dataset))
p2 <- plot_mfcc_obj(super.dt)

files.sub <- files[source=="human" & gender=="female", path][1:3]
super.dt <- rbindlist( lapply(files.sub, make_dataset))
p3 <- plot_mfcc_obj(super.dt)

files.sub <- files[source=="human" & gender=="male", path][1:3]
super.dt <- rbindlist( lapply(files.sub, make_dataset))
p4 <- plot_mfcc_obj(super.dt)

plot_grid(p1,p2,p3,p4)







