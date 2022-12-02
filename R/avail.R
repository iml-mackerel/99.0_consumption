##' cavail.predator
##' @param wd working directory
##' @details working directory needs to contain hierarchical file structure so that (wd/predator-name/population-name/lifestage-name/data-name.txt).
##' @importFrom reshape2 dcast
##' @export
avail.predator <-function(wd){
    predator.list <- list.files(wd,recursive = TRUE)
    info <- strsplit(predator.list,'/')
    if(any(sapply(info,length)<4)){warning(paste("this working directory is missing information (wd should be: ./predator/population/lifestage/dataname.txt ----",info[[which(sapply(info,length)<4)]][1]))}
    info <- do.call(rbind.data.frame, info)
    names(info) <- c('species','population','lifestage','data')
    info$data <- gsub('.Rdata','',gsub('.txt','',info$data))
    info <- reshape2::dcast(info,species+population+lifestage~data,length,value.var = 'data')
    info$wd <- paste(wd,info$species,info$population,info$lifestage,sep='/')
    info$id <- apply(info[,1:3],1,paste,collapse=".")
    return(info)
}
