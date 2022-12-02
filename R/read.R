##' read.input
##' @param x filename
##' @details Reads in data and metadata input of a "predation input file"
##' @export
read.input <- function(x){
    tab <- read.table(x,skip=11)
    names(tab) <- read.table(x,skip=10)[1,]
    
    m <- readLines(x,n=9)
    meta <- as.list(gsub(".*: ","",m))
    names(meta) <- sapply(gsub(": .*","",m), "[[", 1)
    
    attr(tab,"meta") <- meta
    tab
}

##' read.predator
##' @param x directory
##' @param verbose logical
##' @details Reads in all pred txt input files or Rdata (booted already) in a directory (excluding subdirectories). A Predator is defined as species - population - life stage. So for instance bird chicks are a predator, whereas just birds are not.
##' @export
read.predator <- function(x,verbose=TRUE){
    f <- list.files(x,full.names = T,recursive = FALSE)
    dat <- lapply(f,function(x){if(verbose)print(x);if(grepl('txt',x))read.input(x) else get(load(x))})
    names(dat) <- gsub(".Rdata","",gsub(".txt","",list.files(x)))
    class(dat) <- c("predator")
    return(dat)
}

##' write.input
##' @param x data
##' @param file filename
##' @param titel header
##' @param species species
##' @param stage stage
##' @param pop population
##' @param region region
##' @param source source
##' @param citedby cited by
##' @param unit unit
##' @param notes notes
##' @details writes a predator input file.
##' @export
write.input <- function(x,file,title,species=NA,stage=NA,pop=NA,region=NA,source=NA,citedby=NA,unit=NA,notes=NA){
    header <- data.frame(input=c(title,
                                 paste("species: ",species),
                                 paste("life stage: ",stage),
                                 paste("population: ",pop),
                                 paste("region: ",region),
                                 paste("source: ",source),
                                 paste("cited by: ",citedby),
                                 paste("unit: ",unit),
                                 paste("notes: ",notes),
                                 ""))
    write.table(header,file,quote = FALSE,row.names = FALSE,col.names=FALSE,append=FALSE)
    write.table(x,file,quote = FALSE,row.names = FALSE,sep = '\t',append=TRUE)
}
