##' consum.predator
##' @param x bootstrapped predator object
##' @details Calculate predator consumption of prey in tonnes.
##' @export
consum.predator <- function(x){
    if(all(c('alpha','beta','W') %in% names(x))) x$DEE <- dee.kleiber(x)
    if(all(c('DEE','CC') %in% names(x))) x$DI <- with(x,DEE/CC*ifelse('AE' %in% names(x),AE,1)) # DI in grams
    if(all(c('W','PBM') %in% names(x))) x$DI <- with(x,W*(PBM/100)*1000)   # kg to gram
    if(all(c('DI','DAY') %in% names(x))) x$TI <- with(x,DI*DAY)
    if(all(c('N','Nfrac') %in% names(x))) x$N <- with(x,N*Nfrac) 

    if(all(c('TI','P','N') %in% names(x))) x$C <- with(x, TI*P*N)/1000000 else{warning('insufficient information to estimate consumption');return(NULL)} 
    
    return(x)
}
