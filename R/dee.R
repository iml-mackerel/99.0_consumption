##' dee.kleiber
##' @param x bootstrapped predator object
##' @details Get DEE (Dailt Energy Expenditure) from the Kleiber equation (alpha * W ^ beta) with corrections based on AF (Activity Factor), GP (Growth Premium) and ME (metabolisable ernergy)
##' @references Benoît, H., Swain, D., Bowen, W., Breed, G., Hammill, M., and Harvey, V. 2011. Evaluating the potential for grey seal predation to explain elevated natural mortality in three fish species in the southern Gulf of St. Lawrence. Marine Ecology Progress Series, 442: 149–167. http://www.int-res.com/abstracts/meps/v442/p149-167/
##' @export
dee.kleiber <- function(x){
    if(!all(c('alpha','beta','W') %in% names(x))) stop('need alpha and beta parameters and W')
    AF <- GP <- ME <- 1                          # default if not available
    with(x,alpha*W^beta*AF*GP/ME)
}

##' W.gompertz
##' @param x bootstrapped predator object
##' @details get weight from Gompertz equation
##' @references Benoît, H., Swain, D., Bowen, W., Breed, G., Hammill, M., and Harvey, V. 2011. Evaluating the potential for grey seal predation to explain elevated natural mortality in three fish species in the southern Gulf of St. Lawrence. Marine Ecology Progress Series, 442: 149–167. http://www.int-res.com/abstracts/meps/v442/p149-167/
##' @export
W.gompertz <- function(x){
    with(x,gamma.1*exp(-gamma.2*exp(-gamma.3*age)))
}
