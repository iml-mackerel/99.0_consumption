##' unique.in.list
##' @param x list of dataframes
##' @param name column name
##' @details Finds the unique values of each variable/column across all data.frame in a list
unique.in.list <- function(x,name){
    yy <- lapply(x,function(x) {if(name %in% names(x)) na.omit(unique(x[,name]))})
    yym <- which(sapply(yy,length)==max(sapply(yy,length)))[1]  # longest string 
    unique(x[[yym]][,name])  # unique stuff in that string
}

##' bound
##' @param x vector
##' @param lower lower bound
##' @param upper upper bound
##' @details Takes a vector and substitutes all values outside the specified bounds by their respective bound.
bound <- function(x,lower,upper){
    x[x<lower] <-lower
    x[x>upper] <-upper
    return(x)
}

#' Filter a data frame dynamically
#' @param df data frame to filter
#' @param args named vector 
#' @details Function to dynamically subset data. names of vector args are names of argument, value itself it the value that should be selected (==).
#' @export
filter <- function(df, args){
    args  <- lapply(args,function(x) list(`==`,x))
    evaluate <- function(predicate, value) {
        if (is.list(predicate)) {
            operator <- predicate[[1L]]
            rhs <- predicate[[2L]]
        } else {
            operator <- `==`
            rhs <- predicate
        }
        return(operator(value, rhs))
    }
    index <- apply(
        mapply(evaluate, predicate=args, value=df[names(args)]), 1L, all
    )
    return(df[index,])
}