##' rmvnorm helper function to draw multivariate normal samples
##' @param n the number of samples.
##' @param mu the mean vector.
##' @param sigma a positive-definite symmetric matrix specifying the covariance matrix.
##' @param seed optional seed
##' @details Generates samples via the Cholesky decomposition, which is less platform dependent than eigenvalue decomposition.
##' @return If n = 1 a vector of the same length as mu, otherwise an n by length(mu) matrix with one sample in each row.
##' @export
rmvnorm <- function(n = 1, mu, sigma, seed){
    p <- length(mu)
    if(!all(dim(sigma) == c(p, p))){
        stop("incompatible arguments")
    }
    idx <- diag(sigma) > .Machine$double.xmin
    L <- matrix(0,p,p)
    if(any(idx)){
        L[idx,idx] <- chol(sigma[idx,idx])
    }
    if (!missing(seed)) set.seed(seed)
    X <- matrix(rnorm(p * n), n)
    X <- drop(mu) + t(X%*%L)
    if(n == 1){
        drop(X)
    }else{
        t(X)
    }
}
