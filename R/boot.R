##' boot.predator
##' @param x object of class predator
##' @param nsim number of simulations
##' @param deterministic logical.
##' @param lower lower bound for simulated values
##' @details Bootstraps the input data across all levels found for the predator data (e.g., year, age, sex).
##' @export
boot.predator <- function(x,nsim=100,deterministic=FALSE,lower=NULL){
    require(mc2d)
    if(class(x)!="predator") stop('x should be of class predator')

    # identify objects already bootstrapped? (e.g., output bayesian model for N)
    b <- sapply(x,function(x)any("boot" %in% class(x)))
    b <- names(b[b])
    
    # determine levels (year, stage, sex, herd or whatever) and unique values of each: bootstrap over all
    ll <- unique(unlist(lapply(x,names)))
    dist.indicators <- c('mean','sd','min','mode','max','mu','logmean','logsd',grep('sigma',ll,value = TRUE))
    dist.indicatorsm <- c(dist.indicators, b,'sim','parameter')
    ll <- ll[!ll %in% dist.indicatorsm]
    levels <- lapply(ll,function(name) unique.in.list(x,name))
    names(levels) <- ll
    
    # bootstrap and merge all data/simulations into one handy data.frame
    boot.input <- function(x,levels,lower=NULL,name=NULL){
        print(name)
        # grid
        grid <- do.call(expand.grid,c(list(sim=1:nsim),levels))                      # numer of simulations if PER LEVEL (not total)
        
        if(!"boot" %in% class(x)){                                                   # if bootstrap still needed (only distribution parameters)
            grid <- merge(grid,x,all.x = TRUE)
            
            # distribution
            if(deterministic){                                                                  
                if("sd" %in% names(x)) rdis <- function(nsim,mean,sd) rnorm(nsim,mean,0)
                if("min" %in% names(x)) rdis <- function(nsim,min,max) rep(mean(c(min,max)),nsim)                            # simulations = average of the bounds
                if("mode" %in% names(x)) rdis <- function(nsim,min,mode,max) rep(mode,nsim)                                  # simulations = mode
                if("mu" %in% names(x)) rdis <- function(nsim,mu, sigma) {sigma[] <- 0;rmvnorm(nsim,mu,sigma)}
                if("logsd" %in% names(x)) rdis <- function(nsim,logmean,logsd) exp(rnorm(nsim,logmean,0))
            }else{
                if("sd" %in% names(x)) rdis <- rnorm
                if("min" %in% names(x)) rdis <- runif
                if("mode" %in% names(x)) rdis <- rpert
                if("mu" %in% names(x)) rdis <- rmvnorm
                if("logsd" %in% names(x)) rdis <- function(n,logmean,logsd) exp(rnorm(n,logmean,logsd))
            }
            
            # sample
            dist.id <- which(colnames(grid) %in% dist.indicators)
            sims <- ddply(grid,c('sim',ll), function(s){
                args <- c(list(n=1),as.list(s[,dist.id]))
                args <- args[!grepl('sigma',names(args))]
                args <- lapply(args,as.numeric)
                if(any(grepl('sigma',names(s)))) args$sigma <- as.matrix(s[,grepl('sigma',names(s))])
                if(length(args$sd)>1) args$n <- length(args$mean)                                          # for if multiple parameters and normal distribution
                do.call(rdis,args)
            })
            
            # rename columns
            name.id <- (length(ll)+2):ncol(sims)
            names(sims)[name.id] <- if(length(name.id)==1) name else unique(grid$parameter)
            
            # round if no params and not already a bootstrap
            if(!is.null(lower) & length(name.id)==1) sims[name.id] <- bound(sims[name.id],lower,Inf)       # only round if one param
        }else{                                                                       # if bootstrap already (e.g., bayesian output for N)
            navail <-  tapply(x$N,as.list(x[,ll]),length)                            # if already the correct number: no need to do more (though sim id should be off)
            navail <- as.numeric(names(table(unique(navail))))
            if(navail==nsim) return(x)
            sims <- lapply(1:nrow(grid),function(m){
                d <- filter(x, grid[m,-1])                                      # select all levels in data.frame
                if(nrow(d)==0) stop(paste0("this object is not bootstrapped for all levels:",name))
                d <- d[sample(1:nrow(d),1,TRUE),]
                d$sim <- grid[m,'sim']
                return(d)
            })
            sims <- do.call('rbind',sims)
        }
        return(sims)
    }
    
    ret <- lapply(names(x),function(i) boot.input(x[[i]],levels,lower,i))
    ret <- Reduce(merge,ret)
    return(ret)
}

