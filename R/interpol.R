##' interpol
##' @param x vector of x values
##' @param y vector of y values
##' @param method Character vector: linear, spline, loess or gam
##' @param plot logical. plot observed vs predicted?
##' @param k k parameter when method = gam
##' @param from start x value (minimum by default)
##' @param to end x value (maximum by default)
##' @param title plot title (plot method by default)
##' @param ... argument to functions gam (mgcv), na.appox (zoo) or na.spline (zoo)
##' @details if time-series with gaps, return data frame with predictions as well as interpolated series
##' @import mgcv zoo
##' @export
interpol <- function(x,y,method=c('linear','spline','gam','loess'),plot=TRUE,k=-1,from=min(x,na.rm = TRUE),to=max(x,na.rm = TRUE),title=method,...){
    method <- match.arg(method)
    df <- data.frame(x=from:to)
    df <- merge(df,data.frame(x=x,y=y),all.x=TRUE)
    df$upper <- NA
    df$lower <- NA
    switch(method,
           "gam" = { 
               works <- try(mgcv::gam(y ~ s(x,k=k), data=df,...),silent=TRUE) # very simple filling with gam
               if ('try-error' %in% class(works)) m <- mgcv::gam(y ~ x,data=df) else m <- mgcv::gam(y ~ s(x,k=k),data=df,...)
               p <- predict(m,newdata=df,se.fit=TRUE) 
               df$pred <- p$fit
               df$lower <- p$fit-1.98*p$se.fit
               df$upper <- p$fit+1.98*p$se.fit
           },
           "linear" = {
               df$pred <-  zoo::na.approx(df$y,na.rm=FALSE,rule=2,...)
           },
           "spline" = {
               fit <- smooth.spline(na.omit(df[,1:2])$x, na.omit(df[,1:2])$y)
               df$pred <- predict(fit,x=df$x,se=TRUE)$y # the se is notavailable
               res <- (fit$yin - fit$y)/(1-fit$lev)      # jackknife residuals
               sigma <- sqrt(var(res))                     # estimate sd
               df[!is.na(df$y),]$upper <- fit$y + 2.0*sigma*sqrt(fit$lev)   # upper 95% conf. band
               df[!is.na(df$y),]$lower <- fit$y - 2.0*sigma*sqrt(fit$lev)   # lower 95% conf. band
           },
           "loess" = { 
               fit <- loess(y~x,data=df)
               p <- predict(fit,newdata=df$x,se=TRUE) 
               df$pred <- p$fit
               df$lower <- p$fit-1.98*p$se.fit
               df$upper <- p$fit+1.98*p$se.fit
           }
    )
    df$interpol <- ifelse(is.na(df$y),df$pred,df$y)
    if(plot) {
        plot(df$y~df$x,xlim=range(df$x),pch=16,
             ylim=c(min(c(df$pred,df$lower),na.rm = T),max(c(df$pred,df$upper),na.rm=T)),
             main=title,
             ylab='y',
             xlab='x')
        points(df$pred~df$x,col='red',pch=20)
        points(df$interpol~df$x,col='green',pch=20)
        points(df$x,df$lower,col='blue',pch='-')
        points(df$x,df$upper,col='blue',pch='-')
    }
    return(df)
}
