##' seal.increm
##' @param g values of annual gompertz curve values
##' @param age vector
##' @param sex vector  
##' @details Helper function. Calculates annual growth rates (used to get monthly weights). Input data is assumed to be sorted by age.
seal.increm <- function(g,age,sex){
    inc <- 0
    c1 <- ifelse(sex=='f'&age>=6,0.7,ifelse(sex=='m'&age>=8,0.83,rep(1,length(age))))
    c2 <- ifelse(sex=='f'&age<=5,11,ifelse(sex=='m'&age<=7,11,rep(10,length(age))))
    for(i in 2:length(age)){inc <- c(inc,(g[i]/(g[i-1]*0.83*c1[i]))^(1/c2[i]))}
    return(inc)
}

##' seal.W.monthly
##' @param agemax integer
##' @param sex vector
##' @param gamma.1 vector
##' @param gamma.2 vector
##' @param gamma.3 vector
##' @param thisage vector
##' @param thismonth vector
##' @export
##' @details Provides data.frame with monthly W of seals given age, sex and Gompertz parameters. Subseting of results by age and month can be done using thisage and thismonth argument.
seal.W <- function(agemax,sex,gamma.1,gamma.2,gamma.3,thisage=age,thismonth=1:12){
    df <- data.frame(age=0:(agemax+1),sex=sex,gamma.1=gamma.1,gamma.2=gamma.2,gamma.3=gamma.3)
    df$X8 <- W.gompertz(df)                                                                                # month 8 = average gompertz value
    df <- ddply(df,c("sex"),transform,inc=seal.increm(X8,age,sex))                                         # increments (growth rate)
    df[,paste0("X",9:12)] <- sapply(1:4,function(x) c(df[-nrow(df),'X8']*df[-1,'inc']^x,0))                # month 9 to 12
    df[,'X1'] <- c(0,df[-c(nrow(df)),'X12']*df[-1,'inc'])                                                  # month 1
    df[,'X2'] <- df[,'X1']*ifelse(df$age>=8&df$sex=='m',0.83,ifelse(df$age>=6&df$sex=='f',0.7,df[,'inc'])) # month 2
    df[,paste0("X",3:5)] <- sapply(1:3,function(x) df[,'X2']*df[,'inc']^x)                                 # month 4 to 5
    df[,'X6'] <- df[,'X5']*ifelse(df$sex=='m',0.83,df[,'inc'])                                             # month 6
    df[,'X7'] <- df[,'X6']*ifelse(df$sex=='f',0.83,df[,'inc'])                                             # month 7
    df[df$age==0,c('sex',paste0("X",3:7))] <- ddply(df,c("sex"),function(y) sapply(5:1,function(x) y[y$age==0,'X8']/y[y$age==1,'inc']^x))      # age 0
    
    # transform
    W <- reshape2::melt(df[df$age!=(agemax+1),c('age','sex',paste0("X",1:12))],id=c('sex','age'),variable.name='month',value.name='W')
    W$month <- as.numeric(gsub("X","",W$month))
    W$age.dec <- with(W,age+month/12)
    W <- W[W$month %in% thismonth,]
    W <- W[W$age %in% thisage,]
    return(W)
}

