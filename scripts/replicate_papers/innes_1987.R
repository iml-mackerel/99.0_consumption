######################################################################################################
# alpha and beta WITH covariance from Innes 1987
# data extracted from Fig. 3
# alpha and beta in paper are 0.313 and 0.66 (R2=0.86)
######################################################################################################

####### DATA  ##############################
wd <- 'data_supp/cetaceans/'
wdimg <- 'img/data_supp/cetaceans/'

inn <- read.csv(paste0(wd,'innes_1987/Fig3.csv'))

####### FIT  ##############################
inn$x <- log10(inn$W)
inn$y <- log10(inn$DI)
m <- lm(y~x,inn[1:54,])
co <- coef(m)
ab <- c(round(10^co[1],3),round(co[2],2))  # should be 0.313, 0.66

####### PLOT  ##############################
ggplot(inn,aes(x=x,y=y))+
    geom_point()+
    geom_abline(intercept=co[1],slope=co[2])

###### CHECK WITH ONE DATAPOINT LESS #####
test <- t(sapply(1:nrow(inn), function(x){
    d <- inn[-x,]
    m <- lm(y~x,d)
    co <- coef(m)
    c(alpha=round(10^co[1],3),beta=round(co[2],2)) 
}))
test - matrix(c(0.313,0.66),nrow=nrow(inn),ncol=2,byrow=T) # if 2nd point removed, identical to Innes

m <- lm(y~x,inn[-2,])
co <- coef(m)
cov <- vcov(m)                                   # use this covariance matrix (back transformed)

ggplot(inn[-2,],aes(x=x,y=y))+
    geom_point()+
    geom_abline(intercept=co[1],slope=co[2]) # this one is probably better

# back transformation (https://stats.stackexchange.com/questions/533804/covariance-of-log-transformed-variable)
ab <- c(round(10^co[1],3),round(co[2],2))                  # should be 0.313, 0.66
q <- log(0.5+sqrt(0.25+cov[1,1]*exp(-2*ab[1])))
r <- cov[2,1]/exp(ab[1]+q/2)
s <- cov[2,2]
covt <- matrix(c(q,r,r,s),nrow=2,ncol=2,byrow=TRUE)

test <- rmvnorm(40000,ab,covt)
test <- cbind(test,rep(1:4000,10))
test <- cbind(test,sapply(1:nrow(test),function(x) test[x,1]*test[x,3]^test[x,2]))
df <- data.frame(test)
names(df) <- c('alpha','beta','W','DI')

ggplot(inn,aes(x=W,y=DI))+
    geom_point(data=df,col='grey')+
    geom_point()

### check effect rounding
test <- rmvnorm(40000,ab,round(covt,4))
test <- cbind(test,rep(1:4000,10))
test <- cbind(test,sapply(1:nrow(test),function(x) test[x,1]*test[x,3]^test[x,2]))
df <- data.frame(test)
names(df) <- c('alpha','beta','W','DI')


ggplot(inn,aes(x=W,y=DI))+
    geom_point(data=df,col='grey')+
    geom_point()

