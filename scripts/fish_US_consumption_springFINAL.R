######################################################################################################
# Get mackerel consumption from US groundfish
# source: Brian Smith
# uncertainty rules: https://en.wikipedia.org/wiki/Variance#Product_of_independent_variables
# FINAL
######################################################################################################


####### total spring mackerel consumption  ########################################################################################
wd <- 'data_supp/groundfish/all_brian'
wdimg <- 'img/data_supp/groundfish'

spring <- read.csv(paste0(wd,'/spring.only.TmCa3N.Atlantic mackerel.csv'))
spring$X <- NULL
spring$interpol <- FALSE

# fill in 2020 (0 are NA) and early years
refperiod <- head(spring,5) # last 5 years
spring <- rbind(data.frame(year=1968:1972,mean=mean(refperiod$mean),lci=0,uci=max(refperiod$uci),interpol=TRUE),spring)

spring[spring$year==2020,2:5]  <- NA
int <- interpol(spring$year,spring$mean)
spring[spring$year==2020,2:5] <- c(int[int$x==2020,'interpol'],0,spring[spring$year==2021,'uci'],interpol=TRUE)

ggplot(spring,aes(x=year,y=mean,col=as.factor(interpol)))+
    geom_point()+
    geom_errorbar(aes(ymin=lci,ymax=uci))+
    scale_color_manual(values=c('black','darkgrey'))+
    theme(legend.position = 'none')+
    labs(y='Consumption (1000t)',x='Year')

# Brian uses a Gamma distribution, but there is no way I can drawn from that with the given info. use lognormal for approximation (very similar results)
nsim <- 1000

consum.usgroundfish <- ddply(spring,c('year'),function(x){
    mu <- log(x$mean+0.0000001)
    sd <- (log(x$uci+0.0000001)-log(x$mean+0.0000001))/2
    data.frame(species='Groundfish (US)',sim=1:nsim,population='US',lifestage='all',C=exp(rnorm(nsim,mu,sd)),NC=runif(nsim,0.15,0.85))
})

# quick check
test <- ddply(consum.usgroundfish,c('year'),summarise,mean.new=mean(C),lci.new=quantile(C,0.025,na.rm = T),uci.new=quantile(C,0.975,na.rm = T))
test <- merge(test,spring)

ggplot(test,aes(x=year))+
    geom_point(aes(y=mean))+
    geom_point(aes(y=mean.new),col='red')+
    geom_errorbar(aes(ymin=lci,ymax=uci))+
    geom_errorbar(aes(ymin=lci.new,ymax=uci.new),col='red')+
    labs(y='Consumption (1000t)',x='Year')

# save
consum.usgroundfish$C <- with(consum.usgroundfish,C*NC*1000)
consum.usgroundfish$NC <- NULL

save(consum.usgroundfish,file="Rdata/consum.usgroundfish.1000sim.Rdata")
