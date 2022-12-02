######################################################################################################
# extrapolate breeding success of gannets for all 6 populations (Nfrac)
# breeding success data from Jean-Francois Rail (some provided in mail, some extracted from figures)
######################################################################################################

####### DATA  ##############################
wd <- 'data_supp/gannets/'
wdimg <- 'img/data_supp/gannets/'

bs <- read.csv(paste0(wd,'breeding_success.csv'))

####### PLOT  ##############################
ggplot(bs,aes(x=year,y=breeding_success,shape=population,col=population))+
    geom_point()+
    scale_color_manual(values=c('darkred','darkgreen','orange'))

####### St.Mary's ##############################
dsm <- bs[bs$population=="St. Mary's",]
dsm <- merge(expand.grid(year=1968:2021),dsm,all.x = TRUE)
names(dsm)[c(1:2)] <- c('x','y')

m <- mean(dsm[dsm$x %in% 1968:2010,]$y,na.rm = T) # interpolation gives the weirdest results
s <- sd(dsm[dsm$x %in% 1968:2010,]$y,na.rm = T)

dsm$interpol <- ifelse(is.na(dsm$y),m,dsm$y)
dsm$se <- ifelse(is.na(dsm$y),s,(dsm$y-dsm$lower)/2)
dsm[dsm$x==2021,'se'] <- mean((dsm[dsm$x>=2009,'y']-dsm[dsm$x>=2009,'lower'])/1.98,na.rm=T)
dsm$lower <- ifelse(is.na(dsm$y),with(dsm,interpol-1.98*se),dsm$lower)
dsm$upper <- ifelse(is.na(dsm$y),with(dsm,interpol+1.98*se),dsm$upper)
dsm[is.na(dsm$lower),'lower'] <- with(dsm[is.na(dsm$lower),],interpol-1.98*se)
dsm[is.na(dsm$upper),'upper'] <- with(dsm[is.na(dsm$upper),],interpol+1.98*se)
dsm$upper <- pmin(dsm$upper,100)
dsm$population <- "St. Mary's"

ggplot(dsm,aes(x=x,y=interpol))+
    geom_point(aes(y=interpol),col='darkgrey')+
    geom_point(aes(y=y),col='black')+
    geom_errorbar(aes(ymin=lower,ymax=upper),col='darkgrey')+
    labs(title = "St. Mary's",y='Breeding success (%)',x='Year')

####### Bonaventure ##############################
# try different smoothers
smoothers <- c('linear','spline','gam','loess')
x <- bs[bs$population=='Bonaventure',]$year
y <- bs[bs$population=='Bonaventure',]$breeding_success

png(file=paste0(wdimg,'breeding_success_bonaventure_smoothers.png'),width=600, height=500)
par(mfrow=c(2,2))
smooths <- lapply(smoothers, function(m){
    interpol(x,y,method=m) 
})
dev.off()
names(smooths) <- smoothers

# Use output from GAM and use first year to fill up last years
dbo <- smooths$gam
dbo$se <- with(dbo,(upper-pred)/1.98)
new <- dbo[rep(1,8),]
new$x <- 1968:1975
new$y <- NA
dbo <- rbind(new,dbo)
cv <- mean(dsm[!is.na(dsm$y),'se']/dsm[!is.na(dsm$y),'y']*100)  # average CV for st mary's
se <- dbo[!is.na(dbo$y),'y']*cv/100
dbo[!is.na(dbo$y),c('upper','lower')] <- dbo[!is.na(dbo$y),'y']+1.98*cbind(se,-se)
dbo$population <- "Bonaventure"

ggplot(dbo,aes(x=x,y=interpol))+
    geom_errorbar(aes(ymin=lower,ymax=upper),col='darkgrey')+
    geom_point(aes(y=interpol),col='darkgrey')+
    geom_point(aes(y=y),col='black')+
    labs(title = 'Bonaventure',y='Breeding success (%)',x='Year')

####### All others ##############################
dre <- expand.grid(year=1968:2021,population=c('Anticosti','Bird Rock','Baccalieu Island','Funk Island'))
dre <- merge(dre,bs[bs$population=='Bird Rock',],all.x = TRUE)
names(dre)[c(1,3)] <- c('x','y')


m <- mean(bs$breeding_success,na.rm = T)
s <- sd(bs$breeding_success,na.rm = T)

dre$interpol <- ifelse(is.na(dre$y),m,dre$y)
dre$se <- ifelse(is.na(dre$y),s,NA)
dre$lower <- ifelse(is.na(dre$y),with(dre,interpol-1.98*se),NA)
dre$upper <- ifelse(is.na(dre$y),with(dre,interpol+1.98*se),NA)

se <- dre[!is.na(dre$y),'y']*cv/100
dre[!is.na(dre$y),'se'] <- se
dre[!is.na(dre$y),c('upper','lower')] <- dre[!is.na(dre$y),'y']+1.98*cbind(se,-se)


ggplot(dre,aes(x=x,y=interpol))+
    geom_errorbar(aes(ymin=lower,ymax=upper),col='darkgrey')+
    geom_point(aes(y=interpol),col='darkgrey')+
    geom_point(aes(y=y),col='black')+
    labs(y='Breeding success (%)',x='Year')+
    facet_wrap(population~.)

####### All Populations  ##############################
breeding <- rbind(dbo[,-5],dsm,dre)
p <- ggplot(breeding,aes(x=x,y=interpol))+
    geom_errorbar(aes(ymin=lower,ymax=upper),col='darkgrey')+
    geom_point(aes(y=interpol),col='darkgrey')+
    geom_point(aes(y=y),col='black')+
    labs(y='Breeding success (%)',x='Year')+
    facet_wrap(population~.)+
    scale_y_continuous(limits=c(0,100),expand = c(0,0))
saveplot(p,'breeding_success_extrapolated',wd=wdimg,c(25,15))

breeding$y <- breeding$y/200    # chicks per bird instead of per nest and in proportion
breeding$se <- breeding$se/200
breeding$interpol <- breeding$interpol/200
breeding$lower <- breeding$lower/200
breeding$upper <- breeding$upper/200
write.csv(breeding,paste0(wd,'breeding_success_extrapolated.csv'))

