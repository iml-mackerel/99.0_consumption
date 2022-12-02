######################################################################################################
# extrapolate N of gannets for all 6 populations 
# N data from Jean-Francois Rail (some provided in mail, some extracted from figures)
######################################################################################################

####### DATA  ##############################
wd <- 'data_supp/gannets/'
wdimg <- 'img/data_supp/gannets/'

ab <- read.csv(paste0(wd,'N.csv'))

####### PLOT  ##############################
ggplot(ab,aes(x=year,y=abundance,col=population))+
    geom_point()+geom_line()

####### Extrapolate all populations at once ##############################
# go directly for gam smoother (easier to predict extra years)
png(file=paste0(wdimg,'N_smoothed_gam.png'),width=500,height=1000)
par(mfrow=c(3,2))
abs <- lapply(unique(ab$population),function(x){
    if(x=="Anticosti Island") k=4 else k=-1
    d <- with(ab[ab$population==x,],interpol(year,abundance,method='gam',k=k,from=1968,to=2021,title=x))
    d$se <- round(with(d,(upper-pred)/1.98),0)
    d$population <- x
    return(d)
})
dev.off()

abs <- do.call('rbind',abs)
abs[!is.na(abs$y),c('upper','lower','se')] <- c(NA,NA,NA) 
abs$interpol <- round(abs$interpol,0)
abs[abs$population=='Bird Rocks' & abs$x %in% 1968:1972,-1] <- abs[abs$population=='Bird Rocks' & abs$x==1974,-1] # to avoid negative abundancse. logscale backtransform not really helping

####### PLOT  ##############################
p <- ggplot(abs,aes(x=x,y=interpol))+
    geom_point(aes(y=interpol),col='darkgrey')+
    geom_point(aes(y=y),col='black')+
    geom_errorbar(aes(ymin=lower,ymax=upper),col='darkgrey')+
    labs(y='Abundance (numbers)',x='Year')+
    facet_wrap(population~.,scales="free_y")
saveplot(p,'N_extrapolated',wd=wdimg,c(25,15))

write.csv(abs,paste0(wd,'N_extrapolated.csv'))

