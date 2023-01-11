######################################################################################################
# Get mackerel consumption from US groundfish
# source: Brian Smith
# uncertainty rules: https://en.wikipedia.org/wiki/Variance#Product_of_independent_variables
# DEPRICATED: got the spring ones with ci value (use this only for graphs of abundance/ %W)
######################################################################################################

sdSum <- function(sd1,sd2){
    sqrt(sd1^2+sd2^2)
}
sdProd<-function(mu1,sd1,mu2,sd2,cov=0){
    var1 <- sd1^2
    var2 <- sd2^2
    var <- mu1^2*var2 + mu2^2*var1 + cov^2 + 2*mu1*mu2*cov + var1*var2
    sqrt(var)
}

####### total consumption: exploratory  ########################################################################################
wd <- 'data_supp/groundfish/all_brian'
wdimg <- 'img/data_supp/groundfish'

consum <- read.csv(paste0(wd,'/Beveren.dat.fh.3.csv'))

# check estimation of evacuation rate (idem to available)
consum$Emy <- with(consum,a*exp(b*bottemp)) # correct evacuation rate
consum$Emysd <- with(consum, sqrt(a^2*exp(b^2*bottemp_sd))) # no uncertainty in a and b

# estimate daily ingestion (note that valyues 3x less than 2%BM because sst is very low)
consum$DI = with(consum,24*totwt*E) # g/day of all prey eaten, BUT THIS IS WAY LOWER THAN 2% BODYWEIGHT FOR SPINY DOGFISH?
consum$DIsd = with(consum,sqrt((24*totwt)^2*Emysd^2))
consum$totwtsd <- 0
consum$DIsd2 = with(consum,sqrt(24^2*sdProd(totwt,totwtsd,E,Emysd)^2)) # idem

# daily ingestion of mackerel
consum$DImack = with(consum,DI*relmsw/100)  # g mackerel/day
consum$DImacksd = with(consum,sdProd(DI,DIsd,relmsw/100,relci/100/1.96)) 

# if I'd want that in number
consum$DImacknum = with(consum,DImack/meansw2)  # number of mackerel/day (per capite consumption?)

# half year per capita consumption
consum$pccmy = with(consum,DImack*(365/2))  # weight (g) of mackerel in 6 months
consum$pccmysd = with(consum,sqrt(DImacksd^2*(365/2)^2))  # weight (g) of mackerel in 6 months

# pop consumption (kt)
consum$consum <- consum$pcc*consum$abunpop/1000000000
consum$consumsd <- with(consum,sqrt(sdProd(pccmy/1000000000,pccmysd/1000000000,abunpop,abunpop.sd)^2))

# sum over seasons to compare with other data provided
co <- ddply(consum,c('comname','year'),summarise,consum=sum(consum,na.rm = T), consumsd=sdSum(consumsd[seacat=='FALL'],consumsd[seacat=='SPRING']))
dcast(co,year~comname,value.var = 'consum')

# I didn't get all the uncertainties, so can't estimate it. Brian also uses this gamma distribution approach, so wouldn't match

####### total consumption: save input  ########################################################################################

# ## write input for bootstrap
# sp <- 'SPINY DOGFISH'
# 
# # abundance
# x <- consum[consum$seacat=='SPRING' & consum$comname==sp,c('year','abunpop','abunpop.sd')]
# names(x) <- c('year','mean','sd')
# f <- paste0('data/',gsub(" ","_",tolower(sp)),'/US/all/N.txt')
# write.input(x,f,'ABUNDANCE',tolower(sp),'all','US','US','Brian',NA,'numbers',NA)
# 
# # temp
# x <- consum[consum$seacat=='SPRING' & consum$comname==sp,c('year','bottemp','bottemp_sd')]
# names(x) <- c('year','mean','sd')
# f <- paste0('data/',gsub(" ","_",tolower(sp)),'/US/all/TEMP.txt')
# write.input(x,f,'BOTTOM TEMPERATURE',tolower(sp),'all','US','US','Brian',NA,'degrees',NA)
# 
# # %W
# x <- consum[consum$seacat=='SPRING' & consum$comname==sp,c('year','relmsw','relci')]
# names(x) <- c('year','mean','sd')
# x$sd <- x$sd/1.96 # ci to sd
# f <- paste0('data/',gsub(" ","_",tolower(sp)),'/US/all/P.txt')
# write.input(x,f,'PERCENTAGE PREY CONSUMED',tolower(sp),'all','US','US','Brian',NA,'% weight',NA)
# 
# # stomach weight
# x <- consum[consum$seacat=='SPRING' & consum$comname==sp,c('year','totwt','totwtsd')] # no sd!!
# names(x) <- c('year','mean','sd')
# f <- paste0('data/',gsub(" ","_",tolower(sp)),'/US/all/SW.txt')
# write.input(x,f,'AVERAGE STOMACH WEIGHT',tolower(sp),'all','US','US','Brian',NA,'g',NA)
# 
# # a and b (E)
# 
# # DAYS
# df <- data.frame(min=365)
# f <- paste0('data/',gsub(" ","_",tolower(sp)),'/US/all/DAY.txt')
# write.input(x,f,'DAYS OF PREY CONSUMPTION',tolower(sp),'all','US','US','Brian',NA,'days',NA)
# 
# 
# read.input(f)


### plots
# abundance (based on lognormal distribution??)
ab <- na.omit(consum[,c('year','abunpop','abunpop.sd','comname')])
ab$lci <- apply(ab[,-4],1,function(x){x<<-x;exp(quantile(rlnorm(10000,log(x['abunpop']),log(x['abunpop.sd']+0.0001)),0.975))})

ggplot(consum,aes(x=year,y=abunpop))+
    geom_ribbon(aes(ymin=pmax(abunpop-2*abunpop.sd,0),ymax=abunpop+2*abunpop.sd),alpha=0.5)+
    geom_line()+
    facet_grid(seacat~comname)+
    scale_y_continuous(limits=c(0,max(consum$abunpop+2.1*consum$abunpop.sd)),expand=c(0,0))


p <- ggplot(consum[consum$seacat=='SPRING',],aes(x=year,y=abunpop/1000000,col=tolower(comname),linetype=tolower(comname)))+
    #geom_ribbon(aes(ymin=pmax(abunpop-2*abunpop.sd,0),ymax=abunpop+2*abunpop.sd,fill=seacat),alpha=0.5)+
    geom_line()+
    geom_point()+
    scale_color_viridis_d()+
    labs(y='Abundance (million)',x='Year',col='Species',linetype='Species')+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand = c(0,0),limits=c(0,max(consum[consum$seacat=='SPRING','abunpop']/1000000,na.rm = T)*1.01))
saveplot(p,'N_USgroundfish_spring_allspecies',wd=wdimg,c(25,10))    
   
p <- ggplot(consum,aes(x=year,y=abunpop/1000000))+
    #geom_ribbon(aes(ymin=pmax(abunpop-2*abunpop.sd,0),ymax=abunpop+2*abunpop.sd,fill=seacat),alpha=0.5)+
    geom_line(aes(col=tolower(seacat)))+
    facet_wrap(tolower(comname)~.,scale='free_y',ncol=1)+
    labs(y='Abundance (million)',x='Year',col='Season')+
    scale_color_manual(values=c('orange','mediumorchid'))
saveplot(p,'N_USgroundfish_spring_allspecies_springvsfall',wd=wdimg,c(10,25)) 

# %W
ggplot(consum,aes(x=year,y=relmsw))+
    #geom_ribbon(aes(ymin=pmax(abunpop-2*abunpop.sd,0),ymax=abunpop+2*abunpop.sd,fill=seacat),alpha=0.5)+
    geom_point(aes(col=seacat))+
    geom_line(aes(col=seacat))+
    facet_grid(comname~seacat,scale='free_y')+
    scale_y_continuous(limits=c(0,max(consum$abunpop+2.1*consum$abunpop.sd)),expand=c(0,0))

p <- ggplot(consum[consum$seacat=='SPRING',],aes(x=year,y=relmsw))+
    geom_line(size=0.5)+
    geom_point()+
    geom_errorbar(aes(ymin=pmax(relmsw-relci,0),ymax=pmin(relmsw+relci,100)),size=0.5)+
    geom_text(aes(label=nstom,y=Inf),hjust=1.1,size=2,angle=90)+
    facet_wrap(tolower(comname)~.,scale='free_y')+
    labs(y='%W',x='Year')+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand = c(0,0),limits=c(0,100))
saveplot(p,'W_USgroundfish_spring_allspecies',wd=wdimg,c(30,14))

# consumption
consum$consum <- consum$consum*0.5
p <- ggplot(consum[consum$seacat=='SPRING',],aes(x=year,y=consum,col=tolower(comname),linetype=tolower(comname)))+ # 0.5 for northern contingent: average 0.5-0.85
    geom_line()+
    geom_point()+
    scale_color_viridis_d()+
    labs(y='Mackerel consumption (1000t)',x='Year',col='Species',linetype='Species')+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand = c(0,0),limits=c(0,max(consum[consum$seacat=='SPRING','consum'],na.rm = T)*1.01))
saveplot(p,'consum_USgroundfish_spring_allspecies',wd=wdimg,c(25,10))    

perc <- dcast(consum[consum$seacat=='SPRING',],year~comname,value.var='consum')
round(round(perc[,-1]*1000))
perc$tot <- apply(perc[,-1],1,sum,na.rm=T)
round(sweep(perc[,-1],1,perc$tot,'/')*100,0)

ecdf(round(perc[,7]*1000))(7000)