######################################################################################################
# N and W from Tuna MSE (https://iccat.github.io/abft-mse/)
######################################################################################################

library(ABTMSE)
loadABT()

### EXTRACT OUTPUT OMs -------------------------------------------------------------------------------

oms <- ls()[grep("OM_",ls())][1:240]  # vector with all om names
# 48 sims
# 2 pop
# 35 ages
# 55 years
# 4 seasons
# 7 subareas

output <- ldply(oms,function(o){
    x <- get(o)
    if(substr(o,nchar(o),nchar(o)) %in% letters) omi <-gsub("OM","OMI",substr(o,1,nchar(o)-1)) else omi <- gsub("OM","OMI",o)
    xi <- get(omi)
    N <- x@checks$N[,,3,,which(x@areanams=='GSL')] # [1:2, 1:55, 1:4, 1:35, 1:7] 
    dimnames(N) <- list(stock=x@Snames,year=xi@years[1]:xi@years[2],age=1:35)
    W <- xi@wt_age
    dimnames(W) <- list(stock=x@Snames,age=1:35,year=xi@years[1]:xi@years[2])
    M <- x@M[1,,,1]
    dimnames(M) <- list(stock=x@Snames,age=1:35)
    
    df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(melt(N,value.name = 'N'),melt(W,value.name = 'W'),melt(M,value.name = 'M')))
    df$om <- o
    return(df)
})

## sum abundance over all age classes and both stocks (!! more and more of the east), per om ----
tot <- ddply(output,c('year','om'),summarise,
              Wall=sum(W*N)/sum(N),
              W7=sum(W[age>6]*N[age>6])/sum(N[age>6]),
              Nall=sum(N),
              N7=sum(N[age>6]),
              N1perc=sum(N[age==1])/sum(N)*100,
              N16perc=sum(N[age<7])/sum(N)*100) 

tot <- tot[!duplicated(tot[,-2]),]

toMatch <- c("OM_13", "OM_19", "OM_37","OM_43")
tot$true <- grepl(paste(toMatch,collapse="|"), tot$om)

p1 <- ggplot(tot,aes(x=year,y=Nall,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))
p2 <- ggplot(tot,aes(x=year,y=Wall,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))              
p3 <- ggplot(tot,aes(x=year,y=W7,col=om,size=true))+geom_line() +theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))               
p4 <- ggplot(tot,aes(x=year,y=N7,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))
p5 <- ggplot(tot,aes(x=year,y=N1perc,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))+labs(y='% age 1 fish in the population')
p6 <- ggplot(tot,aes(x=year,y=N16perc,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))+labs(y='% age 1-6 fish in the population')

saveplot(p1,'N_OMall',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(p2,'W_OMall',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(p3,'W7+_OMall',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(p4,'N7+_OMall',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(p5,'N1perc_OMall',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(p6,'N16perc_OMall',wd='img/data_supp/tuna/oms/',c(25,15))

## selectivity
sel <- ldply(unique(tot$om),function(o){
    x <- get(o)
    data.frame(x=1:15,y=x@sel[1,which(x@Fleets$name=="RRCAN"),],om=o)
})
sel$true <- grepl(paste(toMatch,collapse="|"), sel$om)

ps <- ggplot(sel,aes(x=x,y=y,col=om))+geom_line(aes(size=true))+scale_size_manual(values=c(0.5,3))
saveplot(ps,'Sel_OMall',wd='img/data_supp/tuna/oms/',c(25,15))


## N bubble plots
outputt <- output[output$om %in% unique(tot$om),]
dummy <- ddply(outputt,c('om'),function(x){
   p <-  ggplot(x,aes(x=year,y=age))+geom_point(aes(size=N))+
       facet_wrap(~stock)
   o <- unique(x$om)
   saveplot(p,o,wd='img/data_supp/tuna/oms/N/',c(25,15))
})

### APPROACH: SUBSET OMS -------------------------------------------------------------------------------
onemean <- ddply(tot,c('om'),summarise,perc=mean(N16perc))
theseom <- onemean[onemean$perc<=100/3,'om']

totthis <- tot[tot$om %in% theseom,]

pt1 <- ggplot(totthis,aes(x=year,y=Nall,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))
pt2 <- ggplot(totthis,aes(x=year,y=Wall,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))              
pt3 <- ggplot(totthis,aes(x=year,y=W7,col=om,size=true))+geom_line() +theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))               
pt4 <- ggplot(totthis,aes(x=year,y=N7,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))
pt5 <- ggplot(totthis,aes(x=year,y=N1perc,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))+labs(y='% age 1 fish in the population')
pt6 <- ggplot(totthis,aes(x=year,y=N16perc,col=om,size=true))+geom_line()+theme(legend.position = 'none')+scale_size_manual(values=c(0.5,3))+labs(y='% age 1-6 fish in the population')

saveplot(pt1,'N_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(pt2,'W_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(pt3,'W7+_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(pt4,'N7+_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(pt5,'N1perc_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))
saveplot(pt6,'N16perc_OMthese',wd='img/data_supp/tuna/oms/',c(25,15))

tuna.ab <- ddply(totthis,c('year'),summarise,logmean=mean(log(Nall)),logsd=sd(log(Nall)))
tuna.w <- ddply(totthis,c('year'),summarise,mean=mean(Wall),sd=sd(Wall))

pn <- ggplot(tuna.ab,aes(x=year,y=exp(logmean)))+
    geom_ribbon(aes(ymin=exp(logmean-2*logsd),ymax=exp(logmean+2*logsd)),alpha=0.2)+
    labs(y='Abundance (numbers)',x='Year')+
    geom_line(data=totthis,aes(y=Nall,group=om),col='darkgrey')+
    geom_line(size=2)+
    scale_x_continuous(expand=c(0,0))
pw <-ggplot(tuna.w,aes(x=year,y=mean))+
    geom_ribbon(aes(ymin=mean-2*sd,ymax=mean+2*sd),alpha=0.2)+
    labs(y='Weight (kg)',x='Year')+
    geom_line(data=totthis,aes(y=Wall,group=om),col='darkgrey')+
    geom_line(size=2)+
    scale_x_continuous(expand=c(0,0))
    
saveplot(pn,'N_OMthese_distribution',wd='img/data_supp/tuna/oms/',c(15,10))
saveplot(pw,'W_OMthese_distribution',wd='img/data_supp/tuna/oms/',c(15,10))

sel2 <- sel[sel$om %in% theseom,]
ps2 <- ps + geom_line(data=sel2,aes(group=om),col='black')

saveplot(ps2,'Sel_OMallvsthese',wd='img/data_supp/tuna/oms/',c(25,15))

### SAVE FINAL INPUT -------------------------------------------------------------------------------
write.input(tuna.ab,file='data/tuna/atlantic/all/N.txt',"ABUNDANCE",'tuna','all','GSL','Canada','ICCAT MSE')
write.input(tuna.w,file='data/tuna/atlantic/all/W.txt',"WEIGHT",'tuna','all','GSL','Canada','ICCAT MSE')

### %BM (See Overholtz 2006) -------------------------------------------------------------------------------
ra <- range(c(tuna.w$mean+2*tuna.w$sd),c(tuna.w$mean-2*tuna.w$sd))
bm <- 0.123*ra^0.8/ra*100

### plot with catch landings on top  ------------------------------------------------------------------------
load("data_supp/tuna/W/Wgsl.Rdata")
load("data_supp/tuna/W/Wss.Rdata")
Wgsl$region <- 'GSL'
Wss$region <- 'Shelf'
Wland <- data.frame(rbind(Wgsl,Wss))

Wland <- ddply(Wland,c('year','region'),summarise,med=median(weight),lower=quantile(weight,0.025),upper=quantile(weight,0.975))
pw2 <-ggplot(tuna.w,aes(x=year))+
    geom_point(data=Wland[Wland$year<2020,],aes(y=med,group=region,col=region),position = position_dodge(width = 0.50))+
    geom_linerange(data=Wland[Wland$year<2020,],aes(ymin=lower,ymax=upper,group=region,col=region), position = position_dodge(width = 0.5))+
    geom_ribbon(aes(ymin=mean-2*sd,ymax=mean+2*sd),alpha=0.2)+
    labs(y='Weight (kg)',x='Year',col='Region')+
    geom_line(data=totthis,aes(y=Wall,group=om),col='darkgrey')+
    geom_line(size=2,aes(y=mean))+
    scale_x_continuous(expand=c(0,0))+
    scale_color_manual(values=c('grey70','grey50'))+
    theme(legend.position = c(0.1,0.85))
saveplot(pw2,'W_OMthese_distribution_withlandings',wd='img/data_supp/tuna/oms/',c(15,10))

