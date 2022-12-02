######################################################################################################
# Cetacean abundance data
# source: draft resdoc Francois Gosselin
# see csv file for simple data imputations
######################################################################################################

####### DATA  ########################################################################################
wd <- 'data_supp/cetaceans/'
wdimg <- 'img/data_supp/cetaceans'

n <- read.csv(paste0(wd,"abundance.csv"))
n$lowcor <- with(n,exp(log(Ncor)-2*logsdcor))
n$highcor <- with(n,exp(log(Ncor)+2*logsdcor))
n$logNcor <- round(log(n$Ncor),3)

p <- ggplot(n[n$region=='total',],aes(x=factor(year),y=Ncor/1000))+
    geom_errorbar(aes(ymin=(Ncor-lowcor)/1000,ymax=(Ncor+highcor)/1000),fill='grey',width=0.1)+
    geom_point()+
    facet_wrap(~species,nrow=1)+
    labs(y='Abundance (thousands)',x='Year')+
    scale_y_continuous(expand=c(0,0),limits=c(0,800))

saveplot(p,'cetaceans_N',wd=wdimg,c(20,10))

n[n$region=='total',c('year','species','logNcor','logsdcor')]
