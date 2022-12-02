######################################################################################################
# Get mackerel consumption from US groundfish
# source: https://fwdp.shinyapps.io/tm2020/ (downloaded by species from 2. total consumption)
######################################################################################################

####### DATA (VERSION 1: from app)  ########################################################################################
wd <- 'data_supp/groundfish/diet_composition_app'
wdimg <- 'img/data_supp/groundfish'

f <- list.files(wd,full.names = T,recursive = TRUE)
s <- tolower(gsub(".2022.*","",gsub(".*).","",f)))
USdiet <- lapply(f, read.csv)
USdiet <- lapply(1:length(USdiet),function(x){USdiet[[x]]$season <- s[x];USdiet[[x]]})
USdiet <- do.call('rbind',USdiet)
USdiet$X <-NULL

table(s) # 57 species

USdietf <- ddply(USdiet,c('season','Predator'),function(x){
    if(any(c('Scomber scombrus','Scombridae') %in% x$Prey)){x}else{
       rbind(x,data.frame(Predator=x$Predator[1],Prey="Scomber scombrus",Pct.m=0,Pct.m.ci=0,Pct.fo=0,Pct.fo.ci=0,Nstom=x$Nstom[1],Ntows=x$Ntows[1],Meansw=0,season=x$season[1])) 
    }}) 
USdietm <- USdietf[USdietf$Prey %in% c('Scomber scombrus','Scombridae'),]
USdietm <- USdietm[order(USdietm$season,USdietm$Pct.m),]

keypred <- ddply(USdietm,c('Predator'),summarise,m=sum(Pct.m))
keys <- keypred[keypred$m>0,'Predator']

USdietm <- USdietm[USdietm$Predator %in% keys,]

####### PLOT  ########################################################################################
# this doesn't match with the consumption estimates?? there are fish with no %w not in the C estimates, and with with no C but still %W
# info from Brian: minimum number of stomachs needed for this data: 200 stomachs here vs 25 for the consumption estimates.
p <- ggplot(USdietm,aes(x=Predator,y=Pct.m,ymin=Pct.m-Pct.m.ci,ymax=Pct.m+Pct.m.ci,col=Prey))+
    geom_errorbar()+
    geom_point()+
    geom_text(aes(label=Nstom,y=Inf),size=2,vjust=1)+
    facet_grid(season~.)+
    theme(axis.text.x = element_text(angle=90,hjust=0))
    
saveplot(p,'W_USgroundfish_shinyApp',wd=wdimg,c(20,25))

####### DATA (VERSION 2: from Brian Smith)  ########################################################################################
wd <- 'data_supp/groundfish/diet_composition_brian/'
wdimg <- 'img/data_supp/groundfish'

USdiet2 <- read.csv(paste0(wd,"diet_composition.csv"))
USdiet2m <- USdiet2[USdiet2$prey=="SCOSCO",]

####### PLOT  ########################################################################################
# overall plot
ggplot(USdiet2m,aes(x=year,y=relmsw2,ymin=relmsw2-relci2,ymax=relmsw2+relci2))+
    geom_errorbar()+
    geom_point()+
    geom_text(aes(label=nstom,y=Inf),size=2,vjust=1)+
    facet_grid(seacat~comname)+
    theme(axis.text.x = element_text(angle=90,hjust=0))

# plots per species
dummy <- lapply(unique(USdiet2m$comname),function(x){
    p <- ggplot(USdiet2m[USdiet2m$comname==x,],aes(x=year,y=relmsw2,ymin=relmsw2-relci2,ymax=relmsw2+relci2))+
        geom_errorbar()+
        geom_point()+
        geom_text(aes(label=nstom,y=Inf),size=3,hjust=2,angle=90)+
        facet_grid(seacat~comname)+
        theme(axis.text.x = element_text(angle=90,hjust=0))+
        labs(y='%W',x='Year')
    saveplot(p,paste0('W/',x),wd=wdimg,c(25,25))
})




