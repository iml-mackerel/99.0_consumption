######################################################################################################
# Plot consumption estimates in various ways
######################################################################################################

wdimg <- 'img/consumption'

load(file='Rdata/consum.Rdata')
groups <- read.csv("data_supp/groups.csv")
consum <- merge(consum,groups,all.x=TRUE)

load(file='Rdata/consum.tot.Rdata')

newtuna <- consum[consum$species=="tuna" & consum$year==2019,]
newtuna$year <- 2020
consum <- rbind(consum,newtuna)

# plots  #############################################################################################
## by categorie (US groundfish put together) ----------------------------------------------------------
consumqs <- ddply(consum,c('year','group','sim'),summarise,C=sum(C)) # sum over everything (lifestage,pop,sex)
consumqs <- ddply(consumqs, c('year','group'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   
 
consum.tot$group <- "Total (excluding cetaceans)"
consum.tot$total <-TRUE
consumqs$total <- FALSE
consumqs <- rbind(consumqs,consum.tot)

ptot <- ggplot(consumqs,aes(x=year))+
    geom_ribbon(aes(ymin=low,ymax=high,fill=total),alpha=0.5)+
    #geom_linerange(aes(ymin=low,ymax=high,col=total))+
    geom_line(aes(y=med,col=total))+
    facet_wrap(~group,scale='free_y')+
    labs(y='Mackerel consumption (t)',x='Year',scale="free")+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0),limits=c(1968,2020))+
    scale_fill_manual(values=c('darkgrey',viridis::viridis(3)[1]))+
    scale_color_manual(values=c('black',viridis::viridis(3)[1]))+
    theme(legend.position = 'none')+
    geom_blank(aes(y = 0)) 

saveplot(ptot,'consumption_groups',wd=wdimg,c(25,15))

ptotnoci <- ggplot(consumqs,aes(x=year,y=med,col=group))+
    geom_line(size=0.5)+
    labs(y='Mackerel consumption (t)',x='Year',col='')+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0),limits=c(1968,2020))+
    scale_color_manual(values=c(viridis::viridis(5),'black'))+
    geom_blank(aes(y = 0)) +
    theme(legend.position = 'top')

saveplot(ptotnoci,'consumption_groups_noci',wd=wdimg,c(18,10))

## gannets -----------------------------------------------------------------------------------
consumg <- ddply(consum[consum$species=='northern_gannets',], c('year','species','population','lifestage'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   
consumg$population <- as.factor(consumg$population)
levels(consumg$population) <- list('Anticosti Is.'  = "anticosti", 
                                   'Baccalieu Is.' = "baccalieu_island",
                                   'Bird rock' = 'bird_rock',
                                   "Cape St. Mary's" = "cape_st_mary's",
                                   "Funk Is." = "funk_island",
                                   "Bonaventure Is." = "bonaventura")

pgannet <- ggplot(consumg,aes(x=year))+
    geom_ribbon(aes(ymin=low,ymax=high,fill=lifestage),alpha=0.5)+
    geom_line(aes(y=med,col=lifestage))+
    facet_wrap(.~population,scale="free_y")+
    labs(y='Mackerel consumption (t)',x='Year',col='',fill='')+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    scale_fill_viridis_d()+
    scale_color_viridis_d()+
    theme(legend.position = 'top')

saveplot(pgannet,'consumption_gannets_percolonielifestage',wd=wdimg,c(25,15))


## cetaceans -----------------------------------------------------------------------------------
consumc <- ddply(consum[consum$group == 'Cetaceans (Canada)',], c('year','species'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   
consumc$species <- revalue(consumc$species,c( "common_dolphin" = 'Common dolphin'  , 
                                 "harbour_porpoise" = 'Harbour porpoise',
                                 'pilot_whale' = 'Pilot whale',
                                 "white-sided_dolphin" =  "White-sided dolphin"))

pcet <- ggplot(consumc,aes(x=factor(year),y=med))+
    geom_point()+
    geom_errorbar(aes(ymin=low,ymax=high),size=0.5,width=0.1)+
    facet_wrap(.~species)+
    labs(y='Mackerel consumption (t)',x='Year')+
    scale_y_continuous(expand=c(0,0),limits=c(0,18000))


saveplot(pcet,'consumption_cetaceans_perspecies',wd=wdimg,c(10,10))

## US groundfish  -----------------------------------------------------------------------------------
consumqs <- ddply(consum[consum$group %in% c('Groundfish (US)'),],c('year','species','sim'),summarise,C=sum(C))
consumqs <- ddply(consumqs, c('year','species'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   

pgr <- ggplot(consumqs,aes(x=year))+
    geom_ribbon(aes(ymin=low,ymax=high),alpha=0.5)+
    geom_line(aes(y=med))+
    facet_wrap(species~.,scale="free_y")+
    labs(y='Mackerel consumption (t)',x='Year')+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand = c(0,0))

saveplot(pgr,'consumption_groundfish',wd=wdimg,c(25,15))


