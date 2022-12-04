######################################################################################################
# Plot consumption estimates in various ways
######################################################################################################

wdimg <- 'img/consumption'

load(file='Rdata/consum.Rdata')
groups <- read.csv("data_supp/groups.csv")
consum <- merge(consum,groups,all.x=TRUE)

newtuna <- consum[consum$species=="tuna" & consum$year==2019,]
newtuna$year <- 2020
consum <- rbind(consum,newtuna)

# plots  #############################################################################################
## by categorie (US groundfish put together) ----------------------------------------------------------
consumqs <- ddply(consum,c('year','group','sim'),summarise,C=sum(C)) # sum over everything (lifestage,pop,sex)
consumqs <- ddply(consumqs, c('year','group'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   
consumqtot <- ddply(consum[consum$group!="Cetaceans (Canada)",],c('year','sim'),summarise,C=sum(C)) # sum over everything (lifestage,pop,sex)
consumqtot <- ddply(consumqtot, c('year'),summarise,low=quantile(C,0.025),med=median(C),high=quantile(C,0.975))   
consumqtot$group <- "Total (excluding cetaceans)"
consumqtot$total <-TRUE
consumqs$total <- FALSE
consumqs <- rbind(consumqs,consumqtot)

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
    geom_line(size=1)+
    labs(y='Mackerel consumption (t)',x='Year',col='')+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0),limits=c(1968,2020))+
    scale_color_manual(values=c(viridis::viridis(5),'black'))+
    geom_blank(aes(y = 0)) 

saveplot(ptotnoci,'consumption_groups_noci',wd=wdimg,c(15,10))

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


