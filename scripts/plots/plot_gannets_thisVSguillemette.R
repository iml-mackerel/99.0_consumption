######################################################################################################
# Comparison our results with Guillemette 2018
# Fig. 2
# New numbers are higher because 
#    1) lower value for AE (error in guillemette)
#    2) % of non-breeders allowed to be higher
#    3) obviously more colonies
######################################################################################################

### Data from Guillemette 2018 (extracted from graph)
ref <- read.csv2("data_supp/gannets/C_guillemette2018.csv",sep=',')

### COMPARE 
# 1) comparison with current calc framework, using (deterministic) values from Guillemette
wd <- 'data'
avail <- avail.predator(wd)
avail <- avail[avail$population %in% c('anticosti','bird_rock','bonaventura'),]

input <- apply(avail,1,read.predator)                  # to remove warning about incomplete final year: add an enter at the end of the datafile
names(input) <- avail$wd

pred.ga <- lapply(1:length(input),function(x){boot.predator(x=input[[x]],nsim=1,lower=0,deterministic=TRUE)})    # 2 sims for uniform distribution
names(pred.ga) <- names(input)

consum.ga <- lapply(1:length(pred.ga),function(x){
    # consumption estimate
    pred.ga[[x]]$AE <-0.8                    # see mail. this is the true value used (but in paper it says 0.75)
    pred.ga[[x]]$P <- 0.54
    if(x %in% grep("non",names(pred.ga))) pred.ga[[x]]$Nfrac <- 0.11
    d <- consum.predator(pred.ga[[x]])
    
    pred.ga[[x]]$P <- 1
    d$Cup <- consum.predator(pred.ga[[x]])$C
    
    # metadata
    i <- strsplit(names(pred.ga)[[x]],'/')[[1]]
    d <- cbind(lifestage=i[4],d)
    d <- cbind(population=i[3],d)
    d <- cbind(species=i[2],d)
    return(d)
})
consum.ga <- do.call('rbind.fill',consum.ga)

compare <- ddply(consum.ga,c('year','sim'),summarise,Clow=sum(C),Cup=sum(Cup))                                # sum over pops and lifestages

p1 <- ggplot()+
    geom_ribbon(data=ref,aes(x=year,ymin=lower,ymax=upper),alpha=0.5,fill='darkgreen')+
    geom_ribbon(data=compare,aes(x=year,ymin=Clow,ymax=Cup),alpha=0.5)+  # guillemette values with this code
    labs(y='mackerel taken by gannets in the Gulf (t)')

saveplot(p1,'compare_guillemette_deterministic_oldvalues',wd='img/data_supp/gannets/',c(15,10))

# 2)  comparison with current values (and not deterministic)
compare2 <- consum[consum$species=='northern_gannets' & 
                  consum$population %in% c('bonaventura','bird_rock','anticosti'),]
compare2 <- ddply(compare2,c('year','sim'),summarise,C=sum(C))                                # sum over pops and lifestages
compare2 <- ddply(compare2,c('year'),summarise,min=quantile(C,0.025),max=quantile(C,0.975))

p2 <- ggplot()+
    geom_ribbon(data=ref,aes(x=year,ymin=lower,ymax=upper),alpha=0.5,fill='darkgreen')+
    geom_ribbon(data=compare2,aes(x=year,ymin=min,ymax=max),alpha=0.5)+
    labs(y='mackerel taken by gannets in the Gulf (t)')

saveplot(p2,'compare_guillemette_stochastic_newvalues',wd='img/data_supp/gannets/',c(15,10))



