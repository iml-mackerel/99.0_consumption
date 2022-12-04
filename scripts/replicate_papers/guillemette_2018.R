####################################################################
##### repeat the analyses of guillemette 2018 (with new data) ######
####################################################################

### preparation ####################################################
bon <- read.input("data/northern_gannets/bonaventura/breeders/N.txt")
bon$pop <- attr(bon,'meta')$population

ant <- read.input("data/northern_gannets/anticosti/breeders/N.txt")
ant$pop <- attr(ant,'meta')$population

bir <- read.input("data/northern_gannets/bird_rock/breeders/N.txt")
bir$pop <- attr(bir,'meta')$population

gan_ab <- rbind(bon,ant,bir)

ggplot(gan_ab,aes(x=year,y=mean,col=pop))+geom_line()

### breeders ###############################################################
ingestion_rate = 4865 #kj/day (DEE)
digestibility = 0.8 # % also known as assimilation efficiency (in paper it is written 0.75 but in reality 0.8 was used: see mail)
prey_calo_content = 7.5 # kj/g, NRJ
prey_perc = c(0.54,1) # % mackerel in diet
time = 100 # days

prey_gram = ingestion_rate/digestibility/prey_calo_content # per day grams eaten
mack = prey_gram*prey_perc #per day, grams of mackerel eaten
mack # guillemette: 438-811g of mackerel !!! (I don't understand how guillemette got to these numbers, but they're onyl slighy different)

gan_ab$low_breeders <- gan_ab$mean*mack[1]*time/1000000 # total tonnes of mackerel eaten
gan_ab$high_breeders <- gan_ab$mean*mack[2]*time/1000000 # total tonnes of mackerel eaten

### non-breeders ###############################################################
fraction_breeders = 0.113 #% but in mail from francois this should be much higher: 28%
gan_ab$low_nonbreeders = gan_ab$mean/2 * fraction_breeders*mack[1]*time/1000000 # total tonnes of mackerel eaten
gan_ab$high_nonbreeders = gan_ab$mean/2 * fraction_breeders*mack[2]*time/1000000 # total tonnes of mackerel eaten

### chicks  ###############################################################
ingestion_total = 24.2 # 24kg over 13 weeks (montevecchi). contrasts with cooper (cape gannet), who estimated 37 498g over 88 days or 243 587kj (12.5 weeks)
prod = c(0.027,0.77) # annual values are used, see fig 3A!!!!! no way to get these however!!!!
# annual breeding success from different colonies given by fancois rail.

gan_ab$low_chicks = ingestion_total*prey_perc[1]*gan_ab$mean*prod[2]/1000 # mean productivity???
gan_ab$high_chicks = ingestion_total*prey_perc[1]*gan_ab$mean*prod[2]/1000 # mean productivity???

### total  ###############################################################

gan_ab$total_low = with(gan_ab,low_breeders+low_nonbreeders+low_chicks)
gan_ab$total_high = with(gan_ab,high_breeders+high_nonbreeders+high_chicks)

gan_ab <- gan_ab[gan_ab$year>1972.5,] # guillemete excluded this
ggplot(gan_ab,aes(x=year))+
    geom_ribbon(aes(ymin=total_low,ymax=total_high),fill='black',alpha=0.5)+
    geom_ribbon(aes(ymin=low_breeders,ymax=high_breeders),fill='red',alpha=0.5)+
    geom_ribbon(aes(ymin=low_nonbreeders,ymax=high_nonbreeders),fill='orange',alpha=0.5)+
    geom_ribbon(aes(ymin=low_chicks,ymax=high_chicks),fill='blue',col='blue',alpha=0.5)+
    facet_wrap(.~pop,scale='free_y')

# there are many years that are missing, varying dependent on species. obviously need to fill in gaps or otherwise it's NA. no idea how it's done in the paper.
v <- gan_ab[gan_ab$pop=='bonavantura','mean']
y <- gan_ab[gan_ab$pop=='bonavantura','year']

tot <- ddply(gan_ab,c('pop'),function(x) interpol(x$year,x$total_low,"spline")) #gam and linear also tested
names(tot)[5] <-'low'
tot$high <- ddply(gan_ab,c('pop'),function(x) interpol(x$year,x$total_high,'spline'))$interpol
names(tot)[2] <- 'year'
ggplot(tot,aes(x=year))+geom_point(aes(y=pred),col='red')+geom_point(aes(y=y))
ggplot(tot,aes(x=year,ymin=low,ymax=high,fill=pop))+geom_ribbon() # needs to all be the same time frame

t <- ddply(tot,'year',summarise,low=sum(low),high=sum(high))
ggplot(t,aes(x=year,ymin=low,ymax=high))+geom_ribbon()
