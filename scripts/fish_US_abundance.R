######################################################################################################
# Get mackerel abundance from US groundfish
# source: Brian Smith
######################################################################################################

####### DATA  ########################################################################################
wd <- 'data_supp/groundfish/abundance'
wdimg <- 'img/data_supp/groundfish'

ab <- read.csv2(paste0(wd,"/abundance.csv"),sep=',')[,c('year','abunpop','abunpop.sd','comname')] # fall abundance!!
ab$comname <-  paste(substr(ab$comname, 1, 1), tolower(substr(ab$comname, 2, nchar(ab$comname))), sep="")
names(ab) <- c('year','abundance','sd','species')
ab <- type.convert(ab,as.is=TRUE)

load(usfishn,file="Rdata/usfishn.Rdata") #only species that are actually used
ab <- merge(usfishn,ab,all.x = TRUE)


p <- ggplot(ab,aes(x=year,y=abundance/10^6))+
    geom_ribbon(aes(ymin=pmax(abundance/10^6-2*sd/10^6,0),ymax=abundance/10^6+2*sd/10^6),fill='grey')+
    geom_line()+
    facet_wrap(species~.,scale='free_y',ncol=4)+
    labs(y='Abundance (millions)',x='Year')+
    scale_y_continuous(expand = c(0,0))

saveplot(p,'N_USgroundfish_fall',wd=wdimg,c(25,15))

plot(y=ab[ab$species=='Spiny dogfish','abundance'],x=ab[ab$species=='Spiny dogfish','year'],type='l')
