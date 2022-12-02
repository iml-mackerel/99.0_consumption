######################################################################################################
# Get Na,s,h,y for seals
# Raw Robjects from Steven Rossi (fall seal assessment)
######################################################################################################

####### DATA  ########################################################################################
wd <- 'data_supp/seals'
wdimg <- 'img/data_supp/grey_seals'

weighted.sd <- function(mu,x,w){if(sum(w)>1)sqrt(sum(w*(x-mu)^2)/(sum(w)-1))else NA}

### get CC values  --------------------------------------------------------------------------------------
cc <- read.csv2(paste0(wd,'/caloric content/CC.csv'),sep=',')
cc <- type.convert(cc)

d <- melt(cc,id=c('Scientific.name','Common.name','w'),variable.name = 'source',value.name = 'cc')
ggplot(d,aes(x=cc))+
    geom_histogram()+
    facet_grid(source~.)

ddply(d,c('source'),function(x){
    m <- weighted.mean(x$cc,x$w,na.rm = TRUE)
    sd <- sqrt(Hmisc::wtd.var(x$cc,x$w,na.rm = TRUE))
    return(c(m=m,sd=sd))
})

labs <- as_labeller(c(`hammill` = "Hammill et al. (2007) - Not season-specific", `beck_summer` = "Beck et al (2007) - Summer"))
p <- ggplot(d[d$source %in% c('hammill','beck_summer'),],aes(x=cc))+
    geom_histogram()+
    facet_wrap(source~.,labeller = labs,ncol=1)+
    ylab("Count")+
    xlab("'Caloric Content (kj/g)'")

saveplot(p,'CC_seals',wd=wdimg,c(15,10)) 



