######################################################################################################
# Get mackerel consumption from US groundfish
# source: https://fwdp.shinyapps.io/tm2020/
# notes: mean+-CI do not appear from a (log)normal distribution so unsure how to resample:
# notes: DEPRICATED: got new data from Brian Smith (with spring/fall abundances)
######################################################################################################

####### total consumption  ########################################################################################
wd <- 'data_supp/groundfish/consumption_mackerel'
wdimg <- 'img/data_supp/groundfish'

f <- list.files(wd,full.names = T,recursive = TRUE)
usfish <- lapply(f, read.csv)
usfish <- do.call('rbind',usfish)
usfish$X <- NULL
usfish$Prey <- NULL
names(usfish) <- c('species','year','mean','high','low')
usfish$population <- 'US'
usfish[,c('mean','high','low')] <- usfish[,c('mean','high','low')]*1000 # kt to t
usfish$speciesnolat <- gsub("\\s*\\([^\\)]+\\)",'',usfish$species)

# plot origingal consumption estimates (all mackerel, all year) 
p0 <- ggplot(usfish, aes(x=year,y=mean))+
    geom_ribbon(aes(ymin=low,ymax=high),alpha=0.5)+
    geom_line()+
    facet_wrap(~species,scale='free_y')+
    labs(y='Mackerel consumption (t)',x='Year')

saveplot(p0,'C_USgroundfish_rawtotal',wd=wdimg,c(25,15))

####### spring consumption  ########################################################################################

# by what do I need to multiple the average %W that brian used to get back to Ws or the %W in the spring, which I want?
# (ns*ws + nw*wf)/(ns*nf)*X=Ws 
# X=(Ws*ns*nf)/(ns*Ws+nf*Wf)
# (2*sqrt(((ns-1)*cis/2^2+(nf-1)*cif/2^2)/(ns+nf-2))*X=cis
# x=cis/(2*sqrt(((ns-1)*cis/2^2+(nf-1)*cif/2^2)/(ns+nf-2))

USdiet2 <- read.csv("data_supp/groundfish/diet_composition_brian/diet_composition.csv")
USdiet2m <- USdiet2[USdiet2$prey=="SCOSCO",]
USdiet2m$comname <- paste(substr(USdiet2m$comname, 1, 1), tolower(substr(USdiet2m$comname, 2, nchar(USdiet2m$comname))), sep="")

cor <- ddply(USdiet2m,c('year','comname'),function(x,weighted=FALSE){
    n <- x[x$seacat=='SPRING',]$nstom
    if(!weighted)x$nstom=2
    f <- x[x$seacat=='FALL',]
    s <- x[x$seacat=='SPRING',]
    x <- (s$relmsw2*s$nstom*f$nstom)/(s$nstom*s$relmsw2+f$nstom*f$relmsw2)
    x <- ifelse(f$relmsw2==0 &s$relmsw2==0,1,x)
    ssd <- s$relci2/2
    fsd <- f$relci2/2
    xci <- s$relci2/(sqrt(((s$nstom-1)*ssd^2+(f$nstom-1)*fsd^2)/(s$nstom+f$nstom-2))*2)
    xci <- ifelse(f$relci2==0 &s$relci2==0,1,xci)
    c(cor=x,corci=xci,wspring=s$relmsw2,wcispring=s$relci2,wn=n)
})

names(cor)[2] <-'speciesnolat'

usfish <- merge(usfish,cor,all.x = TRUE)  # need to do this for CI as well? unsure
usfish$mean.spring <- with(usfish,mean*cor)/2  # 6 months
usfish$low.spring <- with(usfish,low*corci)/2 
usfish$high.spring <- with(usfish,high*corci)/2
usfish <- usfish[usfish$speciesnolat!="Bluefish",]  # nearly all observatiosn from fall

# plots
# spring (6 months consumption estimates)
ps <- ggplot(usfish, aes(x=year,y=mean.spring))+
    geom_ribbon(aes(ymin=low.spring,ymax=high.spring),alpha=0.5)+
    geom_line()+
    facet_wrap(~species,scale='free_y')+
    labs(y='Mackerel consumption spring (t)',x='Year')

saveplot(ps,'C_USgroundfish_spring6months',wd=wdimg,c(25,15))

# $W spring used for each species
usfish$nclass <- ifelse(usfish$wn<51,"25-50",
                          ifelse(usfish$wn<101,"51-100",
                                 ifelse(usfish$wn<251,"101-250","251+")))
usfish$nclass <- factor(usfish$nclass,labels = c("25-50","51-100","101-250","251+"))

pw <- ggplot(usfish,aes(x=year,y=wspring,ymin=wspring-wcispring,ymax=pmin(wspring+wcispring,100),col=nclass))+
    geom_errorbar()+
    geom_point(size=1)+
    facet_wrap(.~speciesnolat,ncol=4)+
    theme(axis.text.x = element_text(angle=90,hjust=0),legend.position='top')+
    labs(x='Year',y='%W',col='Number of stomachs')+
    scale_color_viridis_d()
saveplot(pw,'W_USgroundfish_spring',wd=wdimg,c(30,30))

####### interpolated spring consumption  ########################################################################################
par(mfrow=c(4,4))
usfishnn <- ddply(usfish,c('speciesnolat'),function(x){interpol(x=x[!is.na(x$mean.spring),]$year,y=x[!is.na(x$mean.spring),]$mean.spring,method = 'linear',from=1968,to=2021)})
usfishnn[,c('low','lowraw')] <- ddply(usfish,c('species'),function(x){interpol(x=x[!is.na(x$mean.spring),]$year,y=x[!is.na(x$mean.spring),]$low.spring,method = 'linear',from=1968,to=2021,plot=FALSE)})[,c('interpol','y')]
usfishnn[,c('high','highraw')] <- ddply(usfish,c('species'),function(x){interpol(x=x[!is.na(x$mean.spring),]$year,y=x[!is.na(x$mean.spring),]$high.spring,method = 'linear',from=1968,to=2021,plot=FALSE)})[,c('interpol','y')]

p2 <- ggplot(usfishnn, aes(x=x,y=interpol))+
    geom_errorbar(aes(ymin=low,ymax=high),col='darkgrey',width=0)+
    geom_errorbar(aes(ymin=lowraw,ymax=highraw),width=0)+
    geom_point(col='darkgrey',size=1)+
    geom_point(aes(y=y),size=1)+
    facet_wrap(~speciesnolat,scale='free_y')+
    labs(y='Mackerel consumption (t)',x='Year')

saveplot(p2,'C_USgroundfish_spring6months_interpolated',wd=wdimg,c(30,15))

####### northern contingent interpolated spring consumption  ########################################################################################
## estimate mass eaten of northern contingent specifically
usfishn <- usfishnn[,c('speciesnolat','x','interpol','low','high')]
names(usfishn) <- c('species','year','mean','low','high')
prop.winter <- c(1,1)        # keep the whole 6 months
prop.north <- c(0.2,0.8)   # proportion of northern fish

#  resampling function
resamp <- function(mean,high,low,prop.winter,prop.north,nsim,summarise=TRUE){
    mea <<- mean;high <<-high;low<<-low
    if(is.na(mean)) return(rep(NA,ifelse(summarise,3,nsim))) else{
        if(as.numeric(mean)==0) return(rep(0,ifelse(summarise,3,nsim))) else{
            m <- runif(nsim,min=prop.winter[1],max=prop.winter[2])
            p <- runif(nsim,min=prop.north[1],max=prop.north[2])
            logsd <- (log(as.numeric(high))-log(as.numeric(mean)))/2 # assumed a lognormal (though not 100% correct) for upper part. Only lower limit will be too low (but doesn't really matter because close to 0 anyway)
            logmu <- log(as.numeric(mean))
            c <- rnorm(nsim,mean = logmu,sd=logsd) # see whether this was no on a log scale originally!!! otherwise wrong here
            new <- exp(c)*m*p
            if(summarise) new <- c(quantile(new,0.025),median(new),quantile(new,0.975))
            return(new)
        }
    }
}

# get estimates for the northern contingent
usfishn[,c('low','mean','high')] <- data.frame(t(apply(usfishn,1,function(x){resamp(x['mean'],x['high'],x['low'],prop.winter,prop.north,nsim=10000)})))

p1 <- ggplot(usfishn, aes(x=year,y=mean))+
    geom_ribbon(aes(ymin=low,ymax=high),alpha=0.5)+
    geom_line()+
    facet_wrap(~species,scale='free_y')+
    labs(y='Mackerel consumption (t)',x='Year')

saveplot(p1,'C_USgroundfish_spring6months_interpolated_northerncontingent',wd=wdimg,c(25,15))
save(usfishn,file="Rdata/usfishn.Rdata")

# get the  bootstrapped data for merging with all other data
nsim=100
usfishb <- usfishnn[,c('speciesnolat','x','interpol','low','high')]
names(usfishb) <- c('species','year','mean','low','high')
usfishb$species <- gsub(" \\(.*","",usfishb$species)

consum.usgroundfish <- ddply(usfishb,c('species','year'),function(x){
    C <- resamp(x[,'mean'],x[,'high'],x[,'low'],prop.winter,prop.north,nsim=nsim,summarise=FALSE)
    cbind(x[,c('species','year')],sim=1:nsim,population='US',lifestage='adults',C)
})

save(consum.usgroundfish,file="Rdata/consum.usgroundfish.100sim.Rdata")





