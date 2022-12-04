######################################################################################################
# Plot SSB vs M
######################################################################################################

wdimg <- 'img/MSSB'

### total consumption
load(file='Rdata/consum.tot.Rdata')

### Model output
year <- 2020
url <- paste0("https://github.com/iml-mackerel/0.0_model/blob/master/Rdata/",year,"/fit.Rdata?raw=true")
download.file(url, destfile= "./data_supp/mackerel/fit.RData", mode = "wb")
load("./data_supp/mackerel/fit.RData")

ssb <- CCAM::ssbtable(fit)
ssb$M <- fit$data$natMor[1,1]
ssb$Mabs <- with(ssb,Estimate*(1-exp(-M)))
ssb$Mabslow <- with(ssb,Low*(1-exp(-M)))
ssb$Mabshigh <- with(ssb,High*(1-exp(-M)))

logobs <- data.frame(cbind(fit$data$aux,fit$data$logobs))
catch <- logobs[logobs$fleet==1,]
catch[,c(4:5)] <- exp(catch[,c(4:5)])
catch$year <- catch$year+1967

all <- merge(ssb,catch)

p1 <- ggplot(all,aes(x=year))+
    geom_ribbon(aes(ymin=Low/1000,ymax=High/1000),fill='lightgrey')+
    geom_line(size=1,aes(y=Estimate/1000))+
    geom_ribbon(aes(ymin=Mabslow/1000,ymax=Mabshigh/1000),fill='red',alpha=0.5)+
    geom_line(aes(y=Mabs/1000),col='red',size=1)+
    labs(y='SSB (kt)',x='Year')+
    scale_y_continuous(expand = c(0,0),limits=c(0,700))+
    scale_x_continuous(expand = c(0,0),limits=c(min(ssb$year),max(ssb$year)+1))

saveplot(p1,'SSB_M',wd=wdimg,c(15,10))

p2 <- p1+geom_ribbon(aes(ymin=aux1/1000,ymax=aux2/1000),fill='blue',alpha=0.5)

saveplot(p2,'SSB_M_Catch',wd=wdimg,c(15,10))

p3 <- p2+
    geom_ribbon(data=consum.tot[consum.tot$year<=year,],aes(ymin=low/1000,ymax=high/1000,y=NULL),fill='orange',alpha=0.5)+
    geom_line(data=consum.tot[consum.tot$year<=year,],aes(y=med/1000),col='orange',size=1)

saveplot(p3,'SSB_M_Catch_consum',wd=wdimg,c(15,10))

p4 <- ggplot(all,aes(x=year))+
    geom_ribbon(aes(ymin=Low/1000,ymax=High/1000),fill='lightgrey')+
    geom_line(size=1,aes(y=Estimate/1000))+
    geom_ribbon(aes(ymin=Mabslow/1000,ymax=Mabshigh/1000),fill='red',alpha=0.5)+
    geom_line(aes(y=Mabs/1000),col='red',size=1)+
    geom_ribbon(data=consum.tot[consum.tot$year<=year,],aes(ymin=low/1000,ymax=high/1000,y=NULL),fill='orange',alpha=0.5)+
    geom_line(data=consum.tot[consum.tot$year<=year,],aes(y=med/1000),col='orange',size=1)+
    labs(y='SSB (kt)',x='Year')+
    scale_x_continuous(expand = c(0,0),limits=c(2010,year+1))+
    scale_y_continuous(expand=c(0,0),limits=c(0,100))

saveplot(p4,'SSB_M_consum_zoom',wd=wdimg,c(15,10))

p5 <- ggplot(all,aes(x=year))+
    geom_ribbon(aes(ymin=Low/1000,ymax=High/1000),fill='lightgrey')+
    geom_line(size=1,aes(y=Estimate/1000))+
    geom_ribbon(aes(ymin=aux1/1000,ymax=aux2/1000),fill='blue',alpha=0.5)+
    geom_ribbon(data=consum.tot[consum.tot$year<=year,],aes(ymin=low/1000,ymax=high/1000,y=NULL),fill='orange',alpha=0.5)+
    geom_line(data=consum.tot[consum.tot$year<=year,],aes(y=med/1000),col='orange',size=1)+
    labs(y='SSB (kt)',x='Year')+
    scale_x_continuous(expand = c(0,0),limits=c(2010,year+1))+
    scale_y_continuous(expand=c(0,0),limits=c(0,100))

saveplot(p5,'SSB_catch_consum_zoom',wd=wdimg,c(15,10))
