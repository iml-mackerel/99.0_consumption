######################################################################################################
# Plot SSB vs M
######################################################################################################

ssb <- read.input('data/atlantic_mackerel/Canada/adult/ssb.txt')
ssb$M <- 0.2
ssb$Mssb <- with(ssb,mean*(1-exp(-M)))

ggplot(ssb,aes(x=year,y=mean/1000))+
    geom_ribbon(aes(ymin=low/1000,ymax=high/1000),fill='lightgrey')+
    geom_line(size=1)+
    geom_line(aes(y=Mssb/1000),col='red',size=1)+
    labs(y='SSB (kt)',x='Year')
