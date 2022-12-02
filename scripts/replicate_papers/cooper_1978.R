#########################################################################
##### get uncertainty around assimilation efficiency from Cooper 1978 ###
##### from Guillemete 2018: deterministic 75%                         ###
#########################################################################

######## DATA ############################################################
## from table 1
AE <- c(69.2,74.2,77.3,80.2,65.8,81.3,77.7,73.7,75,72.3)
range(AE)
age <- c(10,20,30,41,51,61,71,83,93,98)
plot(age,AE,col='red',pch=16)

# linear interpolation because ages are irregular 
AEagei <- interpol(age,AE,plot=FALSE)
lines(AEagei$x,AEagei$pred)
mean(AEagei$interpol)
sd(AEagei$interpol)
