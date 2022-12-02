######################################################################################################
# Benoit 2011
# Check the Kleiber equation because some weird going on
# Check to replicate the seasonal growth curve (mass vs age)
######################################################################################################


### Kleiber equation ---------------------------------------------------------------------------------
det <- FALSE
if(det) cov <- matrix(rep(0,4),nrow=2) else cov <- matrix(c(5393.1,-12.393,-12.393,0.035),nrow=2,byrow=T)

ab <- c(293.75,0.75)
test <- rmvnorm(1000,ab,cov)
test <- cbind(test,rep(1:250,4))
test <- cbind(test,apply(test,1,function(x) x[1]*x[3]^x[2]))
df <- data.frame(test)
names(df) <- c('alpha','beta','W','DI')

ggplot(df,aes(x=W,y=DI))+
    geom_point()

### Seasonal Gompertz --------------------------------------------------------------------------------
# Parameters
agemax <- 40
par.annual <- read.input("data/grey_seals/Gulf/adults/GOMP.txt")

df <- expand.grid(age=0:agemax,sex=c('f','m'))

## combine params
W <- reshape2::dcast(par.annual,sex~parameter,value.var = 'mean')   # deterministic mean value...
W <- merge(df,W)

## get the weight
W <- seal.W(W$age,W$sex,W$gamma.1,W$gamma.2,W$gamma.3,month=1:12)
#W <- seal.W(age=0:agemax,sex=W$sex[1],gamma.1=W$gamma.1[1],gamma.2=W$gamma.2[1],gamma.3=W$gamma.3[1],month=1:12) # test with just one scenario

# plot (exactly the same as figure in Benoit 2011)
p0 <- ggplot(W,aes(x=age.dec,y=W,col=sex))+
    geom_line()+
    labs(x='Age (yr)',y='Mass (kg)',col='')+
    scale_y_continuous(expand=c(0,0),limits=c(0,300))+
    theme(legend.position = c(0.75,0.2))+
    scale_color_manual(values=c('darkgrey','black'),labels=c('Females','Males'))


saveplot(p0,'W_seals',wd= 'img/data_supp/grey_seals',c(12,8))

p1 <- ggplot(W,aes(x=age.dec,y=W,col=sex))+
    geom_line()+
    geom_point(data=W[W$month==6,])+
    labs(x='Age (yr)',y='Mass (kg)',col='')+
    scale_y_continuous(expand=c(0,0))+
    theme(legend.position = c(0.75,0.2))+
    scale_color_manual(values=c('darkgrey','black'),labels=c('Females','Males'))

saveplot(p1,'W_seals_june',wd= 'img/data_supp/grey_seals',c(12,8))


p2 <- ggplot(W,aes(x=month,y=W,col=factor(age)))+
    geom_line()+
    facet_wrap(.~sex)+
    labs(x='Month',y='Mass (kg)',col='Age')+
    scale_color_viridis_d(direction = -1)
saveplot(p2,'W_seals_monthly',wd= 'img/data_supp/grey_seals',c(20,12))

