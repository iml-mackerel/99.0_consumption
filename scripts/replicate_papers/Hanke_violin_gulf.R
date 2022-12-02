######################################################################################################
# Code and data from Alex
######################################################################################################

load("data_supp/tuna/W/2021GULF.RData")
library(stringr)
GULF2 = GULF 
GULF2[,Round_Weight:=as.character(cut(RND_WEIGHT_KGS_SLIP,breaks = seq(50,600,25)))]
GULF2[,Round_Weight:= str_replace(str_split_fixed(Round_Weight,"[,]",n=2)[,1],"[\\(]","")]
#GULF2[,Round_weight:=matrix(unlist(strsplit(Round_Weight,",")),ncol=2,byrow = T)[,1]]
# Reported weight is dressed. Round weight is 1.25 x reported weight.

ggplot(GULF2[RND_WEIGHT_KGS_SLIP<600], aes(y=RND_WEIGHT_KGS_SLIP,x=factor(Year)))+ geom_violin(fill="grey") + geom_hline(yintercept = 250,col="red") + xlab("Year") +stat_summary(fun.y=median, geom="point", size=2, color="purple") + theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

ggplot(GULF2[!is.na(RND_WEIGHT_KGS_SLIP)&RND_WEIGHT_KGS_SLIP<600], aes(x=as.numeric(Round_Weight)))+ geom_bar() + geom_vline(xintercept = 250,col="red") + facet_wrap(~Year,ncol=4) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) + xlab("Round weight (kg)")+theme_bw()


GULF2[RND_WEIGHT_KGS_SLIP>0,mean(RND_WEIGHT_KGS_SLIP, na.rm=T),Year]
GULF2[RND_WEIGHT_KGS_SLIP>0,mean(RND_WEIGHT_KGS_SLIP, na.rm=T)]

ggplot(GULF2[RND_WEIGHT_KGS_SLIP<600], aes(y=RPT_WEIGHT_KGS_SLIP,x=RND_WEIGHT_KGS_SLIP))+ geom_point() + geom_hline(yintercept = 350,col="red")

ggplot(GULF2, aes(x=Round_Weight))+ geom_bar() + geom_vline(yintercept = 350,col="red")

GULF2[RND_WEIGHT_KGS_SLIP<600,lm(RND_WEIGHT_KGS_SLIP~RPT_WEIGHT_KGS_SLIP)]

GULF2[order(Year,)][!is.na(RND_WEIGHT_KGS_SLIP)&RND_WEIGHT_KGS_SLIP<600,mean(RND_WEIGHT_KGS_SLIP),Year]
GULF2[!is.na(RND_WEIGHT_KGS_SLIP)&RND_WEIGHT_KGS_SLIP<600,sum(RND_WEIGHT_KGS_SLIP<350)/length(RND_WEIGHT_KGS_SLIP),Year]
GULF2[!is.na(RND_WEIGHT_KGS_SLIP)&RND_WEIGHT_KGS_SLIP<600,mean(RND_WEIGHT_KGS_SLIP),]


## save by eli
Wgsl <- GULF2[RND_WEIGHT_KGS_SLIP<600,c('RND_WEIGHT_KGS_SLIP','Year')]
names(Wgsl) <- c('weight','year')
save(Wgsl,file='data_supp/tuna/W/Wgsl.Rdata')
