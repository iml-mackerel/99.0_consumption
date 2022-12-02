######################################################################################################
# Code and data from Alex
######################################################################################################

load("data_supp/tuna/W/SWNS_trip_unrestricted_2002_2022.RData")
library(stringr)
library(data.table)

# Housekeeping
marfis.bftlog[FLEET=="QC", FLEET := "PQ"]

# Other tuna catch
marfis.bftlog[, OTC := sum(BET_DRESSED_WEIGHT_LBS_TRIP,
                           YFT_DRESSED_WEIGHT_LBS_TRIP,
                           ALB_DRESSED_WEIGHT_LBS_TRIP, na.rm=T), TRIP_ID]

# Try to assign a FLEET to the unknowns
marfis.bftlog[, UF := length(unique(FLEET)), VR_NUMBER]
marfis.bftlog[, FLEET2 := paste(unique(FLEET), collapse="_"),VR_NUMBER]
marfis.bftlog[FLEET=="UNK"&UF==2, FLEET2 := sapply(str_split(marfis.bftlog[FLEET=="UNK"&UF==2]$FLEET2,"_"), function(x) x[-match("UNK",x)])]
marfis.bftlog[FLEET=="UNK"&UF==2, FLEET := FLEET2]

SWNS9 = data.table(marfis.bftlog)

SWNS9[,Round_Weight:=as.character(cut(BFT_LANDED_WEIGHT_LBS*0.453592,breaks = seq(50,600,25)))]
SWNS9[,Round_Weight:= str_replace(str_split_fixed(Round_Weight,"[,]",n=2)[,1],"[\\(]","")]
#SWNS9[,Round_weight:=matrix(unlist(strsplit(Round_Weight,",")),ncol=2,byrow = T)[,1]]
# Reported weight is dressed. Round weight is 1.25 x reported weight.

ggplot(SWNS9[YEAR>2003&BFT_LANDED_WEIGHT_LBS*0.453592<400&BFT_LANDED_WEIGHT_LBS*0.453592>10], aes(y=BFT_LANDED_WEIGHT_LBS*0.453592,x=factor(YEAR)))+ geom_violin(fill="grey") + geom_hline(yintercept = 175,col="red") + xlab("Year") +stat_summary(fun.y=median, geom="point", size=2, color="purple") + theme_bw() + ylab("RND_WEIGHT_KGS") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

ggplot(SWNS9[YEAR>2002&BFT_LANDED_WEIGHT_LBS*0.453592<400], aes(x=as.numeric(Round_Weight)))+ geom_bar() + geom_vline(xintercept = 175,col="red") + facet_wrap(~YEAR,ncol=4) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) + xlab("Round weight (kg)")+theme_bw()

SWNS9[order(YEAR),][BFT_LANDED_WEIGHT_LBS>0,mean(BFT_LANDED_WEIGHT_LBS*0.453592, na.rm=T),YEAR]
SWNS9[BFT_LANDED_WEIGHT_LBS>0,mean(BFT_LANDED_WEIGHT_LBS*0.453592, na.rm=T)]

## save by eli
Wss <- SWNS9[YEAR>2003&BFT_LANDED_WEIGHT_LBS*0.453592<400&BFT_LANDED_WEIGHT_LBS*0.453592>10,c('BFT_LANDED_WEIGHT_LBS','YEAR')]
Wss$BFT_LANDED_WEIGHT_LBS <- Wss$BFT_LANDED_WEIGHT_LBS*0.453592
names(Wss) <- c('weight','year')
save(Wss,file='data_supp/tuna/W/Wss.Rdata')
