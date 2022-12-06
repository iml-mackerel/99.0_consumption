######################################################################################################
# Estimate consumption of mackerel by various predators
######################################################################################################

####### CALCULATIONS  ##############################
wd <- 'data'
wdimg <- 'img/consumption'
avail <- avail.predator(wd)
avail

# 1) get input data -------------------------------
input <- apply(avail,1,read.predator)                  # to remove warning about incomplete final year: add an enter at the end of the datafile
names(input) <- avail$wd

# 2) bootstrap data  ----------------------------
overwrite.b <- FALSE
nsim <- 1000                                            # For seals:  only 300!!! Save N.Rdata with correct number of simulations!!
dr <- "Rdata/boot/"
dir.create(dr,showWarnings = FALSE)

pred <- lapply(1:length(input),function(x){                                         #option(warn=2);option(warn=1)
      print(avail[x,'wd'])
      f <- paste0(dr,avail[x,'id'],".Rdata")
      if(!file.exists(f)|overwrite.b){
          b <- boot.predator(input[[x]],nsim=nsim,lower=0.001,deterministic=FALSE)
          save(b,file=f)
          return(b)
      }else{
          b <- get(load(f))
          return(b)
      } 
    }
)    
names(pred) <- names(input)

save(pred,file="Rdata/pred.Rdata")

# 3) estimate consumption  ----------------------------
overwrite.c <- FALSE
dr <- "Rdata/consum/"
dir.create(dr,showWarnings = FALSE)

consum <- lapply(1:length(pred),function(x){
    print(names(pred)[[x]])
    f <- paste0(dr,avail[x,'id'],".Rdata")
    if(!file.exists(f)|overwrite.c){
      # get weight (group dependent)
      if(all(c('age','gamma.1','gamma.2','gamma.3') %in% names(pred[[x]]))){
          if(grepl('seal',names(pred)[x])){
              pred[[x]] <- pred[[x]][order(pred[[x]]$age,pred[[x]]$sex),]
              # draw weight from months 6 to 10
              agemax <- max(pred[[x]]$age)
              pred[[x]]$W <- sapply(1:nrow(pred[[x]]),function(i){
                                                        weights <- seal.W(agemax,
                                                                          pred[[x]][i,'sex'],
                                                                          pred[[x]][i,'gamma.1'],
                                                                          pred[[x]][i,'gamma.2'],
                                                                          pred[[x]][i,'gamma.3'],
                                                                          thisage=pred[[x]][i,'age'],  
                                                                          thismonth=6:10)$W
                                                        sample(weights,1)
                }) 
          }else{
              pred[[x]]$W <- W.gompertz(pred[[x]]) 
          }
      } 
     
      # consumption estimate
      d <- consum.predator(pred[[x]])                   
      
      # metadata
      i <- strsplit(names(pred)[[x]],'/')[[1]]
      d <- cbind(lifestage=i[4],d)
      d <- cbind(population=i[3],d)
      d <- cbind(species=i[2],d)
      save(d,file=f)
      return(d)
    }else{
      d <- get(load(f))
      return(d)
    }
})
load('Rdata/consum.usgroundfish.Rdata')
consum$usgroundfish <- consum.usgroundfish
consum <- do.call('rbind.fill',consum)

save(consum,file='Rdata/consum.Rdata')

# 4) total estimated consumption  ----------------------------

#load(file='Rdata/consum.Rdata')
groups <- read.csv("data_supp/groups.csv")
consum <- merge(consum,groups,all.x=TRUE)

# exclude cetaceans and padd last years for tuna
with(consum[consum$group!="Cetaceans (Canada)",],table(year,species))

newtuna <- consum[consum$species=="tuna" & consum$year==2019,]  # tuna does not have 2020 and 2021: forward 2019
newtuna$year <- 2020
consum <- rbind(consum,newtuna)
newtuna$year <- 2021
consum <- rbind(consum,newtuna)

# overall median (be careful if not same number of simulations for each species)
consum.tot1 <- ddply(consum[consum$group!="Cetaceans (Canada)",],c('year','species','sim'),summarise,C=sum(C))   # for every simulation, total over ages/herds/etc.
consum.tot1 <- ddply(consum.tot1,c('year','species'),transform,nsim=length(C))   # for every simulation, total over ages/herds/etc.

m <- max(consum.tot1$nsim)

consum.tot2 <- ddply(consum.tot1,c('year','species'), function(x){ # subsample to boost number of sims for thos who have less
    xx<<- x
    n <- unique(x$nsim)
    nmissing <- m-n
    y <- x[sample(nrow(x), nmissing,replace=TRUE), ]
    if(nrow(y)!=0) y$sim <- (n+1):m
    return(rbind(x,y))
})

consum.tot3 <- ddply(consum.tot2,c('year','sim'),summarise,C=sum(C))

consum.tot <- ddply(consum.tot3, c('year'),summarise,low=quantile(C,0.025),med=quantile(C,0.5),high=quantile(C,0.975))   

consum.tot <- consum.tot[consum.tot$year %in% 1968:2021,]

save(consum.tot,file='Rdata/consum.tot.Rdata')
write.table(consum.tot,file="output/consum.txt",row.names = FALSE)

