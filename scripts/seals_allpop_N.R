######################################################################################################
# Get Na,s,h,y for seals
# Raw Robjects from Steven Rossi (fall seal assessment)
######################################################################################################

####### DATA  ########################################################################################
wd <- 'data_supp/seals'
wdimg <- 'img/data_supp/grey_seals'

f <- list.files(wd,full.names = T,recursive = TRUE)

### get N matrix  --------------------------------------------------------------------------------------
fn <- f[grep("Nposts",f)]
Narrays <- lapply(fn,function(x)get(load(x)))
Nlists <- lapply(Narrays,melt,varnames=c('sim','age','year','sex','herd'))
names(Nlists) <- gsub("data_supp/seals/abundance/","",gsub("/Nposts.Rdata","",fn))
N <- dplyr::bind_rows(Nlists, .id = 'model')                                         # N at timing of the pup production surveys (Mid jan: why not jan 1st though)
N$Mid <- apply(N[,c('model','sim','age','sex')],1,paste,collapse="_")

# check (see Fig. 9 in draft resdoc. shelf goes to 300k and gul to 55k ) -------------------------------
sim <- ddply(N,c('sim','year','herd','model'),summarise, value=sum(value))
sim <- ddply(sim,c('year','herd'),summarise, median=median(value),lower=quantile(value,0.025),upper=quantile(value,0.975))
ggplot(sim, aes(x=year,y=median))+
    geom_ribbon(aes(ymin=lower,ymax=upper,fill=factor(herd)),alpha=0.2)+
    geom_line(aes(col=factor(herd)))+
    scale_y_continuous(expand=c(0,0))+
    labs(col='Herd',fill='Herd',x='Year',y='N (1000s)')

### get M matrix (to get N at 1st of june) ---------------------------------------------------------------
fm <- f[grep("Mposts",f)]
Marrays <- lapply(fm,function(x)get(load(x)))
Mlists <- lapply(Marrays,melt,varnames=c('sim','age','sex'))
names(Mlists) <- gsub("data_supp/seals/abundance/","",gsub("/Mposts.Rdata","",fm))
M <- dplyr::bind_rows(Mlists, .id = 'model')
M$Mid <- apply(M[,c('model','sim','age','sex')],1,paste,collapse="_")

### combine ----------------------------------------------------------------------------------------------
N$M <- M$value[match(N$Mid, M$Mid)]                          # faster than merge and join

### clean and save ---------------------------------------------------------------------------------------
nsim <- 300                                                  # number of samples to keep per level (currently 2000, also per model)

N$value <- N$value*1000                                      # thousands to numbers
N$N <- N$value*exp(-N$M*5/12)                                # early june
N$year <- N$year+1959                                        # 1960...2022
N$sex <- c('m','f')[N$sex]
N$herd <- c('Shelf','Gulf')[N$herd]
N$age <- N$age-1                                              # 0-30

Nsave <- N[N$sim %in% 1:(nsim/length(unique(N$model))),]
Nsave <- ddply(Nsave,c('age','year','sex','herd'),summarise,sim=1:nsim,N=N)
Nsave <- Nsave[,c('sim','age','year','sex','herd','N')]           # 62496000 rows!!!

class(Nsave) <- c('data.frame','boot')
save(Nsave,file="data/grey_seals/Gulf_SS/adults/N.Rdata")               # boostrapped object already

######## plot ###############################################################################
# check (see Fig. 9 in draft resdoc. shelf goes to 300k and gul to 55k )
sim2 <- ddply(N,c('sim','year','herd','model'),summarise, N=sum(N))
sim2 <- ddply(sim2,c('year','herd'),summarise, median=median(N),lower=quantile(N,0.025),upper=quantile(N,0.975))
p0 <- ggplot(sim2, aes(x=year,y=median))+
    geom_ribbon(aes(ymin=lower,ymax=upper),fill='grey')+
    geom_line()+
    scale_y_continuous(expand=c(0,0))+
    facet_wrap(herd~.,scale='free_y')+
    labs(col='Herd',fill='Herd',x='Year',y='Abundance (thousands)')

saveplot(p0,'N_seals',wd=wdimg,c(20,10))

# check with reduced number of simulations (300 without model, instead of 2000 with model level)
sim2 <- ddply(Nsave,c('sim','year','herd'),summarise, N=sum(N))
sim2 <- ddply(sim2,c('year','herd'),summarise, median=median(N),lower=quantile(N,0.025),upper=quantile(N,0.975))
p <- ggplot(sim2, aes(x=year,y=median))+
    geom_ribbon(aes(ymin=lower,ymax=upper),fill='grey')+
    geom_line()+
    scale_y_continuous(expand=c(0,0))+
    facet_wrap(herd~.,scale='free_y')+
    labs(col='Herd',fill='Herd',x='Year',y='Abundance')

saveplot(p,paste0('N_seals_sim',nsim),wd=wdimg,c(20,10))
