
library(esd)

path <- "~/ESD_oslo_2018_monsoon_mean_temp"
source(file.path(path,'DS.R'))
source(file.path(path,'DSensemble.R'))
source(file.path(path,'map.R'))
source(file.path(path,'map.station.R'))
source(file.path(path,'plot.R'))

#path.ncep <- file.path(path,'NCEP')
path.era <- file.path(path,'ERAINT')

## Import temperature data:
filename.temp <- file.path(path,"mean.rda")
if(file.exists(filename.temp)) {
  load(filename.temp)
} else {
  X <- read.table(file.path(path,"mean_2017.csv"),header=TRUE,sep=",")
  meta <- read.table(file.path(path,"Stations_local_id.csv"),
                     header=TRUE,sep=",")
  for (id in 1:nrow(meta)) {
    lid <- meta$Local_ID[id]
    Y <- X[X$INDEX==lid,]
    Y[Y <= -999] <- NA
    t <- paste(Y[,2],Y[,3],Y[,4],sep='-')
    z <- zoo(Y[,5],order.by=as.Date(t))
    y <- as.station(z,loc=as.character(meta$Station[id]),lon=meta$Longitude.E.[id],lat=meta$Latitude.N.[id],alt=meta$Elavation.m.[id],param='t2m',unit='degC',src='BMD',stid=lid) 
    if (id == 1) t2m <- y else t2m <- combine.stations(t2m,y)
  }
  ## You can save any object in R with the function 'save' as a '.rda' file
  ## but you can only save and use rda files in R
  attr(t2m,"variable") <- "tmean"
  save(file=filename.temp, t2m, meta)
}


## Extract February-April (or some other months)
it.season <- c('Jun','Jul','Aug')
#it.season <- c('Jan','Feb','Dec')
#it.season <- c('Oct','Oct','Nov')

t2m.4s <- as.4seasons(t2m)
T2M <- subset(t2m.4s, it="jja") #march, april, May

#T2M <- annual(subset(t2m,it=it.season),nmin=90)  #60
## There were some suspicious values with low temperatures - remove those
z <- coredata(T2M)
z[z < 3] <- NA
z -> coredata(T2M)

## Subset in time:
T2M <- subset(T2M, it=c(1981,2017))
## Find number of valid data points at each station:
## Number of valid data points per station (1 and 2 is the direction):
nv <- apply(coredata(T2M),2,'nv')
## Use only stations with more than 32 years of data (recommended fo3 30 yrs):
ok <- nv>=30
T2M <- subset(T2M, is=ok)
# Plot the data
#dev.new()
#diagnose(T2M)
# Fill gaps ith the PCA fill method
T2M <- pcafill(T2M)
dev.new()
map(T2M, FUN='mean',add.text = T, xlim=c(88, 93), ylim=c(20, 27))

#dev.new()
#map(T2M, FUN='max',add.text = T)
dev.new()
map(T2M, FUN='trend',add.text = T)


#plot(T2M,errorbar=FALSE)

# Do PCA analysis of winterminimu m temperature data
#pca.t2m <- PCA(T2M)
#plot(pca.t2m)
#pca.t2m <- subset(pca.t2m,ip=1:2)
#print(names(attributes(pca.t2m)))
#attr(pca.t2m,"unit")

###########Three monthly minimum temperature. This will be the predictor:
t2m.era <- retrieve(file.path(path.era,'erai_tmax.nc'),
                    lon=c(0,180),lat=c(-50,50))

#slp.ncep <- retrieve(file.path(path.ncep,'slp.mon.mean.nc'),
#                     lon=c(0,180),lat=c(-50,50),lev=1000)
#t2m.ncep <- retrieve(file.path(path.ncep,'air.mon.mean.nc'),
#                     lon=c(0,180),lat=c(-50,50),lev=1000)

## Predictor domain:
is.predictor <- list(lon=c(80,100),lat=c(15,45))
T2M.predictor <- subset(t2m.era, is=is.predictor)
#T2M.predictor <- subset(t2m.ncep, is=is.predictor)
#SLP.predictor <- subset(slp.ncep, is=is.predictor)
# 'zoo'is an r-package for means irregular time series

## Calculate the annual mean
#T2M.predictor <- annual(subset(T2M.predictor,it=it.season),nmin=length(it.season))
T2M.4s <- as.4seasons(T2M.predictor)
T2M.predictor <- subset(T2M.4s, it='jja')
#SLP.predictor <- annual(subset(SLP.predictor,it=it.season),nmin=length(it.season))

## Look at the spatial correlation between predictand and predictor
#corfield(subset(T2M,is=1), T2M.predictor)
#corfield(subset(T2M,is=1), SLP.predictor)

## EOF analysis:
#eof <- EOF(T2M.predictor,n=5)
#plot(eof,new=TRUE)

## Try downscaling single stations:
#ds <- DS(subset(T2M,is=10), eof)

## Close all plots and maps:
graphics.off()
## Remove some data objects that take up a lot of space:
##rm("slp.ncep","t2m.ncep","t2m.era","t2m")
gc(reset=TRUE) #clean the r-session

## Downscale an ensemble of GCMs for one station:
# See all the available locations:
print(loc(T2M))
# Select which station to downscale
###i <- 31 ### 19 = Khulna, 1 = Dhaka, Satkhira=21
###print(loc(T2M)[i])

T2M.predictor <- as.annual(T2M.predictor)
T2M <- as.annual(T2M)

####### Testing DS PCA approach ######
# RCP26
## Retrieving GCM data
gcm <- retrieve(file.path(path,'CMIP5/rcp26','tas_Amon_ens_rcp26_000.nc'),lon=c(0,180),lat=c(-50,50))
gcm.4s <- as.4seasons(gcm)
GCM <- as.annual(subset(gcm.4s,it = 'jja'))

comb <- combine(T2M.predictor,as.annual(GCM))

ds.pca <- DS.pca(PCA(T2M),EOF(comb),verbose = FALSE, plot=TRUE)
plot(ds.pca)
dev.copy2pdf(file = file.path(path,paste("ds_tmean_evaluate_rcp26_",paste(it.season,collapse="-"),"_v3.pdf",sep="") ))

# RCP45
## Retrieving GCM data
gcm <- retrieve(file.path(path,'CMIP5/rcp45','tas_Amon_ens_rcp45_000.nc'),lon=c(0,180),lat=c(-50,50))
gcm.4s <- as.4seasons(gcm)
GCM <- as.annual(subset(gcm.4s,it = 'jja'))
dev.copy2pdf(file = file.path(path,paste("ds_tmean_evaluate_",paste(it.season,collapse="-"),"_v3.pdf",sep="") ))

comb <- combine(T2M.predictor,as.annual(GCM))
#browser()
ds.pca <- DS.pca(PCA(T2M),EOF(comb),verbose = FALSE, plot=TRUE)
plot(ds.pca)
dev.copy2pdf(file = file.path(path,paste("ds_tmean_evaluate_rcp45_",paste(it.season,collapse="-"),"_v3.pdf",sep="") ))

## RCP85
##Retrieving GCM data
gcm <- retrieve(file.path(path,'CMIP5/rcp85','tas_Amon_ens_rcp85_000.nc'),lon=c(0,180),lat=c(-50,50))
gcm.4s <- as.4seasons(gcm)
GCM <- as.annual(subset(gcm.4s,it = 'jja'))

comb <- combine(T2M.predictor,as.annual(GCM))
#browser()
ds.pca <- DS.pca(PCA(T2M),EOF(comb),verbose = FALSE, plot=TRUE)
plot(ds.pca)
dev.copy2pdf(file = file.path(path,paste("ds_tmean_evaluate_rcp85_",paste(it.season,collapse="-"),"_v3.pdf",sep="") ))

## Loop across the three rcps
for(rcp in c("rcp26","rcp45","rcp85")) {
  filename.dse <- file.path(path,
                            paste("dsensemble.tmean.",rcp,".",paste(it.season,collapse=""),"v3.rda",sep="") )
  if(file.exists(filename.dse)) {
    load(filename.dse)
  } else {
    
    ## Check if the list dse.all contains ensemble downscaling for the chosen rcp.
    ## If not, then perform ensemble downscaling for the chosen rcp
    # dse.rcp <- DSensemble.annual(subset(T2M,is=i),predictor=T2M.predictor,
    #                              path=file.path(path,"CMIP5"),biascorrect=TRUE,
    #                              it=it.season, #nmin=length(it.season),
    #                              rcp=rcp, pattern="tasmin_Amon_ens_", verbose=FALSE,
    #                              rel.cord=FALSE, lon=is.predictor$lon,
    #                              lat=is.predictor$lat,plot=FALSE)
    ## Downscale PCA based object  
    #browser() 
    dse.pca <- DSensemble.pca(PCA(T2M),predictor=T2M.predictor,
                              path=file.path(path,"CMIP5"),biascorrect=TRUE,
                              it=it.season, select = c(1,4,49,62,63,64),
                              rcp=rcp, pattern="tas_Amon_ens_", verbose=FALSE,
                              rel.cord=FALSE, lon=is.predictor$lon,
                              lat=is.predictor$lat,plot=TRUE)
    print('Done !')
    ## Put the ensemble downscaling (dse.rcp) in the list dse.all
    ## and then save dse.all in the file (filename.dse)
    ## reconvert to station object
    dse <- as.station(dse.pca)
    save(file=filename.dse,dse)
    ## Remove dse.rcp to make space (it has been saved in dse.all) 
    rm("dse.rcp")
    gc(reset=TRUE)
    
  }
}


## Visualize downscaled resuls:############################################################
library(esd)

files.dse <- list.files(path,pattern="dsensemble.tmean",full.names=TRUE)
print(files.dse)
filename.dse <- files.dse[[2]] # 1= Dhaka.rda,    ### 2=Khunla.rda, ### 3=Satkhira
load(filename.dse)
it.season <- attr(dse.all,"it")

## Set the range:
ylim <- c(10,25)  #winter min range
#ylim <- c(15,30)   #pre-monsoon min range
## Plot the results:
rcp <- "rcp45"
dse <- dse.all[[rcp]]
## Produce and save figure:
filename.fig <- file.path(path.fig,
                          paste("dsensemble.tmean.",loc(attr(dse,"station")),
                                ".",paste(it.season,collapse=""),
                                ".",rcp,".png",sep="") )
#pdf(file=filename.fig, width=8, height=6)
dev.new()
plot(dse,ylim=ylim,
     target.show=FALSE,map.show=FALSE,new=FALSE,legend.show=FALSE)
title(paste("Monsoon mean temperature "," (",rcp,")\n",
            loc(attr(dse,"station")),sep=""))
dev.print(png,filename.fig, width=500, height=450)
dev.off()

#dev.copy(file="t2m.annualmean.png") ??
#pdf(file="t2m.seasonalcycle.pdf",width=8,height=6)
#T2M.cycle <- aggregate(t2m,month,FUN='mean')
#plot(T2M.cycle,new=FALSE)
#dev.off()
