# Install and load required packages

## esd for empirical-statistical downscaling and other analysis
if(!require(esd)) {
  if(!require(devtools)) install.packages("devtools")
  library(devtools)
  install_github("metno/esd")
}
library(esd)

## Cairo for saving figures as tiff-files
if(!require(Cairo)) install.packages("Cairo") 
library(Cairo)

## gcmeval for downloading CMIP5 data ()
if(!require(gcmeval)) {
  if(!require(devtools)) install.packages("devtools")
  library(devtools)
  install_github("metno/gcmeval/back-end")
}

# Set paths
path.fig <- "/path/to/figures"
path.table <- "/path/to/tables"
path.data <- "/path/to/outputdata"
path.era5 <- "/path/to/era5"
path.cmip5 <- "/path/to/cmip5"

# Settings for analysis
visualize <- FALSE # show figures
force <- FALSE # force analysis even if it has been done before
verbose <- TRUE # print information
n.predictand <- 4 # number of predictand patterns
n.predictor <- 10 # number of predictor patterns
is.predictor <- list(lon=c(80,100),lat=c(15,45)) # predictor domain
it.season <- c('Mar','Apr','May') # Pre-monsoon season

# Access CMIP5 data - comment out when the data has been downloaded:
varid <- "tas" # tas = temperature; other options "psl" (pressure), "pr" (precip)
for(rcp in c("rcp26","rcp45","rcp85")) {
  gcmeval::getCMIP5(select=1:110, varid=varid, experiment=rcp, 
                    path=file.path(path.cmip5,rcp))
}

# Import temperature data:
filename.temp <- file.path(path.data,"max.rda")
if(file.exists(filename.temp)) {
  load(filename.temp)
} else {
  X <- read.table(file.path(path.data,"Max_2019.csv"),header=TRUE,sep=",")
  meta <- read.table(file.path(path.data,"Stations_local_id.csv"),
                     header=TRUE,sep=",")
  for (id in 1:nrow(meta)) {
    lid <- meta$Local_ID[id]
    Y <- X[as.numeric(X$INDEX)==as.numeric(lid), ]
    Y[as.numeric(Y[,ncol(Y)])<=-999, ncol(Y)] <- NA
    t <- paste(Y$YEAR, Y$M, Y$D, sep='-')
    z <- zoo(as.numeric(Y[,ncol(Y)]), order.by=as.Date(t))
    y <- as.station(z,loc=as.character(meta$Station[id]),lon=meta$Longitude.E.[id],lat=meta$Latitude.N.[id],alt=meta$Elavation.m.[id],param='t2m',unit='degC',src='BMD',stid=lid) 
    if (id == 1) t2m <- y else t2m <- combine.stations(t2m,y)
  }
  ## You can save any object in R with the function 'save' as a '.rda' file
  ## but you can only save and use rda files in R
  attr(t2m,"variable") <- "tmax"
  save(file=filename.temp, t2m, meta)
}

## Get meta data
M <- read.csv(file.path(path.table,"rashid_tableS1.csv"), sep=",")
stnm <- M[,2]
stid <- M[,3]
stlat <-  M[,4]
stlon <-  M[,5]
stalt <-  M[,6]
streg <- M[,1]
## Correction for stationnames in metadata file
nmcorr <- function(x) switch(x, "Ctg_Patenga"="Chattogram", 
                             "CoxBazar"="Cox's Bazar",
                             "M_Court"="Maijdi Court",
                             "Barisal"="Barishal", 
                             "khepupara"="Khepupara", x)

## Calculate seasonal means
t2m.4s <- as.4seasons(t2m)
T2M <- subset(t2m.4s, it=it.season)
T2M <- as.annual(T2M)
## There were some suspicious values with low temperatures - remove those
z <- coredata(T2M)
z[z < 3] <- NA
z -> coredata(T2M)
## Subset in time:
T2M <- subset(T2M, it=c(1981,2019))
## Find number of valid data points at each station:
nv <- apply(coredata(T2M),2,'nv')
## Use only stations with more than 30 years of data (recommended fo3 30 yrs):
ok <- nv>=30
T2M <- subset(T2M, is=ok)  
## Fill gaps with the PCA fill method
T2M <- pcafill(T2M)
names(T2M) <- attr(T2M,"location")

## Prepare the predictand
pca.predictand <- PCA(T2M, n=10)
print("Explained variance:")
print(100*cumsum(attr(pca.predictand,"eigenvalues")^2)/sum(attr(pca.predictand,"eigenvalues")^2), digit=0)
print(100*attr(pca.predictand,"eigenvalues")^2/sum(attr(pca.predictand,"eigenvalues")^2), digit=0)

## Only 4 PCs explain all variance after pcafill - keep only those.
pca.predictand <- PCA(T2M, n=4)

if(visualize) {
  ## Figure 2
  ## Seaonal cycle of the maximum temperature:
  t2m.cycle <- aggregate(t2m, by=month, FUN="mean")
  CairoTIFF(filename=file.path(path.fig,"tmax_seasoncycle.tiff"), 
            width = 3400, height = 2200, pointsize = 16, bg = "white", res = 300)
  par(mar=c(3.5,5,4,2))
  plot(t2m.cycle, new=FALSE)
  dev.off()

  ## Figure 3
  ## Map of March-May tmax, average (a) and trend (b)
  CairoTIFF(filename=file.path(path.fig,"tmax_maps_avg_trend.tiff"), 
            width = 2400, height = 3400, pointsize = 12, bg = "white", res = 300)
  par(mar=c(5,5,4,2))
  map(T2M, FUN="mean", new=FALSE, xlim=c(87.5,93), ylim=c(20.5,26.5),
      mar=c(2,5,3,5), mgp=c(2,0.75,0), fig=c(0,1,0.53,1), cex.main=1.2,
      main="a) MAM mean tmax (deg C)")
  for(i in seq_along(lon(T2M))) {
    lon.i <- lon(T2M)[i]
    lat.i <- lat(T2M)[i]
    if(loc(T2M)[i] %in% c("Bhola","Hatiya","Rangamati","Satkhira","Dinajpur")) {
      lat.i <- lat.i-0.1
    } else if(loc(T2M)[i] %in% c("Sitakunda","Barishal")) {
      lat.i <- lat.i+0.1; lon.i <- lon.i-0.1
    }
    text(lon.i, lat.i, nmcorr(loc(T2M)[i]), cex=0.9, pos=4)
  }
  map(T2M, FUN="trend", new=FALSE, xlim=c(87.5,93), ylim=c(20.5,26.5),
      xlab="", 
      mar=c(3,5,3,5), mgp=c(2,0.75,0), fig=c(0,1,0.05,0.53), 
      main="b) MAM tmax trend (deg C/decade)", cex.main=1.2,
      colbar=list(pal="t2m", breaks=seq(-0.8,0.8,0.2)), add=TRUE)
  i.significant <- which(apply(T2M, 2, function(x) trend.pval(x)<0.1))
  points(lon(T2M)[i.significant], lat(T2M)[i.significant], 
         pch=1, col="black", cex=1.8)
  mtext("lon", side=1, line=3.5, adj=0.5)
  dev.off()

}


## Question: How is the number of predictand PCs affect the trend?
## Answer: Much of the trends in eastern and south-eastern Bangla lies in PC2 
if(visualize) {
  cb <- list(pal="t2m", breaks=seq(-0.8,0.8,0.1))
  map(T2M, FUN="trend", colbar=cb, new=TRUE,
      main=paste("tmax trend (deg C/decade)\n",
                 "pre-monsoon season, ",paste(range(index(T2M)),collapse="-")))
  i.significant <- which(apply(T2M, 2, function(x) trend.pval(x)<0.1))
  T2M.trends <- apply(T2M[,i.significant], 2, trend.coef)
  for(n.pc in seq(1,4)) {
    print(paste0("nPC = ",n.pc))
    T2M.test <- as.station(PCA(T2M), ip=1:n.pc)
    map(T2M.test, FUN="trend", colbar=cb, new=TRUE,
        main=paste("tmax trend (deg C/decade)",
                   "\npre-monsoon season, ",paste(range(index(T2M)),collapse="-"),
                   "\nbased on",n.pc,"principal components"))
    err <- abs(T2M.trends - apply(T2M.test[,i.significant], 2, trend.coef))
    err.rel <- err/abs(T2M.trends)
    print(round(cbind(T2M.trends, err, err.rel, lon(T2M)[i.significant]-mean(lon(T2M))),2))
  }  
}

## Predictor data - ERA5 maximum temperature
## ERA5 data can be downloaded from the Copernicus Climate Data Store 
print("Import ERA5 predictor data")
t2m.era5 <- retrieve(file.path(path.era5,'ERA5_t2m_mon.nc'),
                     lon=c(50,130),lat=c(0,60))
## ERA5 slp
slp.era5 <- retrieve(file.path(path.era5,'ERA5_slp_mon.nc'),
                     lon=c(50,130),lat=c(0,60))
## ERA5 precip
tp.era5 <- retrieve(file.path(path.era5,'ERA5_tp_mon.nc'),
                     lon=c(50,130),lat=c(0,60))

## Subset predictor domain:
print("Prepare predictor data - t2m")
t2m.predictor <- t2m.era5
T2M.predictor <- subset(t2m.predictor, is=is.predictor)
## Calculate the seasonal mean
T2M.4s <- as.4seasons(T2M.predictor)
T2M.predictor <- subset(T2M.4s, it=it.season)
T2M.predictor <- as.annual(T2M.predictor)
## EOF analysis
eof.t2m.predictor <- EOF(T2M.predictor, n=n.predictor)
print("Explained variance:")
print(100*cumsum(attr(eof.t2m.predictor,"eigenvalues")^2)/sum(attr(eof.t2m.predictor,"eigenvalues")^2), digit=0)
print(100*attr(eof.t2m.predictor,"eigenvalues")^2/sum(attr(eof.t2m.predictor,"eigenvalues")^2), digit=0)

print("Prepare predictor data - slp")
slp.predictor <- slp.era5
SLP.predictor <- subset(slp.predictor, is=is.predictor)
## Calculate the seasonal mean
SLP.4s <- as.4seasons(SLP.predictor)
SLP.predictor <- subset(SLP.4s, it=it.season)
SLP.predictor <- as.annual(SLP.predictor)
## EOF analysis
eof.slp.predictor <- EOF(SLP.predictor, n=n.predictor)
print("Explained variance:")
print(100*cumsum(attr(eof.slp.predictor,"eigenvalues")^2)/sum(attr(eof.slp.predictor,"eigenvalues")^2), digit=0)
print(100*attr(eof.slp.predictor,"eigenvalues")^2/sum(attr(eof.slp.predictor,"eigenvalues")^2), digit=0)

print("Prepare predictor data - precip")
pr.predictor <- tp.era5
PR.predictor <- subset(pr.predictor, is=is.predictor)
## Calculate the seasonal mean
PR.4s <- as.4seasons(PR.predictor)
PR.predictor <- subset(PR.4s, it=it.season)
PR.predictor <- as.annual(PR.predictor)
## EOF analysis
eof.pr.predictor <- EOF(PR.predictor, n=n.predictor)
print("Explained variance:")
print(100*cumsum(attr(eof.pr.predictor,"eigenvalues")^2)/sum(attr(eof.pr.predictor,"eigenvalues")^2), digit=0)
print(100*attr(eof.pr.predictor,"eigenvalues")^2/sum(attr(eof.pr.predictor,"eigenvalues")^2), digit=0)

## Test downloading approach with combined reanalysis and GCM data:
test.ds <- FALSE 
if(test.ds) {
  rcp <- "rcp45"
  files.gcm.t2m <- list.files(path=file.path(path.cmip5,rcp), pattern=paste0("tas.rcp"))
  files.gcm.slp <- list.files(path=file.path(path.cmip5,rcp), pattern=paste0("psl.rcp"))
  files.gcm.pr <- list.files(path=file.path(path.cmip5,rcp), pattern=paste0("pr.rcp"))
  i <- 10
  t2m.gcm1 <- retrieve(file.path(path.cmip5,rcp,files.gcm.t2m[[i]]), 
                      lon=is.predictor$lon+c(-5,5), lat=is.predictor$lat+c(-5,5))
  slp.gcm1 <- retrieve(file.path(path.cmip5,rcp,files.gcm.slp[[i]]), 
                       lon=is.predictor$lon+c(-5,5), lat=is.predictor$lat+c(-5,5))
  pr.gcm1 <- retrieve(file.path(path.cmip5,rcp,files.gcm.pr[[i]]), 
                       lon=is.predictor$lon+c(-5,5), lat=is.predictor$lat+c(-5,5))
  T2M.gcm1 <- as.annual(subset(as.4seasons(t2m.gcm1), it=it.season, is=is.predictor))
  SLP.gcm1 <- as.annual(subset(as.4seasons(slp.gcm1), it=it.season, is=is.predictor))
  PR.gcm1 <- as.annual(subset(as.4seasons(pr.gcm1), it=it.season, is=is.predictor))
  
  ####### Testing DS PCA approach ######
  ds.t2m <- DS.pca(pca.predictand, EOF(combine(T2M.predictor, T2M.gcm1), n=n.predictor), verbose=FALSE, plot=FALSE)
  ds.slp <- DS.pca(pca.predictand, EOF(combine(SLP.predictor, SLP.gcm1), n=n.predictor), verbose=FALSE, plot=FALSE)
  ds.pr <- DS.pca(pca.predictand, EOF(combine(PR.predictor, PR.gcm1), n=n.predictor), verbose=FALSE, plot=FALSE)
  for(i in 1:3) {
    dev.new(); plot(ds.t2m, ip=i)
    dev.new(); plot(ds.slp, ip=i)
    dev.new(); plot(ds.pr, ip=i)
  }
}

if(visualize) {
  ## Calculation time for different numbers of predictor and predictand patterns
  for(n1 in c(5,10,15,20)) {
    print(paste(n1,"predictor patterns"))
    for(n2 in c(2,3,4)) {
      print(paste(n2,"predictand patterns"))
      t1 <- Sys.time()
      ds.pca <- DS.pca(PCA(T2M, n=n2), EOF(T2M.predictor, n=n1), 
                       verbose=FALSE, plot=FALSE)
      t2 <- Sys.time()
      print(t2-t1, units="auto")
    }
  }
  
  ## Perform esd with 4 predictand patterns and 10 predictor patterns
  ds.pca <- DS.pca(PCA(T2M, n=n.predictand), 
                   EOF(T2M.predictor, n=n.predictor), 
                   verbose=FALSE, plot=FALSE)
  ## Evaluation of the results including cross-validation
  ## The first and second plots are Figures 4 and 5 in the paper
  for(pc in seq(ncol(ds.pca))) {
    CairoTIFF(filename=file.path(path.fig,paste0("tmax_dseval_",
                                                   paste(it.season,collapse="-"),
                                                   "_PC",pc,".tiff")), 
                width = 3000, height = 3000, pointsize = 16, bg = "white", res = 300)
    par(mar=c(2,2,2,2))
    plot(ds.pca, ip=pc, new=FALSE)
    dev.off()
  }
}

## Perform ensemble downscaling with various numbers of predictor patterns
## and for different RCPs
iselect <- 1:110
for(neof in seq(10,4,-2)) {
  for(rcp in c("rcp45","rcp85","rcp26")) {
    print(paste("Ensemble downscaling of",rcp,"with",neof,"predictor patterns."))
    filename.dse <- file.path(path.data,"dsensemble",
                              paste("dsensemble.tmax.",rcp,".",
                                    paste(it.season,collapse=""),
                                    ".neof",neof,".v4.rda",sep="") )
    if(file.exists(filename.dse)) {
      load(filename.dse)
    } else {
      dse <- list()
    }
    for(predictor in c("t2m", "slp", "pr")) {
      if(is.null(dse[[paste0("pc",ip)]][[predictor]]) | force) {
        print(paste0("Run DSensemble for pc",ip,", predictor: ",predictor," ",rcp))
        dse.ip <- DSensemble(subset(pca.predictand, ip=1:3), 
                             predictor=switch(predictor, 
                              "t2m"=T2M.predictor, "slp"=SLP.predictor,
                              "pr"=PR.predictor),
                             pattern=switch(predictor, 
                              "t2m"="tas", "slp"="psl", "pr"="pr"), 
                             path=path.cmip5, biascorrect=TRUE,
                             rcp=rcp, it=it.season, select = iselect,
                             ip=1:neof, verbose=verbose,
                             rel.cord=FALSE, lon=is.predictor$lon,
                             lat=is.predictor$lat, plot=FALSE)
        dse[[predictor]] <- dse.ip
        save(file=filename.dse, dse)
      }
    }
    print('Done!')
    rm("dse")
    gc(reset=TRUE)
  }
}

## clean the r-session
graphics.off()
gc(reset=TRUE)

## Cross-validation results for RCP4.5 with 10 predictor patterns
## for various predictor variables
rcp <- "rcp45"
neof <- 10
filename.dse <- file.path(path.data,"dsensemble",
                          paste("dsensemble.tmax.",rcp,".",
                                paste(it.season,collapse=""),
                                ".neof",neof,".v4.rda",sep="") )
load(filename.dse)
for(var in c("t2m","slp","pr")) { ## loop over different predictor variables
  D <- dse[[var]]
  r.crossval <- matrix(NA, ncol=6, nrow=length(D)-2)
  colnames(r.crossval) <- paste0(c("r", "p"), c(1,1,2,2,3,3))
  for(i in seq(3,length(D))) {
    ei <- attr(D[[i]], "evaluation")
    r.crossval[i-2, 1] <- cor.test(ei[,1], ei[,2])$estimate
    r.crossval[i-2, 2] <- cor.test(ei[,1], ei[,2])$p.value
    r.crossval[i-2, 3] <- cor.test(ei[,3], ei[,4])$estimate
    r.crossval[i-2, 4] <- cor.test(ei[,3], ei[,4])$p.value
    r.crossval[i-2, 5] <- cor.test(ei[,5], ei[,6])$estimate
    r.crossval[i-2, 6] <- cor.test(ei[,5], ei[,6])$p.value
  }
  eval(parse(text=paste0("r.",var," <- r.crossval")))
}

## Transform ensemble downscaled results from principal components to 
## estimated projections of tmax for each station 
neof <- 10
for(rcp in c("rcp26", "rcp45", "rcp85")) {
  filename.dse <- file.path(path.data,"dsensemble",
                            paste("dsensemble.tmax.",rcp,".",
                                  paste(it.season,collapse=""),
                                  ".neof",neof,".v4.rda",sep="") )
  load(filename.dse)
  for(npc in c(2,3)) { # number of predictand patterns
    eval(parse(text=paste0("dse.",rcp,".npc",npc," <- as.station(dse$t2m, ip=1:npc)")))
  }
}

dse.rcp26 <- dse.rcp26.npc2
dse.rcp45 <- dse.rcp45.npc2
dse.rcp85 <- dse.rcp85.npc2

## Visualize downscaled resuls:
if(visualize) {
  ylim <- c(-10,10)
  npc <- "npc2"
  for(i in 1:length(dse.rcp26)) {
    rcp26.i <- as.anomaly(dse.rcp26[[i]])
    rcp45.i <- as.anomaly(dse.rcp45[[i]])
    rcp85.i <- as.anomaly(dse.rcp85[[i]])
    # One of these plots is Figure 7 in the paper 
    CairoTIFF(filename=file.path(path.fig,paste0("tmax_dsensemble_",names(dse.rcp26)[[i]],".tiff")), 
              width = 3400, height = 1400, pointsize = 12, bg = "white", res = 300)
    par(mar=c(0,0.1,0,0), mgp=c(1.5,0.7,0))
    par(fig=c(0,0.35,0,1), new=FALSE)
    plot(rcp26.i, target.show=FALSE, map.show=FALSE, ylim=ylim, 
         new=FALSE, legend.show=FALSE, envcol="orange")
    text(1905, 9, "a) RCP2.6", cex=1.2, pos=4)
    par(fig=c(0.32,0.67,0,1), new=TRUE)
    plot(rcp45.i, target.show=FALSE, map.show=FALSE, ylim=ylim, 
         new=FALSE, legend.show=FALSE, envcol="orange")
    text(1905, 9, "b) RCP4.5", cex=1.2, pos=4)
    par(fig=c(0.64,1,0,1), new=TRUE)
    plot(rcp85.i, target.show=FALSE, map.show=FALSE, ylim=ylim, 
         new=FALSE, legend.show=FALSE, envcol="orange")
    text(1905, 9, "c) RCP8.5", cex=1.2, pos=4)
    dev.off()
  }
}

if(visualize) {
  ## Calculate projected climate change
  rcp.all <- c("rcp26", "rcp45", "rcp85")
  npc.all <- c("npc2", "npc3")
  stats <- list()
  yz <- subset(T2M, it=c(1981, 2010))
  for(rcp in rcp.all) {
    for(npc in npc.all) {
      eval(parse(text=paste0("dse <- dse.",rcp,".",npc)))
      for(st in names(dse)) {
        t2m.present <- apply(subset(dse[[st]], it=c(1981, 2010)), 2, "mean")
        t2m.near <- apply(subset(dse[[st]], it=c(2021, 2050)), 2, "mean")
        t2m.far <- apply(subset(dse[[st]], it=c(2071, 2100)), 2, "mean")
        dt.near <- sapply(c(q5,mean,q95), function(fn) fn(t2m.near-t2m.present))
        dt.far <- sapply(c(q5,mean,q95), function(fn) fn(t2m.far-t2m.present))
        eval(parse(text=paste0("stats$dt[['",st,"']]$near$",rcp,"$",npc," <- dt.near")))
        eval(parse(text=paste0("stats$dt[['",st,"']]$far$",rcp,"$",npc," <- dt.far")))
        sd.obs <- sd(subset(T2M, is=st, it=c(1981, 2010)))
        sd.dse <- sd(subset(dse[[st]], it=c(1981, 2010)))
        sd.rel <- sd.dse/sd.obs
        eval(parse(text=paste0("stats$sd[['",st,"']]$obs <- sd.obs")))
        eval(parse(text=paste0("stats$sd[['",st,"']]$dse$",npc,"$",rcp," <- sd.dse")))
        eval(parse(text=paste0("stats$sd[['",st,"']]$rel$",npc,"$",rcp," <- sd.rel")))
      }
    }
  }
  dt <- stats$dt
  sd <- stats$sd
  
  ## Figure 6
  ## Use diagnose for validation
  for(rcp in rcp.all) {
    for(npc in npc.all) {
      CairoTIFF(filename=file.path(path.fig,paste0("tmax_dsensemble_diagnose_",
                                                   rcp,"_",npc,".tiff")),
                width = 2700, height = 2100, pointsize = 12, bg = "white", res = 300)
      par(mar=c(3,3.1,0.8,8), mgp=c(4.5,0.7,0))
      eval(parse(text=paste0("diagnose(dse.",rcp,".",npc,
                             ", plot=TRUE, new=FALSE)")))
      dev.off()
    }
  }

  ## Get names of GCM simulations
  gcms <- list()
  for(rcp in c("rcp26","rcp45","rcp85")) {
    eval(parse(text=paste0("models <- attr(dse.",rcp,".npc2[[1]],", 
                           "'model_id')")))
    gcms.rcp <- list()
    for(i in seq(length(models))) {
      gcm.i <- substr(models[i], 1, regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]", models[i])-2)
      rip.i <- substr(models[i], regexpr("r[0-9]{1,2}i[0-9]{1,2}p[0-9]", models[i]), nchar(models[i]))
      gcms.rcp[[gcm.i]] <- c(gcms.rcp[[gcm.i]], rip.i)
    }
    gcms[[rcp]] <- gcms.rcp
  }
  
  ## Figure 8 is a versio of this with npc = 2
  breaks <- seq(0,2,0.2)
  pal <- "rd"
  cols <- colscal(n=length(breaks)-1, pal=pal)
  for(npc in npc.all) {
    CairoTIFF(filename=file.path(path.fig,paste0("tmax_dsensemble_projectedchange_maps_",npc,".tiff")), 
              width = 3500, height = 1800, pointsize = 12, bg = "white", res = 300)
    i <- 0
    for(future in c("far","near")) {
      i <- i+1
      j <- 0
      for(rcp in rcp.all) {
        j <- j+1
        abc <- switch((i-1)*3+j, "1"="e", "2"="f", "3"="g", "4"="a", "5"="b", "6"="c")
        fig <- c((j-1)*0.33, j*0.33, (i-1)*0.5, i*0.5)
        if(i==2 & j==1) sublab <- "projected tmax change" else sublab <- ""
        map(T2M, FUN="mean", cex=0.1, new=FALSE, fig=fig, add=!(i==1 & j==1),
            mar=c(1.5,2,2.5,2), mgp=c(2,0.5,0), 
            xlim=c(88,93), ylim=c(19.5,28), cex.axis = 0.9,
            main=paste0(abc,") ",future," future, ",rcp),
            sub=sublab,
            colbar=list(breaks=breaks, pal=pal, show=(i==2 & j==1)))
        for(st in names(dt)) {
          dt.st <- dt[[st]][[future]][[rcp]][[npc]][2]
          col.st <- cols[breaks[2:length(breaks)]>dt.st & breaks[1:(length(breaks)-1)]<=dt.st]
          col.st[dt.st>max(breaks)] <- cols[length(cols)]
          col.st[dt.st<min(breaks)] <- cols[1]
          points(stlon[stnm==nmcorr(st)], stlat[stnm==nmcorr(st)], col=col.st, 
                 cex=1.5, pch=19)
        }
      }
    }
    dev.off()
  }

  ## Figures for the Supplement
  ## Maps comparing the standard deviation of the downscaled ensemble
  ## with the standard deviation of the observations
  npc <- "npc2"
  breaks <- seq(0.5,1.5,0.1)
  pal <- "burd"
  cols <- colscal(n=length(breaks)-1, pal=pal)
  save.fig <- TRUE
  if(save.fig) {
    CairoTIFF(filename=file.path(path.fig,paste0("tmax_dsenesemble_sd_",npc,".tiff")), 
              width = 3500, height = 1400, pointsize = 12, bg = "white", res = 300)
  } else {
    X11(width = 16, height=7)
  }
  j <- 0
  for(rcp in rcp.all) {
    j <- j+1
    abc <- switch(j, "1"="a", "2"="b", "3"="c")
    fig <- c((j-1)*0.33, j*0.33, 0,1)
    if(j==1) sublab <- "sd(dse)/sd(obs)" else sublab <- ""
    map(T2M, FUN="mean", cex=0.1, new=FALSE, fig=fig, add=!(j==1),
        mar=c(4.3,2,2.5,2), mgp=c(0,0.5,0), 
        xlim=c(88,93), ylim=c(19.5,28), cex.axis = 0.9,
        main=paste0(abc,") ",rcp), 
        sub=sublab,
        colbar=list(breaks=breaks, pal=pal, show=(j==1)))#show=TRUE))
    for(st in names(dt)) {
      sd.st <- sd[[st]]$rel[[npc]][[rcp]]
      col.st <- cols[breaks[2:length(breaks)]>sd.st & breaks[1:(length(breaks)-1)]<=sd.st]
      col.st[sd.st>max(breaks)] <- cols[length(cols)]
      col.st[sd.st<min(breaks)] <- cols[1]
      points(stlon[stnm==nmcorr(st)], stlat[stnm==nmcorr(st)], col=col.st, 
             cex=1.5, pch=19)
    }
  }
  if(save.fig) dev.off()

  ## Produce tables for the paper
  ## Calculate mean pre-monsoon daily maximum temperature 
  t2m.season <- subset(subset(t2m.4s, it=c(1981,2019)), it=it.season)
  stt2m <- apply(t2m.season, 2, mean, na.rm=TRUE)
  stt2m.trend <- apply(t2m.season, 2, trend.coef, na.rm=TRUE)
  stt2m.trend.p <- apply(t2m.season, 2, trend.pval, na.rm=TRUE)
  
  ## Table 1
  n.digits <- 1
  for(npc in npc.all) {
    file.out <- file.path(path.table, paste0("rashid_table1_",npc,".csv"))
    cat(",,, RCP2.6,, RCP4.5,, RCP8.5,", file=file.out, sep="\n")
    cat("Region,Station name,WMO ID,Near,Far,Near,Far,Near,Far\n", 
        file=file.out, append=TRUE)
    for(st in names(dt)) {
      txt.out <- c(as.character(streg[stnm==nmcorr(st)]), 
                   nmcorr(st),
                   stid[stnm==nmcorr(st)], 
                   round(dt[[st]]$near$rcp26[[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far$rcp26[[npc]][[2]], digits=n.digits),
                   round(dt[[st]]$near$rcp45[[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far$rcp45[[npc]][[2]], digits=n.digits),
                   round(dt[[st]]$near$rcp85[[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far$rcp85[[npc]][[2]], digits=n.digits),"\n")
      cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
    }
    txt.out <- c("", "Average", "", 
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near$rcp26[[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far$rcp26[[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near$rcp45[[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far$rcp45[[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near$rcp85[[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far$rcp85[[npc]][[2]])),digits=n.digits)
                 )
    cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  }
 
  ## Table 2
  rcp <- "rcp26"
  for(npc in npc.all) {
    file.out <- file.path(path.table, paste0("rashid_table2_",npc,".csv"))
    cat(",,, Near Future,,, Far Future,,", file=file.out, sep="\n")
    cat("Region,Station name,WMO ID, 5th,50th,95th, 5th,50th,95th\n", 
        file=file.out, append=TRUE)
    for(st in names(dt)) {
      txt.out <- c(as.character(streg[stnm==nmcorr(st)]), 
                   nmcorr(st),
                   stid[stnm==nmcorr(st)], 
                   round(dt[[st]]$near[[rcp]][[npc]][[1]], digits=n.digits), 
                   round(dt[[st]]$near[[rcp]][[npc]][[2]], digits=n.digits),
                   round(dt[[st]]$near[[rcp]][[npc]][[3]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[1]], digits=n.digits),
                   round(dt[[st]]$far[[rcp]][[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[3]], digits=n.digits),"\n")
      cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
    }
    txt.out <- c("", "Average", "", 
                 round(mean(sapply(names(dt), function(i) dt[[i]]$near[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(i) dt[[i]]$near[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(i) dt[[i]]$near[[rcp]][[npc]][[3]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(i) dt[[i]]$far[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(i) dt[[i]]$far[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(i) dt[[i]]$far[[rcp]][[npc]][[3]])),digits=n.digits)
    )
    cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  }
  
  ## Table 3
  rcp <- "rcp45"
  for(npc in npc.all) {
    file.out <- file.path(path.table, paste0("rashid_table3_",npc,".csv"))
    cat(",,, Near Future,,, Far Future,,", file=file.out, sep="\n")
    cat("Region,Station name,WMO ID, 5th,50th,95th, 5th,50th,95th\n", 
        file=file.out, append=TRUE)
    for(st in names(dt)) {
      txt.out <- c(as.character(streg[stnm==nmcorr(st)]), 
                   nmcorr(st),
                   stid[stnm==nmcorr(st)], 
                   round(dt[[st]]$near[[rcp]][[npc]][[1]], digits=n.digits), 
                   round(dt[[st]]$near[[rcp]][[npc]][[2]], digits=n.digits),
                   round(dt[[st]]$near[[rcp]][[npc]][[3]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[1]], digits=n.digits),
                   round(dt[[st]]$far[[rcp]][[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[3]], digits=n.digits),"\n")
      cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
    }
    txt.out <- c("", "Average", "", 
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[3]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[3]])),digits=n.digits)
    )
    cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  }
  
  ## Table 4
  rcp <- "rcp85"
  for(npc in npc.all) {
    file.out <- file.path(path.table, paste0("rashid_table4_",npc,".csv"))
    cat(",,, Near Future,,, Far Future,,", file=file.out, sep="\n")
    cat("Region,Station name,WMO ID, 5th,50th,95th, 5th,50th,95th\n", 
        file=file.out, append=TRUE)
    for(st in names(dt)) {
      txt.out <- c(as.character(streg[stnm==nmcorr(st)]),
                   nmcorr(st),
                   stid[stnm==nmcorr(st)], 
                   round(dt[[st]]$near[[rcp]][[npc]][[1]], digits=n.digits), 
                   round(dt[[st]]$near[[rcp]][[npc]][[2]], digits=n.digits),
                   round(dt[[st]]$near[[rcp]][[npc]][[3]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[1]], digits=n.digits),
                   round(dt[[st]]$far[[rcp]][[npc]][[2]], digits=n.digits), 
                   round(dt[[st]]$far[[rcp]][[npc]][[3]], digits=n.digits),"\n")
      cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
    }
    txt.out <- c("", "Average", "", 
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$near[[rcp]][[npc]][[3]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[1]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[2]])),digits=n.digits),
                 round(mean(sapply(names(dt), function(st) dt[[st]]$far[[rcp]][[npc]][[3]])),digits=n.digits)
    )
    cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  }
  
  ## Table S1 from the Supplement
  file.out <- file.path(path.table, "rashid_tableS1.csv")
  cat("Region, Station name, WMO ID, Latitude (deg N), Longitude (deg E), Elevation (m), tmax (deg C), tmax trend (deg C/decade)", file=file.out, sep="\n")
  cat(", , , , , , Mar-May, Mar-May 1981-2019", file=file.out, sep="\n", append=TRUE)
  for(st in stnm) {
    sttrend <- round(stt2m.trend[tolower(sapply(names(stt2m.trend),nmcorr))==tolower(st)], digits=2)
    sttrendp <- stt2m.trend.p[tolower(sapply(names(stt2m.trend),nmcorr))==tolower(st)]
    sttrendlabel <- as.character(sttrend)
    if(sttrendp<0.01) sttrendlabel <- paste(sttrendlabel,"*")
    #if(sttrend>0) sttrendlabel <- paste0("+",sttrendlabel)
    txt.out <- c(as.character(streg[stnm==st]), st, stid[stnm==st], 
                 stlat[stnm==st], stlon[stnm==st], stalt[stnm==st],
                 round(stt2m[tolower(sapply(names(stt2m),nmcorr))==tolower(st)], digits=2),
                 sttrendlabel,#round(stt2m.trend[tolower(sapply(names(stt2m.trend),nmcorr))==tolower(st)], digits=2),
                 "\n")
    cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  }
  txt.out <- c("", "Average", "", "", "", "", 
               round(mean(stt2m),digits=n.digits),
               round(mean(stt2m.trend),digits=n.digits)
  )
  cat(paste(txt.out, collapse=","), file=file.out, append=TRUE)
  
  ## Table S2 from the Supplement
  nmax <- 3
  X <- matrix(" ", nrow=35, ncol=8)
  irow <- 1; icol <- 1
  for(rcp in names(gcms)) {
    if(irow>1) irow <- irow+1
    X[irow, icol] <- toupper(rcp)
    irow <- irow+1
    if(irow>nrow(X)) {irow <- 1; icol <- icol+2}
    for(gcm in names(gcms[[rcp]])) {
      rip <- gcms[[rcp]][[gcm]]
      rip <- split(rip, ceiling(seq_along(rip)/nmax))
      for(j in seq_along(rip)) {
        if(j==1 | irow==2) X[irow, icol] <- gcm
        X[irow, icol+1] <- paste(rip[[j]], collapse=" ")
        irow <- irow+1
        if(irow>nrow(X)) {irow <- 1; icol <- icol+2}
      }
    }
  }
  file.out <- file.path(path.table, paste0("rashid_tableS2.csv"))
  for(i in seq(nrow(X))) {
    cat(paste(X[i,],collapse=","), file=file.out, sep="\n", append=(i!=1))
  }
}

## Extra analysis
extra <- FALSE
if(extra) {
  dt2 <- list()
  for(neof in c(4,6,8,10)) {
    for(rcp in c("rcp26","rcp45","rcp85")) {
      filename.dse <- file.path(path.data,"dsensemble",
                                paste("dsensemble.tmax.",rcp,".",
                                      paste(it.season,collapse=""),
                                      ".neof",neof,".rda",sep="") )
      if(file.exists(filename.dse)) {
        load(filename.dse)
        eval(parse(text=paste0("dse.",rcp,".",neof," <- dse")))
        for(st in names(dse[[npc]])) {
          t2m.present <- apply(subset(dse[[npc]][[st]], it=c(1981, 2010)), 2, "mean")
          t2m.near <- apply(subset(dse[[npc]][[st]], it=c(2021, 2050)), 2, "mean")
          t2m.far <- apply(subset(dse[[npc]][[st]], it=c(2071, 2100)), 2, "mean")
          dt.near <- sapply(c(q5,mean,q95), function(fn) fn(t2m.near-t2m.present))
          dt.far <- sapply(c(q5,mean,q95), function(fn) fn(t2m.far-t2m.present))
          eval(parse(text=paste0("dt2[['",st,"']]$near$",rcp,"$",npc,"$neof",neof," <- dt.near")))
          eval(parse(text=paste0("dt2[['",st,"']]$far$",rcp,"$",npc,"$neof",neof," <- dt.far")))
        }
      }
    }
  }
  
  
  ## Question: Does the number of predictor patterns influence the projected temperature change?
  ## Answer: Yes, the numbers are different but there is still a warming and the spatial pattern is similar.
  for(st in names(dt2)) {
    dx <- sapply(names(dt2[[st]]$far$rcp26$npc2), function(x) dt2[[st]]$far$rcp26$npc2[[x]][[2]])
    print(paste("mean",round(mean(dx),2),"range",round(diff(range(dx)),2)))
  }
  
  ## Question: Is there a spatial pattern in the projected temperature change?
  ## Answer: Yes, a tendency for lower warmer toward the south-east.
  dtlon <- sapply(names(dt2), function(nm) stlon[nmcorr(nm)==stnm])
  dtlat <- sapply(names(dt2), function(nm) stlat[nmcorr(nm)==stnm])
  neof.all <- paste0("neof",c(4,6,8,10)) 
  npc <- "npc2"
  print.lon <- FALSE
  print.lat <- TRUE
  for(future in c("near","far")) {
    for(rcp in rcp.all) {
      for(neof in neof.all) {
        print(paste(rcp,npc,future,neof))
        dt50 <- sapply(names(dt), function(nm) dt2[[nm]][[future]][[rcp]][[npc]][[neof]][[2]])      
        if(print.lon) print(paste0("cor(lon, dt): ",round(cor(dtlon, dt50),2)))
        if(print.lat) print(paste0("cor(lat, dt): ",round(cor(dtlat, dt50),2)))
      }
    }
  }
  
  if(visualize) {
    pal <- "burd"
    sublab <- "projected tmax change"
    npc <- "npc2"
    future <- "far"
    for(rcp in rcp.all) {
      breaks <- pretty(range(sapply(names(dt), function(st) dt[[st]][[future]][[rcp]][[npc]][2]))+c(-0.1,0.1), 10)
      cols <- colscal(n=length(breaks)-1, pal=pal)
      map(T2M, FUN="mean", cex=0.1, new=TRUE, 
          mar=c(1.5,2,2.5,2), mgp=c(2,0.5,0), 
          xlim=c(88,93), ylim=c(19.5,28), cex.axis = 0.9,
          main=paste(future,"future,",rcp,npc),
          sub=sublab,
          colbar=list(breaks=breaks, pal=pal, show=TRUE))
      for(st in names(dt)) {
        dt.st <- dt[[st]][[future]][[rcp]][[npc]][2]
        col.st <- cols[breaks[2:length(breaks)]>dt.st & breaks[1:(length(breaks)-1)]<=dt.st]
        col.st[dt.st>max(breaks)] <- cols[length(cols)]
        col.st[dt.st<min(breaks)] <- cols[1]
        points(stlon[stnm==nmcorr(st)], stlat[stnm==nmcorr(st)], col=col.st, 
               cex=1.5, pch=19)
      }
    }
  }
  
  ## Question: Is there a justification for using a specific number of predictor patterns in dse? 
  ## For predictand patterns, we simply used the cross-validation to evaluate which PCs were well represented.
  ## Two ways to assess this: 1) diagnosis of the dsensemble, 2) inspection of the common EOF
  ## 1) diagnose dsensemble:
  ## 4 is too few - leads to underestimated interannual variability and trend
  diagnose(dse.rcp85.4$npc2)
  plot(dse.rcp85.4$npc2$Dhaka, new=TRUE)
  ## 6 is better but still underestimates interannual variability and trend
  diagnose(dse.rcp85.6$npc2)
  plot(dse.rcp85.6$npc2$Dhaka, new=TRUE)
  ## 8 & 10 are similar - better in center and western Bangla
  diagnose(dse.rcp85.8$npc2) # 8
  plot(dse.rcp85.8$npc2$Dhaka, new=TRUE) # 8
  diagnose(dse.rcp85.10$npc2) # 10
  plot(dse.rcp85.10$npc2$Dhaka, new=TRUE) # 10
  
  ## 2) common EOF analysis
  ## 8 seems like a reasonable choice. The explained variance of the first 8 EOFs is 99-100%.
  ## The patterns of the higher order EOFs describe fine spatial details and do not hold 
  ## information relevant to the large scale climate patterns. At that point they are more of a 
  ## statistical and mathematical tool than a representation of a physical phenomenon. 
  ## The choice of predictor patterns is still somewhat arbitrary and subjective. 
  rcp <- "rcp85"
  files.gcm <- list.files(file.path(path.cmip5,rcp), pattern="tasmax", full.names=TRUE)
  for(i in seq(1,length(files.gcm),1)) {
    T2M.cmip <- as.annual(subset(as.4seasons(subset(retrieve(files.gcm[[i]]), is=is.predictor)), it=it.season))
    ceof <- EOF(combine(T2M.predictor, T2M.cmip), n=20)
    print("Explained variance:")
    print(100*cumsum(attr(ceof,"eigenvalues")^2)/sum(attr(ceof,"eigenvalues")^2), digit=0)
    for(np in seq(7,14)) {
      ds.ceof <- DS.pca(pca.predictand, subset(ceof, ip=1:np, verbose=FALSE, plot=FALSE), ip=1:np)
      ds.eval <- attr(ds.ceof,"evaluation")
      r1 <- round(cor(ds.eval[,1], ds.eval[,2]),2)
      r2 <- round(cor(ds.eval[,3], ds.eval[,4]),2)
      r3 <- round(cor(ds.eval[,5], ds.eval[,6]),2)
      print(paste(np,"corr:",r1,r2,r3))
    }
  }
  
  ## Try to figure out why the past trend is not reproduced in the east and south-east:
  ## Predictand PCA analysis shows that 30-60% of the trend in these stations
  ## is represented by PC2. The problem is likely that the downscaling model for PC2 
  ## is not as skillful as the one for PC1 - it only has cross-val corr of 0.46 
  
  ## Now try a spatial subset and see if we can get a better representation of these stations
  is.v2 <- list(lon=c(90,93))
  T2M.v2 <- subset(T2M, is=is.v2)
  ## Prepare predictand
  pca.predictand.v2 <- PCA(T2M.v2, n=10)
  print("Explained variance:")
  print(100*cumsum(attr(pca.predictand.v2,"eigenvalues")^2)/sum(attr(pca.predictand.v2,"eigenvalues")^2), digit=0)
  print(100*attr(pca.predictand.v2,"eigenvalues")^2/sum(attr(pca.predictand.v2,"eigenvalues")^2), digit=0)
  ## Only 4 PCs explain all variance after pcafill.
  pca.predictand.v2 <- PCA(T2M.v2, n=4)
  
  ## With this spatial selection (east of 90E), there are fewer stations where much of trend
  ## is contained in PC2 but now Dhaka is one of them!
  if(visualize) {
    cb <- list(pal="t2m", breaks=seq(-0.8,0.8,0.1))
    map(T2M.v2, FUN="trend", colbar=cb, new=TRUE,
        main=paste("tmax trend (deg C/decade)\n",
                   "pre-monsoon season, ",paste(range(index(T2M.v2)),collapse="-")))
    i.significant <- which(apply(T2M.v2, 2, function(x) trend.pval(x)<0.1))
    T2M.v2 <- apply(T2M.v2[,i.significant], 2, trend.coef)
    for(n.pc in seq(1,4)) {
      print(paste0("nPC = ",n.pc))
      T2M.test <- as.station(PCA(T2M.v2), ip=1:n.pc)#as.station(PCA(T2M, n=n.pc))
      map(T2M.test, FUN="trend", colbar=cb, new=TRUE,
          main=paste("tmax trend (deg C/decade)",
                     "\npre-monsoon season, ",paste(range(index(T2M.v2)),collapse="-"),
                     "\nbased on",n.pc,"principal components"))
      err <- abs(T2M.trends - apply(T2M.test[,i.significant], 2, trend.coef))
      err.rel <- err/abs(T2M.trends)
      print(round(cbind(T2M.trends, err, err.rel),2))
    }  
  }
  
  ####### Testing DS PCA approach ######
  if(visualize) {
    i.X <- list(lon=c(88,93), lat=c(20,26))#which(apply(T2M, 2, function(x) trend.pval(x)<0.05))
    X <- subset(T2M,is=i.X)
    X.trend <- apply(X, 2, trend.coef)
    i.Y <- list(lon=c(85,95), lat=c(15,30)) 
    Y <- subset(T2M.predictor, is=i.Y)
    nX <- 4; nY <- 10
    eof.Y <- EOF(Y, n=nY)
    pca.X <- PCA(X, n=nX)
    ds.pca <- DS.pca(pca.X, eof.Y, verbose=FALSE, plot=FALSE)
    for(pc in seq(ncol(ds.pca))) {
      X11(width=9, height=9)
      par(mar=c(2,2,2,2))
      plot(ds.pca, ip=pc, new=FALSE)
    }
    for(n.pc in seq(1,3)) {
      print(paste0("nPC = ",n.pc))
      X.ds <- as.station(ds.pca, ip=1:n.pc)
      map(X.ds, FUN="trend", colbar=cb, new=TRUE,
          main=paste("tmax trend (deg C/decade)",
                     "\npre-monsoon season, ",paste(range(index(X)),collapse="-"),
                     "\nbased on",n.pc,"principal components"))
      err <- abs(X.trend - apply(X.ds, 2, trend.coef))
      err.rel <- err/abs(X.trend)
      print(round(cbind(X.trend, err, err.rel),2))
    }

    ## Try downscaling individual stations
    i.Y <- NULL#list(lon=c(85,95), lat=c(15,30)) 
    Y <- subset(T2M.predictor, is=i.Y)
    nY <- 10
    eof.Y <- EOF(Y, n=nY)
    for(i in i.significant) {
      X <- subset(T2M,is=i)
      X.trend <- trend.coef(X)
      ds.X <- DS(X, eof.Y, verbose=FALSE, plot=FALSE)
      X11(width=9, height=9)
      par(mar=c(2,2,2,2))
      plot(ds.X, new=FALSE)
      err <- abs(X.trend - trend.coef(ds.X))
      err.rel <- err/abs(X.trend)
      print(round(cbind(X.trend, err, err.rel),2))
    }
  }

  ## Why are there multiple files marked EC.EARTH r1i1p1 for the RCP4.5 scenario?
  ## (also two GISS.E2.H r1i1p1 for the same pathway) 
  files.rcp45 <- list.files(file.path(path.cmip5,"rcp45"), full.names=TRUE)
  for(i in seq(1,length(files.rcp45))) {
    x <- retrieve(files.rcp45[[i]], lon=is.predictor$lon, lat=is.predictor$lat)
    if(attr(x, "model_id") %in% c("EC-EARTH","GISS-E2-R")) {
      print(attr(x, "model_id"))
      print(attr(x, "parent_experiment_rip"))
      print(paste0("r",attr(x, "realization"),
                  "i",attr(x, "initialization_method"),
                  "p",attr(x, "physics_version")))
    }
  }
}
