
library(esd)

path.data <- "/path/to/data"

## Import temperature data:
filename.tmax <- file.path(path.data,"tmax.rda")
if(file.exists(filename.tmax)) {
  load(filename.tmax)
} else {
  X <- read.table(file.path(path,"Max_2020.csv"),header=TRUE,sep=",")
  meta <- read.table(file.path(path,"Stations_local_id.csv"),
                     header=TRUE,sep=",")
  for (id in 1:nrow(meta)) {
    lid <- meta$Local_ID[id]
    Y <- X[X$INDEX==lid,]
    Y[Y <= -999] <- NA
    t <- paste(Y[,2],Y[,3],Y[,4],sep='-')
    z <- zoo(Y[,5],order.by=as.Date(t))
    y <- as.station(z,loc=as.character(meta$Station[id]),lon=meta$Longitude.E.[id],
                    lat=meta$Latitude.N.[id],alt=meta$Elavation.m.[id],
                    cntr='Bangladesh',
                    param='t2m',unit='degC',src='BMD',stid=lid) 
    if (id == 1) t2m <- y else t2m <- combine.stations(t2m,y)
  }
  ## You can save any object in R with the function 'save' as a '.rda' file
  ## but you can only save and use rda files in R
  attr(t2m,"variable") <- "tmax"
  attr(t2m, "longname") <- "Maximum air temperature"
  save(file=filename.tmax, t2m, meta)
}

## There were some suspicious values with low temperatures - remove those
z <- coredata(t2m)
z[z < 5] <- NA
z -> coredata(t2m)

it <- c(1981, 2020)
t2m <- subset(t2m, it=it)

monthly.hot <- function(X, threshold) {
  n.hot <- as.monthly(X, FUN='count', threshold=threshold)
  nv <- as.monthly(X, FUN='nv')
  f.hot <- n.hot
  z <- coredata(n.hot)/coredata(nv)
  coredata(f.hot) <- z
  attr(f.hot, 'variable') <- 'f.hot'
  attr(f.hot, 'longname') <- paste('fraction of days with',
                                   attr(X,'param'), 'above', threshold)
  return(f.hot)
}

threshold <- 36
heatwaves <- monthly.hot(t2m, threshold=threshold)

for(it in c('March','April','May','June','July','August','September','October')) {
  map(subset(heatwaves, it=it), FUN='mean', new=TRUE, 
      main=paste('Frequency of hot days in',it,'(>',threshold,'C)'),
      colbar=list(breaks=seq(0,0.2,0.01)))
}

for(it in c('March','April','May','June','July','August','September','October')) {
  map(subset(heatwaves, it=it), FUN='trend', new=TRUE,
      main=paste('Trend in frequency of hot days in',it,'(>',threshold,'C/decade)'),
      colbar=list(breaks=seq(-0.1,0.1,0.02)))
}

#plot(subset(heatwaves, it=it))

