library(esd)
require(devtools)

## Source the file with useful functions directly from github
source_url("https://raw.githubusercontent.com/metno/BMD/master/R/functions_heatwaves.R")

## Change path to your file structure
path.data <- "/path/to/data"
filename.tmax <- file.path(path.data,"tmax.rda")

## Import temperature data:
filename.tmax <- file.path(path.data,"tmax.rda")
if(file.exists(filename.tmax)) {
  load(filename.tmax)
} else {
  filename.data <- file.path(path, "Max_2020.csv")
  filename.meta <- file.path(path, "Stations_local_id.csv")
  X <- read.table(filename.data, header=TRUE, sep=",")
  meta <- read.table(filename.meta, header=TRUE, sep=",")
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
  ## You can save any object in R with the function 'save' as an '.rda' file
  attr(tmax,"variable") <- "tmax"
  attr(tmax, "longname") <- "Maximum air temperature"
  save(file=filename.tmax, tmax, meta)
}

## There were some suspicious values with low temperatures - remove those
z <- coredata(tmax)
z[z < 5] <- NA
z -> coredata(tmax)

it <- c(1981, 2020)
tmax <- subset(tmax, it=it)

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
heatwaves <- monthly.hot(tmax, threshold=threshold)

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

