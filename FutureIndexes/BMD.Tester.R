require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/dognnormaler.R")
require(readxl)
setwd("~/RScript/BMD/Fremtid")
source("~/RScript/BMD/Fremtid/BMD.Count.Rain.R")

BMD.HistCompRain <- function(FYear=1985,TYear=2014,Station=11111){
  ObsData <- BMD.Obs.read(Element="Rainfall")
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
  RefData <- BMD.Proj.Read.Rain(Scen = "Ref")
  RefData <- RefData[RefData[,1]>=FYear & RefData[,1]<=TYear,]
  GridData <- BMD.Proj.Read.Rain(Scen = "Grid")
  GridData <- GridData[GridData[,1]>=FYear & GridData[,1]<=TYear,]
  
  plot(RefData[,5],GridData[,4],pch=16)
}