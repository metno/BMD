require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/dognnormaler.R")
require(readxl)
setwd("~/RScript/BMD/Fremtid")

BMD.Proj.Read.Rain <- function(Scen = "Ref"){
  if(Scen=="Ref"){Data <- read.csv("Dhaka/NEXGDDP_dhaka_pr_historical_fullensemble.csv",sep=";")}
  if(Scen=="S126"){Data <- read.csv("Dhaka/NEXGDDP_dhaka_pr_SSP126_fullensemble.csv",sep=";")}
  if(Scen=="S245"){Data <- read.csv("Dhaka/NEXGDDP_dhaka_pr_SSP245_fullensemble.csv",sep=";")}
  if(Scen=="S370"){Data <- read.csv("Dhaka/NEXGDDP_dhaka_pr_SSP370_fullensemble.csv",sep=";")}
  if(Scen=="S585"){Data <- read.csv("Dhaka/NEXGDDP_dhaka_pr_SSP585_fullensemble.csv",sep=";")}
  if(Scen=="Grid"){Data <- read.csv("Dhaka/ENACTS_rr_dhaka_daily.csv",sep=";")}
  #Data <- Data*86400
  Data<- round(Data,1)
  Datoer <- row.names(Data)
  Year <- as.numeric(substring(Datoer,1,4))
  Mnt <- as.numeric(substring(Datoer,5,6))
  Day <- as.numeric(substring(Datoer,7,8))
  Data <- cbind(Year,Mnt,Day,Data)
  Data
}

BMD.Obs.read <- function(Element = "Rainfall"){
  FileName <- paste("Data_2020/",Element,"_corrected.xlsx",sep="")
  if (Element == "Rainfall"){FileName <- "Data_2020/Rainfall.xlsx"}
  if (Element == "Sunshine"){FileName <- "Data_2020/Sunshine_hour.xlsx"}
  if (Element == "Cloud"){FileName <- "C:/Users/hansoh/Documents/Rscript/BMD/CloudReform.xlsx"}
  Data <- read_xlsx(FileName,na=c("****","-999","-1"))
  if (Element == "Cloud"){Data <- cbind(unlist(Data[,1]),unlist(Data[,2]),unlist(Data[,3]),unlist(Data[,4]),unlist(Data[,5]),unlist(Data[,6]),unlist(Data[,7]),unlist(Data[,8]),unlist(Data[,9]),unlist(Data[,10]),unlist(Data[,11]),unlist(Data[,12]));
  colnames(Data)<-c("St","Year","Month","Day","U0","U3","U6","U9","U12","U15","U18","U21");
  Data[Data[,1]==11047,1] <- 11407}
  if (Element != "Cloud"){Data <- cbind(unlist(Data[,1]),unlist(Data[,2]),unlist(Data[,3]),unlist(Data[,4]),unlist(Data[,5]))}
  Data
}

BMD.Rain.Proj.Counter.Control  <- function(FYear=1985,TYear=2014,FYearP=2071,TYearP=2100,Element="Rainfall",Station=11111,RainT=23,MNDBar=T){
  ObsData <- BMD.Obs.read(Element=Element)
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
  RefData <- BMD.Proj.Read.Rain(Scen = "Ref")
  RefData <- RefData[RefData[,1]>=FYear & RefData[,1]<=TYear,]
  S126Data <- BMD.Proj.Read.Rain(Scen = "S126")
  S126Data <- S126Data[S126Data[,1]>=FYearP & S126Data[,1]<=TYearP,]
  S245Data <- BMD.Proj.Read.Rain(Scen = "S245")
  S245Data <- S245Data[S245Data[,1]>=FYearP & S245Data[,1]<=TYearP,]
  S370Data <- BMD.Proj.Read.Rain(Scen = "S370")
  S370Data <- S370Data[S370Data[,1]>=FYearP & S370Data[,1]<=TYearP,]
  S585Data <- BMD.Proj.Read.Rain(Scen = "S585")
  S585Data <- S585Data[S585Data[,1]>=FYearP & S585Data[,1]<=TYearP,]
  
  #print(ls())
  mDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ObsResult <- c()
  for(m in 1:12){
    ObsResult <- c(ObsResult,
                length(ObsData[ObsData[,3]==m & ObsData[,5]>=RainT & !is.na(ObsData[,5]),5])/mDays[m])
  }
  
  RefResult <- BMD.Rain.Proj.Counter(RefData,RainT=RainT)
  S126Result <- BMD.Rain.Proj.Counter(S126Data,RainT=RainT)
  S245Result <- BMD.Rain.Proj.Counter(S245Data,RainT=RainT)
  S370Result <- BMD.Rain.Proj.Counter(S370Data,RainT=RainT)
  S585Result <- BMD.Rain.Proj.Counter(S585Data,RainT=RainT)
  
  if(MNDBar==F){
    plot(1:12,ObsResult,type="l",ylim=c(0,max(c(ObsResult,S585Result))),ylab="Days",xlab="",main=paste("Dhaka, Threshold = ", RainT, " mm",sep=""),lty=3)
    lines(1:12,RefResult,col="black")
    lines(1:12,S126Result,col="#0034AB")
    lines(1:12,S245Result,col="#F79420")
    lines(1:12,S370Result,col="#E71D25")
    lines(1:12,S585Result,col="darkred")
  }
  if(MNDBar==T){
    plot(-1000,-1000,ylim=c(1.4,6.6),xlim=c(1,13),ylab="",xlab="",main = "Moderately heavy to very heavy rainfall in Dhaka", xaxt = "n", yaxt = "n")
    axis(1, at = c(1:12+0.5),srt=45,
         labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#    axis(2, at = c(1:6),srt=45,
#         labels = c("Obs","Ref","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))
    axis(2, at=c(2:6), labels = FALSE)
#    text(y = c(1:6), par("usr")[1], labels = c("Observations","Rreference","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"), srt = 45, pos = 2, xpd = TRUE)
    text(y = c(2:6), par("usr")[1], labels = c("Rreference","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"), srt = 45, pos = 2, xpd = TRUE)
    
#    MNDBarPlotter(ObsResult,r=1,col="black")
    MNDBarPlotter(RefResult,r=2,col="black")
    MNDBarPlotter(S126Result,r=3,col="#0034AB")
    MNDBarPlotter(S245Result,r=4,col="#F79420")
    MNDBarPlotter(S370Result,r=5,col="#E71D25")
    MNDBarPlotter(S585Result,r=6,col="darkred")
    
  }
  
}

MNDBarPlotter <- function (Data,r=1,col="black"){
  BGCol <- c("seashell","seashell","wheat","wheat","wheat","lightblue","lightblue","lightblue","lightblue","lightgreen","lightgreen","seashell")
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(r-0.4,r+0.4,r+0.4,r-0.4,r-0.4),col=BGCol[n],border="NA")
    Ant <- Data[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(r-0.2,r+0.2,r+0.2,r-0.2,r-0.2),col=col,border="NA")
    }
  }
}

BMD.Rain.Proj.Counter <- function(Data,RainT=50){
  DimData <- dim(Data)
  
  mDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  Result <- c()
  for (Mod in 4:DimData[2]){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  length(Data[Data[,2]==m & Data[,Mod]>=RainT & !is.na(Data[,Mod]),5])/mDays[m])
    }
    Result <- rbind(Result,ModRes)
  }
  
  R2 <- colMeans(Result[,2:13])
  R2
}