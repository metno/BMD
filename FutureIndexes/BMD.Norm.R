require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/dognnormaler.R")
require(readxl)
setwd("~/RScript/BMD/Fremtid")

BMD.Norm.Month.Comp <- function(FYear=1985,TYear=2014){
  PData <- BMD.Proj.Month.Norm.Single(FYear=FYear,TYear=TYear)
  OData <- BMD.Obs.Normal(FYear=FYear,TYear=TYear)
  points(c(1:12),OData,pch=16)
}

BMD.Proj.Day.Norm.Single <- function(FYear=1985,TYear=2014){
  Data <- BMD.Proj.Month.Norm.Single(FYear=FYear,TYear=TYear)
  Result <- c()
  for(N in 1:32){
    Result <- cbind(Result,Lag.Dogn.Normal(Data[N,]))
  }
  
  plot(Result[,1],pch=16,ylim=c(25,35),col="gray",type="l")
  for(n in 1:32){lines(c(1:365),Result[,n],pch=16,col="gray")}
  lines(c(1:365),rowMeans(Result))
  
  Result
}

BMD.Proj.Month.Norm.Single <- function(FYear=1985,TYear=2014){
  Data <- BMD.Proj.Read()
  Data <- Data[Data[,1]>=FYear & Data[,1]<=TYear,]
  Result <- c()
  for (M in 1:12){
    Result <- cbind(Result,
                colMeans(Data[Data[,2]==M,4:35],na.rm=T))
    
  }
#  print(dim(Result))
  plot(Result[1,],pch=16,ylim=c(25,35),col="gray")
  for(n in 1:32){points(c(1:12),Result[n,],pch=16,col="gray")}
  lines(c(1:12),colMeans(Result))
  Result
}

BMD.Proj.Month.Norm.Ensemble <- function(FYear=1985,TYear=2014){
  Data <- BMD.Proj.Read()
  Data <- Data[Data[,1]>=FYear & Data[,1]<=TYear,]
  Result <- c()
  for (M in 1:12){
    Result <- c(Result,
                mean(colMeans(Data[Data[,2]==M,4:35],na.rm=T)))
    
  }
  plot(Result)
}

BMD.Proj.Read <- function(Scen = "Ref"){
  if(Scen=="Ref"){Data <- read.csv("NEXGDDP_Tasmax_dhaka_fullensemble.csv",sep=";")}
  if(Scen=="S126"){Data <- read.csv("NEXGDDP_dhaka_tasmax_SSP126_fullensemble.csv",sep=";")}
  if(Scen=="S245"){Data <- read.csv("NEXGDDP_dhaka_tasmax_SSP245_fullensemble.csv",sep=";")}
  if(Scen=="S370"){Data <- read.csv("NEXGDDP_dhaka_tasmax_SSP370_fullensemble.csv",sep=";")}
  if(Scen=="S585"){Data <- read.csv("NEXGDDP_dhaka_tasmax_SSP585_fullensemble.csv",sep=";")}
  Datoer <- row.names(Data)
  Year <- as.numeric(substring(Datoer,1,4))
  Mnt <- as.numeric(substring(Datoer,5,6))
  Day <- as.numeric(substring(Datoer,7,8))
  Data <- cbind(Year,Mnt,Day,Data)
  Data
}

BMD.Obs.read <- function(Element = "MaxT"){
  FileName <- paste("../Data_2020/",Element,"_corrected.xlsx",sep="")
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

BMD.Obs.Normal <- function(Element="MaxT",Station=11111,FYear = 1985, TYear = 2014){
  Data <- BMD.Obs.read(Element=Element)
  Data <- cbind(Data[Data[,1]==Station,2],Data[Data[,1]==Station,3],Data[Data[,1]==Station,5])
  Y <- unique(Data[,1])
  Resultat <- c()
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(year in min(Y):max(Y)){
    R2 <- year
    #print(year)
    for(month in 1:12){
      D2 <- Data[Data[,1]==year & Data[,2]==month,3]
      LD2 <- length(D2[!is.na(D2)])
      #print(LD2)
      if(LD2 >= (MDays[month]-5)){
        if (Element!="Rainfall"){
          R2 <- c(R2,mean(D2,na.rm = T))}        
        if (Element=="Rainfall"){
          R2 <- c(R2,sum(D2,na.rm = T))}
      }
      if(LD2 < (MDays[month]-5)){
        R2 <- c(R2,NA)
      }
    }
    Resultat <- rbind(Resultat, R2)
    #print(Resultat)
  }
  Resultat <- colMeans(Resultat[Resultat[,1]>=FYear & Resultat[,1]<=TYear,2:13])
  Resultat
}


BMD.HeatWave.Counter <- function(FYear=1985,TYear=2014,Element="MaxT",Station=11111){
  RefData <- BMD.Proj.Read()
  ObsData <- BMD.Obs.read(Element=Element)
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
  Result <- Station
  for(m in 1:12){
    Result <- c(Result,
                round(length(ObsData[ObsData[,3]==m & ObsData[,5]>=36,5])/30,1))
  }
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(RefData[RefData[,2]==m & RefData[,Mod]>=36 & !is.na(RefData[,Mod]),5])/30,1))
    }
    Result <- rbind(Result,ModRes)
  }
  Result
}

BMD.HeatWave.PWarming.Counter1 <- function(FYear=1985,TYear=2014,Element="MaxT",Station=11111, PWarming=2){
  RefData <- BMD.Proj.Read()
  ObsData <- BMD.Obs.read(Element=Element)
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
  PWarmData <- cbind(RefData[,1:3],RefData[,4:35]+PWarming)
  Result <- Station
  for(m in 1:12){
    Result <- c(Result,
                round(length(ObsData[ObsData[,3]==m & ObsData[,5]>=36,5])/30,1))
  }
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(RefData[RefData[,2]==m & RefData[,Mod]>=36 & !is.na(RefData[,Mod]),5])/30,1))
    }
    Result <- rbind(Result,ModRes)
  }

  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(PWarmData[PWarmData[,2]==m & PWarmData[,Mod]>=36 & !is.na(PWarmData[,Mod]),5])/30,1))
    }
    Result <- rbind(Result,ModRes)
  }
  
  Result
}


#This is the one making the plot used in the report
BMD.HeatWave.Proj.Counter <- function(FYear=1985,TYear=2014,FYearP=2071,TYearP=2100,Element="MaxT",Station=11111){
  RefData <- BMD.Proj.Read()
  ObsData <- BMD.Obs.read(Element=Element)
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
#  PWarm1Data <- cbind(RefData[,1:3],RefData[,4:35]+1)
#  PWarm2Data <- cbind(RefData[,1:3],RefData[,4:35]+2)
#  PWarm3Data <- cbind(RefData[,1:3],RefData[,4:35]+3)
#  PWarm4Data <- cbind(RefData[,1:3],RefData[,4:35]+4)
  
  S126Data <- BMD.Proj.Read(Scen = "S126")
  S126Data <- S126Data[S126Data[,1]>=FYearP & S126Data[,1]<=TYearP,]
  S245Data <- BMD.Proj.Read(Scen = "S245")
  S245Data <- S245Data[S245Data[,1]>=FYearP & S245Data[,1]<=TYearP,]
  S370Data <- BMD.Proj.Read(Scen = "S370")
  S370Data <- S370Data[S370Data[,1]>=FYearP & S370Data[,1]<=TYearP,]
  S585Data <- BMD.Proj.Read(Scen = "S585")
  S585Data <- S585Data[S585Data[,1]>=FYearP & S585Data[,1]<=TYearP,]
  
  Result <- Station
  
  mDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  BGCol <- c("seashell","seashell","wheat","wheat","wheat","lightblue","lightblue","lightblue","lightblue","lightgreen","lightgreen","seashell")
  
  for(m in 1:12){
    Result <- c(Result,
                round(length(ObsData[ObsData[,3]==m & ObsData[,5]>=36,5])/mDays[m],1))
  }
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(RefData[RefData[,2]==m & RefData[,Mod]>=36 & !is.na(RefData[,Mod]),5])/mDays[m],1))
    }
    Result <- rbind(Result,ModRes)
  }
  
#  PWarm1Result <- c()
#  for (Mod in 4:35){
#    ModRes <- Mod
#    for(m in 1:12){
#      ModRes <- c(ModRes,
#                  round(length(PWarm1Data[PWarm1Data[,2]==m & PWarm1Data[,Mod]>=36 & !is.na(PWarm1Data[,Mod]),5])/mDays[m],1))
#    }
#    PWarm1Result <- rbind(PWarm1Result,ModRes)
#  }
#  
#  PWarm2Result <- c()
#  for (Mod in 4:35){
#    ModRes <- Mod
#    for(m in 1:12){
#      ModRes <- c(ModRes,
#                  round(length(PWarm2Data[PWarm2Data[,2]==m & PWarm2Data[,Mod]>=36 & !is.na(PWarm2Data[,Mod]),5])/mDays[m],1))
#    }
#    PWarm2Result <- rbind(PWarm2Result,ModRes)
#  }
  
#  PWarm3Result <- c()
#  for (Mod in 4:35){
#    ModRes <- Mod
#    for(m in 1:12){
#      ModRes <- c(ModRes,
#                  round(length(PWarm3Data[PWarm3Data[,2]==m & PWarm3Data[,Mod]>=36 & !is.na(PWarm3Data[,Mod]),5])/mDays[m],1))
#    }
#    PWarm3Result <- rbind(PWarm3Result,ModRes)
#  }
  
#  PWarm4Result <- c()
#  for (Mod in 4:35){
#    ModRes <- Mod
#    for(m in 1:12){
#      ModRes <- c(ModRes,
#                  round(length(PWarm4Data[PWarm4Data[,2]==m & PWarm4Data[,Mod]>=36 & !is.na(PWarm4Data[,Mod]),5])/mDays[m],1))
#    }
#    PWarm4Result <- rbind(PWarm4Result,ModRes)
#  }
  
  S126Result <- c()
  for (Mod in 4:28){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(S126Data[S126Data[,2]==m & S126Data[,Mod]>=36 & !is.na(S126Data[,Mod]),5])/mDays[m],1))
    }
    S126Result <- rbind(S126Result,ModRes)
  }
  
  S245Result <- c()
  for (Mod in 4:28){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(S245Data[S245Data[,2]==m & S245Data[,Mod]>=36 & !is.na(S245Data[,Mod]),5])/mDays[m],1))
    }
    S245Result <- rbind(S245Result,ModRes)
  }
  
  S370Result <- c()
  for (Mod in 4:28){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(S370Data[S370Data[,2]==m & S370Data[,Mod]>=36 & !is.na(S370Data[,Mod]),5])/mDays[m],1))
    }
    S370Result <- rbind(S370Result,ModRes)
  }
  
  S585Result <- c()
  for (Mod in 4:28){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(S585Data[S585Data[,2]==m & S585Data[,Mod]>=36 & !is.na(S585Data[,Mod]),5])/mDays[m],1))
    }
    S585Result <- rbind(S585Result,ModRes)
  }
  
  
  
#  plot(-1000,-1000,ylab="",xlab = "",xlim=c(1,13),ylim=c(0.5,6.5),main = "Heat waves in Dhaka", xaxt = "n", yaxt = "n")
  plot(-1000,-1000,ylab="",xlab = "",xlim=c(1,13),ylim=c(1.5,6.5),main = "Heat waves in Dhaka", xaxt = "n", yaxt = "n")
  axis(1, at = c(1:12+0.5),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#  axis(2, at = c(1:6),
#       labels = c("Obs","Ref","+1","+2","+3","+4"))
#       labels = c("Obs","Ref","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))
  axis(2, at=c(1:6), labels = FALSE)
#  text(y = c(1:6), par("usr")[1], labels = c("Observations","Rreference","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"), srt = 45, pos = 2, xpd = TRUE)
  text(y = c(2:6), par("usr")[1], labels = c("Rreference","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"), srt = 45, pos = 2, xpd = TRUE)
  
#  for(n in 1:12){ #This is plotting the observations
#    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(0.6,1.4,1.4,0.6,0.6),col=BGCol[n],border="NA")
#   Ant <- Result[1,n+1]
#    if(Ant>0){Just <- Ant/30;
#    polygon(x=c(n,n,n+Just,n+Just,n),y=c(0.8,1.2,1.2,0.8,0.8),col="black",border="NA")
#    }
#  }
  
  PData <- colMeans(Result[2:33,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(1.6,2.4,2.4,1.6,1.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(1.8,2.2,2.2,1.8,1.8),col="black",border="NA")
    }
  }
  
#  PData <- colMeans(PWarm1Result[,2:13])
  PData <- colMeans(S126Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(2.6,3.4,3.4,2.6,2.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(2.8,3.2,3.2,2.8,2.8),col="#0034AB",border="NA")
    }
  }
  
#  PData <- colMeans(PWarm2Result[,2:13])
  PData <- colMeans(S245Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(3.6,4.4,4.4,3.6,3.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(3.8,4.2,4.2,3.8,3.8),col="#F79420",border="NA")
    }
  }
  
#  PData <- colMeans(PWarm3Result[,2:13])
  PData <- colMeans(S370Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(4.6,5.4,5.4,4.6,4.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(4.8,5.2,5.2,4.8,4.8),col="#E71D25",border="NA")
    }
  }
  
#  PData <- colMeans(PWarm4Result[,2:13])
  PData <- colMeans(S585Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(5.6,6.4,6.4,5.6,5.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(5.8,6.2,6.2,5.8,5.8),col="darkred",border="NA")
    }
  }
  #Result
}

BMD.HeatWave.PWarming.Counter <- function(FYear=1985,TYear=2014,Element="MaxT",Station=11111){
  RefData <- BMD.Proj.Read()
  ObsData <- BMD.Obs.read(Element=Element)
  ObsData <- ObsData[ObsData[,1]==Station & ObsData[,2]>=FYear & ObsData[,2]<=TYear,]
  PWarm1Data <- cbind(RefData[,1:3],RefData[,4:35]+1)
  PWarm2Data <- cbind(RefData[,1:3],RefData[,4:35]+2)
  PWarm3Data <- cbind(RefData[,1:3],RefData[,4:35]+3)
  PWarm4Data <- cbind(RefData[,1:3],RefData[,4:35]+4)
  
  Result <- Station
  
  mDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  BGCol <- c("seashell","seashell","wheat","wheat","wheat","lightblue","lightblue","lightblue","lightblue","lightgreen","lightgreen","seashell")
  
  for(m in 1:12){
    Result <- c(Result,
                round(length(ObsData[ObsData[,3]==m & ObsData[,5]>=36,5])/mDays[m],1))
  }
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(RefData[RefData[,2]==m & RefData[,Mod]>=36 & !is.na(RefData[,Mod]),5])/mDays[m],1))
    }
    Result <- rbind(Result,ModRes)
  }
  
  PWarm1Result <- c()
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(PWarm1Data[PWarm1Data[,2]==m & PWarm1Data[,Mod]>=36 & !is.na(PWarm1Data[,Mod]),5])/mDays[m],1))
    }
    PWarm1Result <- rbind(PWarm1Result,ModRes)
  }
  
  PWarm2Result <- c()
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(PWarm2Data[PWarm2Data[,2]==m & PWarm2Data[,Mod]>=36 & !is.na(PWarm2Data[,Mod]),5])/mDays[m],1))
    }
    PWarm2Result <- rbind(PWarm2Result,ModRes)
  }
  
  
  PWarm3Result <- c()
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(PWarm3Data[PWarm3Data[,2]==m & PWarm3Data[,Mod]>=36 & !is.na(PWarm3Data[,Mod]),5])/mDays[m],1))
    }
    PWarm3Result <- rbind(PWarm3Result,ModRes)
  }
  
  PWarm4Result <- c()
  for (Mod in 4:35){
    ModRes <- Mod
    for(m in 1:12){
      ModRes <- c(ModRes,
                  round(length(PWarm4Data[PWarm3Data[,2]==m & PWarm4Data[,Mod]>=36 & !is.na(PWarm4Data[,Mod]),5])/mDays[m],1))
    }
    PWarm4Result <- rbind(PWarm4Result,ModRes)
  }
  
  plot(-1000,-1000,ylab="",xlab = "",xlim=c(1,13),ylim=c(0.5,6.5),main = "Heat waves in Dhaka", xaxt = "n", yaxt = "n")
  axis(1, at = c(1:12+0.5),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  axis(2, at = c(1:6),
       labels = c("Obs","Ref","+1","+2","+3","+4"))
  
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(0.6,1.4,1.4,0.6,0.6),col=BGCol[n],border="NA")
    Ant <- Result[1,n+1]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(0.8,1.2,1.2,0.8,0.8),col="black",border="NA")
    }
  }
  
  PData <- colMeans(Result[2:33,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(1.6,2.4,2.4,1.6,1.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(1.8,2.2,2.2,1.8,1.8),col="black",border="NA")
    }
  }
  
  PData <- colMeans(PWarm1Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(2.6,3.4,3.4,2.6,2.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(2.8,3.2,3.2,2.8,2.8),col="#0034AB",border="NA")
    }
  }
  
  PData <- colMeans(PWarm2Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(3.6,4.4,4.4,3.6,3.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(3.8,4.2,4.2,3.8,3.8),col="#F79420",border="NA")
    }
  }
  
  PData <- colMeans(PWarm3Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(4.6,5.4,5.4,4.6,4.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(4.8,5.2,5.2,4.8,4.8),col="#E71D25",border="NA")
    }
  }
  
  PData <- colMeans(PWarm4Result[,2:13])
  for(n in 1:12){
    polygon(x=c(n,n,n+0.9,n+0.9,n),y=c(5.6,6.4,6.4,5.6,5.6),col=BGCol[n],border="NA")
    Ant <- PData[n]
    if(Ant>0){Just <- Ant/30;
    polygon(x=c(n,n,n+Just,n+Just,n),y=c(5.8,6.2,6.2,5.8,5.8),col="darkred",border="NA")
    }
  }
  #Result
}
