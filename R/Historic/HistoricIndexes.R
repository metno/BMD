require(readxl)
#setwd("~/Rscript/BMD")

#NB check paths for your computer

BMD.read <- function(Element = "MaxT"){
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

BMD.Normal <- function(Element="MaxT",Station=11111){
  Data <- BMD.read(Element=Element)
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
  Resultat
}

NormalPlot <- function(Station=11111){
  MaxTemp <- BMD.Normal(Element="MaxT",Station = Station)
  MinTemp <- BMD.Normal(Element="MinT",Station = Station)
  plot(c(1:12),colMeans(MinTemp[MinTemp[,1]>=1961 & MinTemp[,1]<=1990,2:13],na.rm=T),pch=16,col="red",ylim = c(10,35),main=Station,ylab="Temperature",xlab = "Month")
  points(c(1:12),colMeans(MaxTemp[MaxTemp[,1]>=1961 & MaxTemp[,1]<=1990,2:13],na.rm=T),pch=16,col="red")
  points(c(1:12),colMeans(MinTemp[MinTemp[,1]>=1991 & MinTemp[,1]<=2020,2:13],na.rm=T),pch=16,col="green")
  points(c(1:12),colMeans(MaxTemp[MaxTemp[,1]>=1991 & MaxTemp[,1]<=2020,2:13],na.rm=T),pch=16,col="green")
  
}

BMD.DTR <- function(Station=11111, graf=F){
  MinT <- BMD.read("MinT")
  MaxT <- BMD.read("MaxT")
#  LMinT <- length(MinT[,1])
#  Data <- c()
#  for(n in 1:LMinT){
#    D2 <- MinT[n,]
#    D2 <- c(D2,MaxT[MaxT[,1]==D2[1] & MaxT[,2]==D2[2] & MaxT[,3]==D2[3] & MaxT[,4]==D2[4],5])
#    Data <- rbind(Data,D2)
#  }
  Data <- cbind(MaxT,MinT[,5],MaxT[,5]-MinT[,5])
  Data <- Data[Data[,1]==Station,]
  Y <- unique(Data[,2])
  
  Resultat <- c()
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(year in min(Y):max(Y)){
    R2 <- year
    #print(year)
    for(month in 1:12){
      D2 <- Data[Data[,2]==year & Data[,3]==month,7]
      LD2 <- length(D2[!is.na(D2)])
      #print(LD2)
      if(LD2 >= (MDays[month]-5)){
        R2 <- c(R2,mean(D2,na.rm = T))
      }
      if(LD2 < (MDays[month]-5)){
        R2 <- c(R2,NA)
      }
    }
    Resultat <- rbind(Resultat, R2)
    #print(Resultat)
  }
  if(graf){
    plot(c(1:12),colMeans(Resultat[Resultat[,1]>=1961 & Resultat[,1]<=1990,2:13],na.rm=T),pch=16,col="red",ylim = c(0,15),main=Station,ylab="Daily Temperature Range",xlab = "Month")
    points(c(1:12),colMeans(Resultat[Resultat[,1]>=1991 & Resultat[,1]<=2020,2:13],na.rm=T),pch=16,col="green")
  }
#  Data
  Resultat
}

BMD.Sunshine <- function(Station="Dhaka",Graf=F){
  Data <- read.table("Data_2020/Sunshine_hour.txt",header=T,na.strings = "****")
  Data <- Data[Data[,1]==Station,]

  Y <- unique(Data[,2])
  
  Resultat <- c()
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(year in min(Y):max(Y)){
    R2 <- year
    #print(year)
    for(month in 1:12){
      D2 <- Data[Data[,2]==year & Data[,3]==month,5]
      LD2 <- length(D2[!is.na(D2)])
      #print(LD2)
      if(LD2 >= (MDays[month]-5)){
        R2 <- c(R2,mean(D2,na.rm = T))
      }
      if(LD2 < (MDays[month]-5)){
        R2 <- c(R2,NA)
      }
    }
    Resultat <- rbind(Resultat, R2)
    #print(Resultat)
  }
  if(Graf){
  plot(c(1:12),colMeans(Resultat[Resultat[,1]>=1961 & Resultat[,1]<=1990,2:13],na.rm=T),pch=16,col="red",ylim = c(0,15),main=Station,ylab="Average sunhine hours",xlab = "Month")
  points(c(1:12),colMeans(Resultat[Resultat[,1]>=1991 & Resultat[,1]<=2020,2:13],na.rm=T),pch=16,col="green")}
  #  Data
  Resultat
}

BMD.Cloud.Reform <- function(){
  Result <- c()
  Cloud <- read.table("Data_2020/Cloud.txt",header=F,skip=6)
  #Cloud <- Cloud[1:5000,]
  Stations <- unique(Cloud[,1])
  n<-0
  for(St in Stations){
    C2 <- Cloud[Cloud[,1]==St,]
    Years <- unique(C2[,2])
    #print(is.array(as.array(Years)))
    for(Y in Years){
      print(Y)
      C3 <- C2[C2[,2]==Y,]
      Months <- unique(C3[,3])
      for(M in Months){
        C4 <- C3[C3[,3]==M,]
        Days <- unique(C4[,4])
        for(D in Days){
          n<-n+1
          Row<-c()
          C5 <- C4[C4[,4]==D,]
          LC5 <- length(C5[,1])
          print(c(St,Y,M,D,LC5))
          if (LC5==8){
            Row<-c(St,Y,M,D,C5[,6])
          }
          if(LC5<8){
            Row<-c(St,Y,M,D,array(NA,24))
            Row[C5[,5]+5]<-C5[,6]
            Row <- Row[c(1:5,8,11,14,17,20,23,26)]
          }
          Result <- rbind(Result,Row)
        }
      }
    }
  }
  colnames(Result)<-c("St","Y","M","D",0,3,6,9,12,15,18,21)
  Result
}

BMD.Cloud.Process <- function(CData){
  #Mean
  CData <- cbind(CData,rowMeans(CData[,5:12]),rowSums(CData[,c(5,9,12)]))
  CData
}

BMD.Cloud.Month <- function(CData,Station=11111){
  CData <- CData[CData[,1]==Station,]
  Y <- unique(CData[,2])
  
  Resultat <- c()
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(year in min(Y):max(Y)){
    R2 <- year
    print(year)
    for(month in 1:12){
      D2 <- CData[CData[,2]==year & CData[,3]==month,]
      LD2 <- length(D2[!is.na(D2[,1]),1])
      print(LD2)
      if(LD2 >= (MDays[month]-5)){
        R2 <- c(R2,mean(D2[,13],na.rm = T))
      }
      if(LD2 < (MDays[month]-5)){
        R2 <- c(R2,NA)
      }
    }
    Resultat <- rbind(Resultat, R2)
    #print(Resultat)
  }
  
  plot(c(1:12),colMeans(Resultat[Resultat[,1]>=1961 & Resultat[,1]<=1990,2:13],na.rm=T),pch=16,col="red",ylim = c(0,10),main=Station,ylab="Average dailysum of clouds",xlab = "Month")
  points(c(1:12),colMeans(Resultat[Resultat[,1]>=1991 & Resultat[,1]<=2020,2:13],na.rm=T),pch=16,col="green")
  #  Data
  Resultat
}

BMD.Cloud.Month.Hour <- function(Station=11111, Hour=6, Graf=FALSE){ #Time in UTC, 6UTC=12 Bangladesh)
  Data <- BMD.read("Cloud")
  Data <- Data[Data[,1]==Station,]
  
  Data <- Data[,c(1:4,(Hour/3+5))]
  
  Y <- unique(Data[,2])
  
  Resultat <- c()
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(year in min(Y):max(Y)){
    R2 <- year
    #print(year)
    for(month in 1:12){
      D2 <- Data[Data[,2]==year & Data[,3]==month,5]
      LD2 <- length(D2[!is.na(D2)])
      #print(LD2)
      if(LD2 >= (MDays[month]-5)){
        R2 <- c(R2,mean(D2,na.rm = T))
      }
      if(LD2 < (MDays[month]-5)){
        R2 <- c(R2,NA)
      }
    }
    Resultat <- rbind(Resultat, R2)
    #print(Resultat)
  }
  if(Graf){
    plot(c(1:12),colMeans(Resultat[Resultat[,1]>=1961 & Resultat[,1]<=1990,2:13],na.rm=T),pch=16,col="red",ylim = c(0,15),main=Station,ylab="Average octa",xlab = "Month")
    points(c(1:12),colMeans(Resultat[Resultat[,1]>=1991 & Resultat[,1]<=2020,2:13],na.rm=T),pch=16,col="green")}
  #  Data
  Resultat
}

BMD.ColdWave <- function(StNr=11111,ColdLimit=10,HeatLimit=36){
  DaysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  Data <- BMD.read(Element = "MinT")
  DataMax <- BMD.read(Element = "MaxT")
  SData <- Data[Data[,1]==StNr,]
  SDataMax <- DataMax[DataMax[,1]==StNr,]
  Y <- unique(SData[,2])
  SData <- cbind(SData,SData[,5])
  SData[SData[,6]<=ColdLimit & !is.na(SData[,6]),6] <- 2
  SData[SData[,6]>ColdLimit & !is.na(SData[,6]),6] <- 3
  SDataMax <- cbind(SDataMax,SDataMax[,5])
  SDataMax[SDataMax[,6]<HeatLimit & !is.na(SDataMax[,6]),6] <- 3
  SDataMax[SDataMax[,6]>=HeatLimit & !is.na(SDataMax[,6]),6] <- 2
  plot(10000,10000,ylim=c(range(Y)),xlim=c(0,366),
       ylab="",xlab="",main=StNr, xaxt = "n")
#  plot(10000,10000,ylim=c(range(Y)),xlim=c(0,366),
#       ylab="",xlab="",main="Dhaka", xaxt = "n")
  axis(1, at = c(1,32,60,91,121,152,182,213,243,274,305,335),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  
  polygon(c(sum(DaysInMonth[1:2]),sum(DaysInMonth[1:2]),sum(DaysInMonth[1:5]),sum(DaysInMonth[1:5]),sum(DaysInMonth[1:2])),c(0,10000,10000,0,0),col="wheat")
  polygon(c(sum(DaysInMonth[1:5]),sum(DaysInMonth[1:5]),sum(DaysInMonth[1:9]),sum(DaysInMonth[1:9]),sum(DaysInMonth[1:5])),c(0,10000,10000,0,0),col="lightblue")
  polygon(c(sum(DaysInMonth[1:9]),sum(DaysInMonth[1:9]),sum(DaysInMonth[1:11]),sum(DaysInMonth[1:11]),sum(DaysInMonth[1:9])),c(0,10000,10000,0,0),col="lightgreen")
  
  Cols <- c("lightgreen","blue")
  ColsMax <- c("lightgreen","red")
  for(n in Y){
    YSD <- SData[SData[,2]==n,]
    LYSD <- length(YSD[,6])
    
    YSDMax <- SDataMax[SDataMax[,2]==n,]
    LYSDMax <- length(YSDMax[,6])
    
    points(c(1:LYSD),array(n,LYSD),pch=".")
    points(c(1:LYSD),array(n,LYSD), col=Cols[YSD[,6]],pch=16)
    points(c(1:LYSDMax),array(n,LYSDMax), col=ColsMax[YSDMax[,6]],pch=16)
    
    lines(c(0,0),c(-1000,10000),lty=3)
    for(m in 1:12){
      lines(c(sum(DaysInMonth[1:m])+1,sum(DaysInMonth[1:m])+1),c(-1000,10000),lty=3)
    }
  }
}

BMD.Trend.Month.DTR <- function(Data = NA, Station=11604,StYear=1980,Month=1) {
  if(is.na(Data)){Data <- BMD.DTR(Station = Station)}
  Data <- Data[Data[,1]>=StYear,]
  Kurve <- lm(Data[,(Month+1)]~Data[,1])
  Trend <- round(Kurve$coefficients[2],3)*10
  PVerd <- round(anova(Kurve)$`Pr(>F)`[1],5)
  plot(Data[,1],Data[,Month+1],pch=16,
       main=paste(Station, ", month: ", Month, sep=""),
       sub=paste("Trend: ", Trend, ", p-value: ", PVerd,sep=""),ylab="DTR",xlab="")
  lines(c(0,10000),c(Kurve$coefficients[1],Kurve$coefficients[1]+Kurve$coefficients[2]*10000))
}

BMD.Trend.Month.Temp <- function(Data.max = NA, Data.min = NA, Station=11604,StYear=1980,Month=1) {
  if(is.na(Data.max)){Data.max <- BMD.Normal(Station = Station,Element="MaxT")}
  if(is.na(Data.min)){Data.min <- BMD.Normal(Station = Station,Element="MinT")}
  Data.max <- Data.max[Data.max[,1]>=StYear,]
  Data.min <- Data.min[Data.min[,1]>=StYear,]
  Curve.max <- lm(Data.max[,(Month+1)]~Data.max[,1])
  Curve.min <- lm(Data.min[,(Month+1)]~Data.min[,1])
  Trend.max <- round(Curve.max$coefficients[2],3)*10
  Trend.min <- round(Curve.min$coefficients[2],3)*10
  PVal.max <- round(anova(Curve.max)$`Pr(>F)`[1],5)
  PVal.min <- round(anova(Curve.min)$`Pr(>F)`[1],5)
  plot(Data.max[,1],Data.max[,Month+1],
       pch=16, col="Darkred",
       ylim = c(min(Data.min[,Month+1],na.rm=T),max(Data.max[,Month+1],na.rm=T)),
       main=paste(Station, ", month: ", Month, sep=""),
       sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
       ylab="DTR",xlab="")
  lines(c(0,10000),c(Curve.max$coefficients[1],Curve.max$coefficients[1]+Curve.max$coefficients[2]*10000),col="Darkred")
  points(Data.min[,1],Data.min[,Month+1],col="Darkblue",pch=16)
  lines(c(0,10000),c(Curve.min$coefficients[1],Curve.min$coefficients[1]+Curve.min$coefficients[2]*10000),col="Darkgreen")
}

BMD.Trend.Temp.Multi <- function(Data.max = NA, Data.min = NA, Station=11604,StYear=1980) {
  if(is.na(Data.max)){Data.max <- BMD.Normal(Station = Station,Element="MaxT")}
  if(is.na(Data.min)){Data.min <- BMD.Normal(Station = Station,Element="MinT")}
  Data.max <- Data.max[Data.max[,1]>=StYear,]
  Data.min <- Data.min[Data.min[,1]>=StYear,]
  ymin <- min(min(Data.min[,2:13],na.rm=T))
  ymax <- max(max(Data.max[,2:13],na.rm=T))
  
  par(mfrow=c(3,4))
  n <- 0
  for (Month in 1:12){
    n <- n+1
    Curve.max <- lm(Data.max[,(Month+1)]~Data.max[,1])
    Curve.min <- lm(Data.min[,(Month+1)]~Data.min[,1])
    Trend.max <- round(Curve.max$coefficients[2],3)*10
    Trend.min <- round(Curve.min$coefficients[2],3)*10
    PVal.max <- round(anova(Curve.max)$`Pr(>F)`[1],3)
    PVal.min <- round(anova(Curve.min)$`Pr(>F)`[1],3)
    plot(Data.max[,1],Data.max[,Month+1],
         pch=16, col="Darkred",
         ylim = c(ymin,ymax),
         main=paste(Station, ", month: ", Month, sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend.max, ", p: ", PVal.max, "\n Min, Trend: ", Trend.min, ", p: ", PVal.min,sep=""),
         ylab="",xlab="")
    lines(c(0,10000),c(Curve.max$coefficients[1],Curve.max$coefficients[1]+Curve.max$coefficients[2]*10000),col="Darkred")
    points(Data.min[,1],Data.min[,Month+1],col="Darkblue",pch=16)
    lines(c(0,10000),c(Curve.min$coefficients[1],Curve.min$coefficients[1]+Curve.min$coefficients[2]*10000),col="Darkgreen")
  }
}

BMD.Trend.Temp.Season.Multi <- function(Data.max = NA, Data.min = NA, Station=11604,StYear=1980) {
  if(is.na(Data.max)){Data.max <- BMD.Normal(Station = Station,Element="MaxT")}
  if(is.na(Data.min)){Data.min <- BMD.Normal(Station = Station,Element="MinT")}
  Data.max <- Data.max[Data.max[,1]>=StYear,]
  Data.min <- Data.min[Data.min[,1]>=StYear,]
  Data.max <- cbind(Data.max[,1],
                    rowMeans(Data.max[,c(12,1,2)+1]),
                    rowMeans(Data.max[,c(3:5)]+1),
                    rowMeans(Data.max[,c(6:9)]+1),
                    rowMeans(Data.max[,c(10,11)+1]))
  Data.min <- cbind(Data.min[,1],
                    rowMeans(Data.min[,c(12,1,2)+1]),
                    rowMeans(Data.min[,c(3:5)]+1),
                    rowMeans(Data.min[,c(6:9)]+1),
                    rowMeans(Data.min[,c(10,11)+1]))
  ymin <- min(min(Data.min[,2:4],na.rm=T))
  ymax <- max(max(Data.max[,2:4],na.rm=T))
  
  Seasons <- c("Winter","Pre monsoon", "Monsoon", "Post Monsoon")
  
  par(mfrow=c(2,2),mgp=c(1.8,0.5,0),mar=c(4.1, 4.1, 3.1, 2.1))
  for (Month in 1:4){
    ylabel <- ""
    if (Month==1 | Month == 3){ylabel<- "Celsius"}
    Curve.max <- lm(Data.max[,(Month+1)]~Data.max[,1])
    Curve.min <- lm(Data.min[,(Month+1)]~Data.min[,1])
    Trend.max <- round(Curve.max$coefficients[2],3)*10
    Trend.min <- round(Curve.min$coefficients[2],3)*10
    PVal.max <- round(anova(Curve.max)$`Pr(>F)`[1],3)
    PVal.min <- round(anova(Curve.min)$`Pr(>F)`[1],3)
    plot(Data.max[,1],Data.max[,Month+1],
         pch=16, col="Darkred",
         ylim = c(ymin,ymax),
         #main=paste(Station, ", ", Seasons[Month], sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend.max, ", p: ", PVal.max, "\n Min, Trend: ", Trend.min, ", p: ", PVal.min,sep=""),
         ylab=ylabel,xlab="")
    title(paste(Station, ", ", Seasons[Month], sep=""),line=0.5)
    lines(c(0,10000),c(Curve.max$coefficients[1],Curve.max$coefficients[1]+Curve.max$coefficients[2]*10000),col="Darkred")
    points(Data.min[,1],Data.min[,Month+1],col="Darkblue",pch=16)
    lines(c(0,10000),c(Curve.min$coefficients[1],Curve.min$coefficients[1]+Curve.min$coefficients[2]*10000),col="Darkgreen")
  }
}

BMD.Trend.Rain.Season.Multi <- function(Data = NA, Station=11604,StYear=1980) {
  if(is.na(Data)){Data <- BMD.Normal(Station = Station,Element="Rainfall")}
  Data <- Data[Data[,1]>=StYear,]
  Data <- cbind(Data[,1],
                    rowSums(Data[,c(12,1,2)+1]),
                    rowSums(Data[,c(3:5)]+1),
                    rowSums(Data[,c(6:9)]+1),
                    rowSums(Data[,c(10,11)+1]))
  ymin <- min(min(Data[,2:4],na.rm=T))
  ymax <- max(max(Data[,2:4],na.rm=T))
  
  Seasons <- c("Winter","Pre monsoon", "Monsoon", "Post Monsoon")
  
  par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(4.1, 4.1, 3.1, 2.1))
  for (Month in 1:4){
    ylabel <- ""
    if (Month==1 | Month == 3){ylabel<- "mm"}
    Curve <- lm(Data[,(Month+1)]~Data[,1])
    Trend <- round(Curve$coefficients[2],3)*10
    PVal <- round(anova(Curve)$`Pr(>F)`[1],3)
    plot(Data[,1],Data[,Month+1],
         pch=16, col="Darkblue",
         ylim = c(ymin,ymax),
         #main=paste(Station, ", ", Seasons[Month], sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend, ", p: ", PVal,sep=""),
         ylab=ylabel,xlab="")
    title(paste(Station, ", ", Seasons[Month], sep=""),line=0.5)
    lines(c(0,10000),c(Curve$coefficients[1],Curve$coefficients[1]+Curve$coefficients[2]*10000),col="Darkblue")
  }
}

BMD.Trend.DTR.Season.Multi <- function(Data = NA, Station=11604,StYear=1980) {
  if(is.na(Data)){Data <- BMD.DTR(Station = Station)}
  Data <- Data[Data[,1]>=StYear,]
  Data <- cbind(Data[,1],
                rowMeans(Data[,c(12,1,2)+1]),
                rowMeans(Data[,c(3:5)]+1),
                rowMeans(Data[,c(6:9)]+1),
                rowMeans(Data[,c(10,11)+1]))
  ymin <- min(min(Data[,2:4],na.rm=T))
  ymax <- max(max(Data[,2:4],na.rm=T))
  
  Seasons <- c("Winter","Pre monsoon", "Monsoon", "Post Monsoon")
  
  par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(4.1, 4.1, 3.1, 2.1))
  for (Month in 1:4){
    ylabel <- ""
    if (Month==1 | Month == 3){ylabel<- "Celsius"}
    Curve <- lm(Data[,(Month+1)]~Data[,1])
    Trend <- round(Curve$coefficients[2],3)*10
    PVal <- round(anova(Curve)$`Pr(>F)`[1],3)
    plot(Data[,1],Data[,Month+1],
         pch=16, col="Darkorange",
         ylim = c(ymin,ymax),
         #main=paste(Station, ", ", Seasons[Month], sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend, ", p: ", PVal,sep=""),
         ylab=ylabel,xlab="")
    title(paste(Station, ", ", Seasons[Month], sep=""),line=0.5)
    lines(c(0,10000),c(Curve$coefficients[1],Curve$coefficients[1]+Curve$coefficients[2]*10000),col="Darkorange")
  }
}

BMD.Trend.Sunshine.Season.Multi <- function(Data = NA, Station="Dhaka", StYear=1980) {
  if(is.na(Data)){Data <- BMD.Sunshine(Station = Station)}
  Data <- Data[Data[,1]>=StYear,]
  Data <- cbind(Data[,1],
                rowMeans(Data[,c(12,1,2)+1]),
                rowMeans(Data[,c(3:5)]+1),
                rowMeans(Data[,c(6:9)]+1),
                rowMeans(Data[,c(10,11)+1]))
  ymin <- min(min(Data[,2:4],na.rm=T))
  ymax <- max(max(Data[,2:4],na.rm=T))
  
  Seasons <- c("Winter","Pre monsoon", "Monsoon", "Post Monsoon")
  
  par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(4.1, 4.1, 3.1, 2.1))
  for (Month in 1:4){
    ylabel <- ""
    if (Month==1 | Month == 3){ylabel<- "Hours"}
    Curve <- lm(Data[,(Month+1)]~Data[,1])
    Trend <- round(Curve$coefficients[2],3)*10
    PVal <- round(anova(Curve)$`Pr(>F)`[1],3)
    plot(Data[,1],Data[,Month+1],
         pch=16, col="Darkorange",
         ylim = c(ymin,ymax),
         #main=paste(Station, ", ", Seasons[Month], sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend, ", p: ", PVal,sep=""),
         ylab=ylabel,xlab="")
    title(paste(Station, ", ", Seasons[Month], sep=""),line=0.5)
    lines(c(0,10000),c(Curve$coefficients[1],Curve$coefficients[1]+Curve$coefficients[2]*10000),col="Darkorange")
  }
}

BMD.Trend.Cloud.Season.Multi.Hour <- function(Data = NA, Station=11111, StYear=1980, Hour=6) { #Hour 6UTC id midday
  if(is.na(Data)){Data <- BMD.Cloud.Month.Hour(Station = Station, Hour=Hour)}
  Data <- Data[Data[,1]>=StYear,]
  Data <- cbind(Data[,1],
                rowMeans(Data[,c(12,1,2)+1]),
                rowMeans(Data[,c(3:5)]+1),
                rowMeans(Data[,c(6:9)]+1),
                rowMeans(Data[,c(10,11)+1]))
  ymin <- min(min(Data[,2:4],na.rm=T))
  ymax <- max(max(Data[,2:4],na.rm=T))
  
  Seasons <- c("Winter","Pre monsoon", "Monsoon", "Post Monsoon")
  
  par(mfrow=c(2,2),mgp=c(1.5,0.5,0),mar=c(4.1, 4.1, 3.1, 2.1))
  for (Month in 1:4){
    ylabel <- ""
    if (Month==1 | Month == 3){ylabel<- "Octa"}
    Curve <- lm(Data[,(Month+1)]~Data[,1])
    Trend <- round(Curve$coefficients[2],3)*10
    PVal <- round(anova(Curve)$`Pr(>F)`[1],3)
    plot(Data[,1],Data[,Month+1],
         pch=16, col="Darkgray",
         ylim = c(0,8),
         #main=paste(Station, ", ", Seasons[Month], sep=""),
         #sub=paste("Trend of max: ", Trend.max, ", p-value of max: ", PVal.max, ", trend of min: ", Trend.min, ", p-value of min: ", PVal.min,sep=""),
         sub=paste("Max, Trend: ", Trend, ", p: ", PVal,sep=""),
         ylab=ylabel,xlab="")
    title(paste(Station, ", ", Seasons[Month], ", ", Hour, " UTC", sep=""),line=0.5)
    lines(c(0,10000),c(Curve$coefficients[1],Curve$coefficients[1]+Curve$coefficients[2]*10000),col="Darkgray")
  }
}

BMD.Raifall.Frequency.Month <- function(Data = NA, Station=11604, StYear=1980, Graf=F, Month=7){
  if(is.na(Data)){Data <- BMD.read(Element="Rainfall")}
  Data <- Data[Data[,1]==Station,]
  Data <- Data[Data[,2]>=StYear,]
  Years <- unique(Data[,2])
  MDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  Result <- c()
  for (Y in Years){
    DY <- Data[Data[,2]==Y,]
    for (M in 1:12){
      DM <- DY[DY[,3]==M,]
      LDM <- length(DM[!is.na(DM[,5]),5])
      if(LDM >= MDays[M]-5){
        R <- c(DM[1,2:3],
               length(DM[DM[,5]<1,5]),
               length(DM[DM[,5]>=1 & DM[,5]<11,5]),
               length(DM[DM[,5]>=11 & DM[,5]<23,5]),
               length(DM[DM[,5]>=23 & DM[,5]<44,5]),
               length(DM[DM[,5]>=44 & DM[,5]<89,5]),
               length(DM[DM[,5]>=89,5]))
        
      }
      if(LDM < MDays[M]-5){
        R <- c(DM[1,2:3],array(NA,6))
      }
      Result <- rbind(Result, R)
    }
  }
  if(Graf){
    GD <- Result[Result[,2]==Month,]
    rownames(GD) <- GD[,1]
    colnames(GD) <- c("Year","Month","Dry","Light","Moderate","Modrate Heavy","Heavy","Very Heavy")
    GD <- t(GD[,3:8])
    barplot(GD,col=c("Brown","lightgreen","lightblue","blue","darkblue","purple"))
  }
  Result
}

BMD.Raifall.Frequency.Seasons <- function(Data = NA, Station=11604, StYear=1980, Graf=F){
  Data <- BMD.Raifall.Frequency.Month(Data = Data, Station = Station, StYear = StYear)
  Years <- unique(Data[,1])
  Result <- c()
  for(Y in Years){
    print(Y)
    DY <- Data[Data[,1]==Y,]
    print(DY[DY[2,]==12 | DY[2,]==1 | DY[2,]==2,])
    Result <- rbind(Result,c(Y,1,colSums(DY[DY[2,]==12 | DY[2,]==1 | DY[2,]==2,c(3:8)])))
    print(DY[DY[,2]==3 | DY[,2]==4 | DY[,2]==5,])
    Result <- rbind(Result,c(Y,2,colSums(DY[DY[,2]==3 | DY[,2]==4 | DY[,2]==5,c(3:8)])))
    print(DY[DY[2,]==6 | DY[2,]==7 | DY[2,]==8 | DY[2,]==9,])
    Result <- rbind(Result,c(Y,3,colSums(DY[DY[2,]==6 | DY[2,]==7 | DY[2,]==8 | DY[2,]==9,c(3:8)])))
    print(DY[DY[2,]==10 | DY[2,]==11,])
    Result <- rbind(Result,c(Y,4,colSums(DY[DY[2,]==10 | DY[2,]==11,c(3:8)])))
  }
  Result
}
