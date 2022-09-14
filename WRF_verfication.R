
setwd("F:/WRF_Verification/Com_obs_model/2017")
library(Metrics)
library(dplyr)

X1 <- read.csv("Day_3_all_month_2017.csv", sep=",", header=TRUE)
X2 <- subset(X1[X1$Month%in%c(3:5),])
write.table(X2,file="Pre_Day3_2017.csv", sep=",",row.names=F)
my_data <-read.csv("Pre_Day3_2017.csv", sep = ",", header = TRUE)
head(my_data)
#tail(my_data)

PBIAS <- percent_bias(my_data[,6], my_data[,7])
View(PBIAS)
write.table(PBIAS,file = "PBIAS_Pre_day3_2017.csv", sep=",",row.names=F)

mae <- mae(my_data[,6], my_data[,7])
write.table(mae,file = "MAE_Pre_Day3_2017.csv", sep=",",row.names=F)
mse <- mean((my_data[,7] - my_data[,6])^2)
write.table(mse,file = "MSE_Pre_Day3_2017.csv", sep=",",row.names=F)
rmse <- sqrt(mean((my_data[,7] - my_data[,6])^2))
write.table(rmse,file = "RMSE_Pre_Day3_2017.csv", sep=",",row.names=F)



library("ggpubr")
ggscatter(my_data, x = "Observation", y = "Wrf_model", 
          add = "reg.line",color = "blue",
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observation", ylab = "Wrf_model")



h1 <- sum(my_data[,8] >=1 & my_data[,9] >=1, na.rm=TRUE)
f1 <- sum(my_data[,8] >=1 & my_data[,9] <=0, na.rm=TRUE)
m1 <- sum(my_data[,8] <=0 & my_data[,9] >=1, na.rm=TRUE)
n1 <- sum(my_data[,8] <=0 & my_data[,9] <=0, na.rm=TRUE)
#h2 <- sum(x$Feb >=1 & x$Mar >=1, na.rm=TRUE)
t1 <- h1+f1+m1+n1

bias1 <- (h1+f1)/(h1+m1)
pod1 <- (h1/(h1+m1))
far1 <- (f1/(h1+f1))
pofd1 <- (f1/(n1+f1))
sr1 <- (h1/(h1+f1))
ts1 <- (h1/(h1+m1+f1))
#hits random
hits1 <- ((h1+m1)*(h1+f1)/(h1+f1+m1+n1))
ets1 <- (h1-hits1)/(h1+m1+f1-hits1)
accuracy1 <- (h1+n1)/(h1+m1+f1+n1)
hk1 <- (h1/(h1+m1)-(f1/(f1+n1)))
oddr1 <- (pod1/(1-pod1))/(pofd1/(1-pofd1))

vs1 <- rbind(bias1,pod1,far1,pofd1,sr1,ts1,ets1,accuracy1,hk1,oddr1)
write.table(vs1,file="rainfall_Rangpur.csv", row.names=T, sep=",")


# Rainfall_fc, column 5 of the table
#h1 <- x[,5]
# Dhaka Obs, column 6 of the table
#h2 <- x[,6]
#Dhaka Obs, column 6-11 of the table 
#h1 <- x[,6:11]
#head(h1)
#Condition
#sum(data$rebounds > 8 & data$rebounds < 10, na.rm=TRUE)
