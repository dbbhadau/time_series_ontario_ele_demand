library(fma)
library(Metrics)
library(fpp)
library(astsa)
library(zoo)
library(sqldf)
library(e1071)
library(caTools)
#rmse
rmse <- function(error)
{
  sqrt(mean(error^2))
}

DF<- data.frame(read.csv("C:/Users/Disha Bhadauria/Desktop/718/project/ZonalDemands_2017.csv"))
TRAINING<-sqldf("select
      Date,avg(Ontario_Demand) as Ontario_Demand,avg(ottawa) as Ottawa, avg(Toronto) as Toronto, 
      avg(Niagara) as Niagara, avg(Bruce) as Bruce
      FROM DF
      group by Date")
TRAINING<-sqrt(TRAINING[,2:6])
TRAINING.ts<-ts(TRAINING)
DF2<-data.frame(read.csv("C:/Users/Disha Bhadauria/Desktop/718/project/ZonalDemands1.csv"))
TEST<-sqldf("select
     Date,avg(Ontario_Demand) as Ontario_Demand,avg(ottawa) as Ottawa, avg(Toronto) as Toronto, 
      avg(Niagara) as Niagara, avg(Bruce) as Bruce
            FROM DF2
            group by Date")
TEST<-sqrt(TEST[,2:6])
TEST.ts<-ts(TEST)
View(TRAINING)
View(TEST)
plot(TRAINING.ts[,2:6],main="ZONAL DEMANDS")
#TEST FOR Stationarity
#ONTARIO_DEMAND
adf.test(TRAINING$Ontario_Demand)
kpss.test(TRAINING$Ontario_Demand)
#Stationarity

#OTTAWA
adf.test(TRAINING$Ottawa)
kpss.test(TRAINING$Ottawa)
#Stationarity

#TORONTO
adf.test(TRAINING$Toronto)
kpss.test(TRAINING$Toronto)
# Stationarity

#NIAGARA
adf.test(TRAINING$Niagara)
kpss.test(TRAINING$Niagara)
#Stationarity

#BRUCE
adf.test(TRAINING$Bruce)
kpss.test(TRAINING$Bruce)
#Stationarity

#Benchmark Models
#Ontario_demand
#HOLTS METHOD
ON<-ts(TRAINING$Ontario_Demand) 
plot(ON)
fit_on_wh<-holt(ON,initial = "optimal",h=15)
lines(fit_on_wh$fitted, col = "blue", type="o")
accuracy(fit_on_wh$mean,TEST$Ontario_Demand)
qqnorm(fit_on_wh$residuals)
qqline(fit_on_wh$residuals)
#ETS METHOD
fit_on_ets<-ets(ON,model="ZZZ")
ets_on<-forecast(fit_on_ets,h=15)
lines(fit_on_ets$fitted, col = "red", type="o")
accuracy(ets_on,TEST$Ontario_Demand)
qqnorm(fit_on_ets$residuals)
qqline(fit_on_ets$residuals)
#ARIMA
fit_on_arima<-auto.arima(ON,approximation = FALSE)
arima_on<-forecast(fit_on_arima,h=15)
lines(fit_on_arima$fitted, col = "green", type="o")
accuracy(arima_on,TEST$Ontario_Demand)
summary(fit_on_arima)
acf(fit_on_arima$residuals)
qqnorm(fit_on_arima$residuals)
qqline(fit_on_arima$residuals)
#SVM
fit_ON_SVM <- svm(Ontario_Demand~.,data=TRAINING.ts,method="C-classification", kernel="linear")
ON_SVM_pred<-predict(fit_ON_SVM,TEST.ts)
lines(fit_ON_SVM$fitted, col = "PURPLE", type="o")
ON_SVM_error<-(TEST$Ontario_Demand-ON_SVM_pred)
ON_svm_RMSE <- rmse(ON_SVM_error)
ON_svm_RMSE
accuracy(ON_SVM_pred,TEST$Ontario_Demand)
qqnorm(fit_ON_SVM$residuals)
qqline(fit_ON_SVM$residuals)

#Logistic regression
fit_ON_LR<-glm(Ontario_Demand~.,data=TRAINING.ts)
predict <- predict(fit_ON_LR, type = 'response',TEST.ts)
lines(fit_ON_LR$fitted, col = "cyan", type="o")
ON_LR_error<-(TEST$Ontario_Demand-predict)
ON_LR_RMSE <- rmse(ON_LR_error)
ON_LR_RMSE  
qqnorm(fit_ON_LR$residuals)
qqline(fit_ON_LR$residuals)

legend("bottomleft",lty=1,col=c("blue","red","green","purple","cyan"),c("Holts","ETS","ARIMA","SVM","LR"))
#toronto
#HOLTS METHOD
TOR<-ts(TRAINING$Toronto) 
plot(TOR)
fit_tor_wh<-holt(TOR,initial = "optimal",h=15)
lines(fit_tor_wh$fitted, col = "blue", type="o")
accuracy(fit_tor_wh$mean,TEST$Toronto)
qqnorm(fit_tor_wh$residuals)
qqline(fit_tor_wh$residuals)
#ETS METHOD
fit_TOR_ets<-ets(TOR,model="ZZZ")
ets_TOR<-forecast(fit_TOR_ets,h=15)
lines(fit_TOR_ets$fitted, col = "red", type="o")
accuracy(ets_TOR,TEST$Toronto)
qqnorm(fit_TOR_ets$residuals)
qqline(fit_TOR_ets$residuals)
#ARIMA
fit_TOR_arima<-auto.arima(TOR,approximation = FALSE)
arima_TOR<-forecast(fit_TOR_arima,h=15)
lines(fit_TOR_arima$fitted, col = "green", type="o")
accuracy(arima_TOR,TEST$Toronto)
summary(fit_TOR_arima)
qqnorm(fit_TOR_arima$residuals)
qqline(fit_TOR_arima$residuals)
#SVM
fit_TOR_SVM <- svm(Toronto~.,data=TRAINING.ts,method="C-classification", kernel="linear")
TOR_SVM_pred<-predict(fit_TOR_SVM,TEST.ts)
lines(fit_TOR_SVM$fitted, col = "PURPLE", type="o")
TOR_SVM_error<-(TEST$Toronto-TOR_SVM_pred)
TOR_svm_RMSE <- rmse(TOR_SVM_error)
TOR_svm_RMSE
qqnorm(fit_TOR_SVM$residuals)
qqline(fit_TOR_SVM$residuals)
#logistic regression
fit_TOR_LR<-glm(Toronto~.,data=TRAINING.ts)
TOR_LR_PRED <- predict(fit_TOR_LR, type = 'response',TEST.ts)
lines(fit_TOR_LR$fitted, col = "cyan", type="o")
TOR_LR_error<-(TEST$Toronto-TOR_LR_PRED)
TOR_LR_RMSE <- rmse(TOR_LR_error)
TOR_LR_RMSE
qqnorm(fit_TOR_LR$residuals)
qqline(fit_TOR_LR$residuals)
legend("bottomleft",lty=1,col=c("blue","red","green","purple","cyan"),c("Holts","ETS","ARIMA","SVM","LR"))
#OTTAWA
#HOLTS METHOD
Ott<-ts(TRAINING$Ottawa) 
plot(Ott)
fit_Ott_wh<-holt(Ott,initial = "optimal",h=15)
lines(fit_Ott_wh$fitted, col = "blue", type="o")
accuracy(fit_Ott_wh$mean,TEST$Ottawa)
qqnorm(fit_Ott_wh$residuals)
qqline(fit_Ott_wh$residuals)
#ETS METHOD
fit_Ott_ets<-ets(Ott,model="ZZZ")
ets_Ott<-forecast(fit_Ott_ets,h=15)
lines(fit_Ott_ets$fitted, col = "red", type="o")
accuracy(ets_Ott,TEST$Ottawa)
qqnorm(fit_Ott_ets$residuals)
qqline(fit_Ott_ets$residuals)
#ARIMA
fit_Ott_arima<-auto.arima(Ott,approximation = FALSE)
arima_Ott<-forecast(fit_Ott_arima,h=15)
lines(fit_Ott_arima$fitted, col = "green", type="o")
accuracy(arima_Ott,TEST$Ottawa)
summary(fit_Ott_arima)
qqnorm(fit_Ott_arima$residuals)
qqline(fit_Ott_arima$residuals)
#SVM
fit_Ott_SVM <- svm(Ottawa~.,data=TRAINING.ts,method="C-classification", kernel="linear")
Ott_SVM_pred<-predict(fit_Ott_SVM,TEST.ts)
lines(fit_Ott_SVM$fitted, col = "PURPLE", type="o")
Ott_SVM_error<-(TEST$Ottawa-Ott_SVM_pred)
Ott_svm_RMSE <- rmse(Ott_SVM_error)
Ott_svm_RMSE
qqnorm(fit_Ott_SVM$residuals)
qqline(fit_Ott_SVM$residuals)
#logistic regression
fit_Ott_LR<-glm(Ottawa~.,data=TRAINING.ts)
Ott_LR_PRED <- predict(fit_Ott_LR, type = 'response',TEST.ts)
lines(fit_Ott_LR$fitted, col = "cyan", type="o")
Ott_LR_error<-(TEST$Ottawa-Ott_LR_PRED)
Ott_LR_RMSE <- rmse(Ott_LR_error)
Ott_LR_RMSE 
qqnorm(fit_Ott_LR$residuals)
qqline(fit_Ott_LR$residuals)
legend("bottomleft",lty=1,col=c("blue","red","green","purple","cyan"),c("Holts","ETS","ARIMA","SVM","LR"))
acf(Ott_LR_error)
#NIAGARA
#HOLTS METHOD
Nia<-ts(TRAINING$Niagara) 
plot(Nia)
fit_Nia_wh<-holt(Nia,initial = "optimal",h=15)
lines(fit_Nia_wh$fitted, col = "blue", type="o")
accuracy(fit_Nia_wh$mean,TEST$Niagara)
qqnorm(fit_Nia_wh$residuals)
qqline(fit_Nia_wh$residuals)
#ETS METHOD
fit_Nia_ets<-ets(Nia,model="ZZZ")
ets_Nia<-forecast(fit_Nia_ets,h=15)
lines(fit_Nia_ets$fitted, col = "red", type="o")
accuracy(ets_Nia,TEST$Niagara)
summary(fit_Nia_ets)
qqnorm(fit_Nia_ets$residuals)
qqline(fit_Nia_ets$residuals)
#ARIMA
fit_Nia_arima<-auto.arima(Nia,approximation = FALSE)
arima_Nia<-forecast(fit_Nia_arima,h=15)
lines(fit_Nia_arima$fitted, col = "green", type="o")
accuracy(arima_Nia,TEST$Niagara)
summary(fit_Nia_arima)
qqnorm(fit_Nia_arima$residuals)
qqline(fit_Nia_arima$residuals)
#SVM
fit_NIA_SVM <- svm(Niagara~.,data=TRAINING.ts,method="C-classification", kernel="linear")
NIA_SVM_pred<-predict(fit_NIA_SVM,TEST.ts)
lines(fit_NIA_SVM$fitted, col = "PURPLE", type="o")
NIA_SVM_error<-(TEST$Niagara-NIA_SVM_pred)
NIA_svm_RMSE <- rmse(NIA_SVM_error)
NIA_svm_RMSE
qqnorm(fit_NIA_SVM$residuals)
qqline(fit_NIA_SVM$residuals)
#logistic regression
fit_NIA_LR<-glm(Niagara~.,data=TRAINING.ts)
NIA_LR_PRED <- predict(fit_NIA_LR, type = 'response',TEST.ts)
lines(fit_NIA_LR$fitted, col = "cyan", type="o")
NIA_LR_error<-(TEST$Niagara-NIA_LR_PRED)
NIA_LR_RMSE <- rmse(NIA_LR_error)
NIA_LR_RMSE 
qqnorm(fit_NIA_LR$residuals)
qqline(fit_NIA_LR$residuals)


legend("bottomleft",lty=1,col=c("blue","red","green","purple","cyan"),c("Holts","ETS","ARIMA","SVM","LR"))
acf(NIA_SVM_error)

#BRUCE
#HOLTS METHOD
Bru<-ts(TRAINING$Bruce) 
plot(Bru)
acf(Bru)
pacf(Bru)
fit_Bru_wh<-holt(Bru,initial = "optimal",h=15)
lines(fit_Bru_wh$fitted, col = "blue", type="o")
accuracy(fit_Bru_wh$mean,TEST$Bruce)
qqnorm(fit_Bru_wh$residuals)
qqline(fit_Bru_wh$residuals)
#ETS METHOD
fit_Bru_ets<-ets(Bru,model="ZZZ")
ets_Bru<-forecast(fit_Bru_ets,h=15)
lines(fit_Bru_ets$fitted, col = "red", type="o")
accuracy(ets_Bru,TEST$Bruce)
qqnorm(fit_Bru_ets$residuals)
qqline(fit_Bru_ets$residuals)
#ARIMA
fit_Bru_arima<-auto.arima(Bru,approximation = FALSE)
arima_Bru<-forecast(fit_Bru_arima,h=15)
lines(fit_Bru_arima$fitted, col = "green", type="o")
accuracy(arima_Bru,TEST$Bruce)
summary(fit_Bru_arima)
qqnorm(fit_Bru_arima$residuals)
qqline(fit_Bru_arima$residuals)
#SVM
fit_BRU_SVM <- svm(Bruce~.,data=TRAINING.ts,method="C-classification", kernel="linear")
BRU_SVM_pred<-predict(fit_BRU_SVM,TEST.ts)
lines(fit_BRU_SVM$fitted, col = "PURPLE", type="o")
BRU_SVM_error<-(TEST$Bruce-BRU_SVM_pred)
BRU_svm_RMSE <- rmse(BRU_SVM_error)
BRU_svm_RMSE
qqnorm(fit_BRU_SVM$residuals)
qqline(fit_BRU_SVM$residuals)
#logistic regression
fit_BRU_LR<-glm(Bruce~.,data=TRAINING.ts)
BRU_LR_PRED <- predict(fit_BRU_LR, type = 'response',TEST.ts)
lines(fit_BRU_LR$fitted, col = "cyan", type="o")
BRU_LR_error<-(TEST$Bruce-BRU_LR_PRED)
BRU_LR_RMSE <- rmse(BRU_LR_error)
BRU_LR_RMSE 
qqnorm(fit_BRU_LR$residuals)
qqline(fit_BRU_LR$residuals)

legend("topright",lty=1,col=c("blue","red","green","purple","cyan"),c("Holts","ETS","ARIMA","SVM","LR"))
acf(fit_Bru_wh$residuals)

#OUTLIERS DETECTION
#Ontario outliers
out_on<-boxplot(TRAINING$Ontario_Demand,main="Ontario Demand")
out_on$out
hist(TRAINING$Ontario_Demand)
#Ottawa outliers
out_ott<-boxplot(TRAINING$Ottawa,main="Ottawa")
out_ott$out
hist(TRAINING$Ottawa)
#Toronto outliers
out_tor<-boxplot(TRAINING$Toronto,main="Toronto")
out_tor$out
hist(TRAINING$Toronto)
#Niagara outliers
out_nia<-boxplot(TRAINING$Niagara,main="Niagara")
out_nia$out
hist(TRAINING$Niagara)
#Bruce outliers
out_bru<-boxplot(TRAINING$Bruce,main="Bruce")
out_bru$out
hist(TRAINING$Bruce)


