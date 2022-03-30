library(timeSeries)
library(timeDate)
library(FinTS)
library(fGarch)
library(rugarch)
library(TSstudio)
setwd("C:/Users/RAJEEV KRISHNA S/Dropbox/My PC (DESKTOP-HF4IT6B)/Desktop/PGDBA COURSE/IIM C/SELECTED ASPECTS OF ADVANCED PREDICTIVE MODELLING")

data=read.csv("finaldata.csv")
head(data)
print(nrow(data))
print(ncol(data))
print(summary(data))

tsactivecases=ts(data$Active.Cases)
plot(tsactivecases,xlab='Time',ylab='Rate of change in active Covid cases')
tsbsemetal=ts(data$BSE_metal)
plot(tsbsemetal,xlab='Time',ylab='Daily % returns in BSE metal index ')
tsbsehealth=ts(data$BSE_healthcare)
plot(tsbsehealth,xlab='Time',ylab='Daily % returns in BSE Heathcare index ')
tsbseenergy=ts(data$BSE_energy)
plot(tsbseenergy,xlab='Time',ylab='Daily % returns in BSE Energy index')
tsbsefinance=ts(data$BSE_finance)
plot(tsbsefinance,xlab='Time',ylab='Daily % returns in BSE Finance index ')
tsbsefmcg=ts(data$ BSE_FMCG)
plot(tsbsefmcg,xlab='Time',ylab='Daily % returns in BSE FMCG index')
tsbseit=ts(data$BSE_IT)
plot(tsbseit,xlab='Time',ylab='Daily % returns in BSE Information technology index')

library(tseries)
adf.test(tsactivecases)
adf.test(tsbsemetal)
adf.test(tsbsehealth)
adf.test(tsbseenergy)
adf.test(tsbsefinance)
adf.test(tsbsefmcg)
adf.test(tsbseit)


acf(tsbsemetal)
acf(tsbsehealth)
acf(tsbseenergy)
acf(tsbsefinance)
acf(tsbsefmcg)
acf(tsbseit)
acf(tsactivecases)

pacf(tsbsemetal)
pacf(tsbsehealth)
pacf(tsbseenergy)
pacf(tsbsefinance)
pacf(tsbsefmcg)
pacf(tsbseit)
pacf(tsactivecases)


library(vars)
VAR_data <- window(ts.union(tsactivecases,tsbseit,tsbsefmcg,tsbsefinance,tsbseenergy,tsbsehealth,tsbsemetal),start=1,end=361)
VAR_data 
Test_data <- window(ts.union(tsactivecases,tsbseit,tsbsefmcg,tsbsefinance,tsbseenergy,tsbsehealth,tsbsemetal),start=362,end=382)
Test_data 
VARselect(VAR_data)

VAR_est <- VAR(y = VAR_data, p = 9)
summary(VAR_est)


causality(VAR_est, cause = "tsactivecases", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbsemetal", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbsehealth", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbsefmcg", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbsefinance", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbseenergy", vcov.=vcovHC(VAR_est))
causality(VAR_est, cause = "tsbseit", vcov.=vcovHC(VAR_est))


impresp <- irf(VAR_est,n.ahead=10)
plot(impresp)


fevd <- fevd(VAR_est, n.ahead =20)
plot(fevd)

forecasts <- predict(VAR_est,n.ahead=20,ci=.95)
plot(forecasts)

library(Metrics)
predict_activecases<-forecasts$fcst[[1]][,"fcst"]
predict_bseit<-forecasts$fcst[[2]][,"fcst"]
predict_bsefmcg<-forecasts$fcst[[3]][,"fcst"]
predict_bsefinance<-forecasts$fcst[[4]][,"fcst"]
predict_bseenergy<-forecasts$fcst[[5]][,"fcst"]
predict_bsehealth<-forecasts$fcst[[6]][,"fcst"]
predict_bsemetal<-forecasts$fcst[[7]][,"fcst"]

actual_activecases=Test_data[,1] 
actual_bseit=Test_data[,2]
actual_bsefmcg=Test_data[,3]
actual_bsefinance=Test_data[,4]
actual_bseenergy=Test_data[,5]
actual_bsehealth=Test_data[,6]
actual_bsemetal=Test_data[,7]


rmse(actual_activecases,predict_activecases)
rmse(actual_bseit,predict_bseit)
rmse(actual_bsefmcg,predict_bsefmcg)
rmse(actual_bsefinance,predict_bsefinance)
rmse(actual_bseenergy,predict_bseenergy)
rmse(actual_bsehealth,predict_bsehealth)
rmse(actual_bsemetal,predict_bsemetal)


mape(actual_activecases,predict_activecases)
mape(actual_bseit,predict_bseit)
mape(actual_bsefmcg,predict_bsefmcg)
mape(actual_bsefinance,predict_bsefinance)
mape(actual_bseenergy,predict_bseenergy)
mape(actual_bsehealth,predict_bsehealth)
mape(actual_bsemetal,predict_bsemetal)

