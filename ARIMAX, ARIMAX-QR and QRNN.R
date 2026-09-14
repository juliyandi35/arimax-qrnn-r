library(readxl)
X=read_excel("DATA_PENELITIAN.xlsx",sheet = "Inflow")
X <- X$Inflow

Y=read_excel("DATA_PENELITIAN.xlsx",sheet = "Outflow")

# Analisis Deskriptif
summary(X)
summary(Y$Outflow)

model.tsr=lm(Y$Outflow~X)
summary(model.tsr)
fits.tsr=model.tsr$fitted.values
res.tsr=model.tsr$residuals

fore.tsr=predict(model.tsr,data.frame(X))
Box.test(res.tsr,lag=6)
Box.test(res.tsr,lag=12)
Box.test(res.tsr,lag=18)
Box.test(res.tsr,lag=24)
Box.test(res.tsr,lag=36)
ks.test(res.tsr,"pnorm",mean=mean(res.tsr),sd=sd(res.tsr))

library(FinTS)
hasil.simulasi=matrix(0,12,2)
colnames(hasil.simulasi)=c('chi-sq','p-value')
for (i in 1:12){
  LM.simulasi=ArchTest(res.tsr,lags=i)
  hasil.simulasi[i,1]=LM.simulasi$statistic
  hasil.simulasi[i,2]=LM.simulasi$p.value
}
hasil.simulasi

#ARIMA
library(forecast)
library(lmtest)
library(tseries)
library(foreign)
library(TSA)

#load data & view series
plot.ts(Y$Outflow)

#Identifying arima
tsdisplay(Y$Outflow)
ar1<- arima(Y$Outflow, order = c(3,1,3))
ar1

tsdisplay(ar1$residuals)

plot.ts(Y$Outflow)
lines(Y$Outflow-ar1$residuals, col="red")
legend("topright",c("Outflow","ARIMA FV"),col=1:2,lty=1,ncol=1,cex=0.8)

##Arima with transfer from bush$s11
mod.2=arimax(Y$Outflow, order=c(3,1,3), xtransf=Y$M, transfer=list(c(1,0)))
summary(mod.2)

##Now plotting the data and the intervention model (only)
plot.ts(Y$Outflow)
lines(Y$Outflow-ar1$residuals, col="blue")
lines(Y$Outflow-mod.2$residuals, col="green")
legend("topright",c("Outflow","ARIMA FV","ARIMAX FV"),col=c("black","blue","green"),lty=1,ncol=1,cex= 0.8)
pred <- Y$Outflow-mod.2$residuals

# ARIMAX-QR
library(quantreg)
arimax.qr=rq(Y$Outflow~pred,c(0.025,0.05,0.50,0.95,0.975))
summary(arimax.qr,se="iid")
resiarimax.qr=residuals(arimax.qr)
fittedarimax.qr=fitted(arimax.qr)

plot.ts(Y$Outflow)
lines(fittedarimax.qr[,1], col="red")
lines(fittedarimax.qr[,2], col="green")
lines(fittedarimax.qr[,3], col="yellow")
lines(fittedarimax.qr[,4], col="blue")
lines(fittedarimax.qr[,5], col="purple")
legend("topright",c("Outflow","ARIMAX-QR 0.025","ARIMAX-QR 0.05",
                    "ARIMAX-QR 0.5","ARIMAX-QR 0.95","ARIMAX-QR 0.975"),col=c("black","red","green","yellow","blue","purple"),lty=1,ncol=1,cex= 0.8)

et.in.arimaxqr=(abs(Y$Outflow-fittedarimax.qr))^2
pt.in.arimaxqr=abs(100*et.in.arimaxqr/Y$Outflow)
RMSE.in.arimaxqr=sqrt(mean(et.in.arimaxqr))
MdAE.in.arimaxqr=median(et.in.arimaxqr)
MdAPE.in.arimaxqr=median(pt.in.arimaxqr)
hasil.arimaxqr=cbind(RMSE.in.arimaxqr,MdAE.in.arimaxqr,MdAPE.in.arimaxqr)
hasil.arimaxqr

# QRNN
ytrain <- Y$Outflow[1:round(0.8*length(Y$Outflow),0)]
ytest <- Y$Outflow[(round(0.8*length(Y$Outflow),0)+1):length(Y$Outflow)]

xtrain <- pred[1:round(0.8*length(pred),0)]
xtest <- pred[(round(0.8*length(pred),0)+1):length(pred)]

library(qrnn)
RMSE.in=matrix(0,15,1)
MAE.in=matrix(0,15,1)
MdAE.in=matrix(0,15,1)
MAPE.in=matrix(0,15,1)
MdAPE.in=matrix(0,15,1)
RMSE.out=matrix(0,15,1)
MAE.out=matrix(0,15,1)
MdAE.out=matrix(0,15,1)
MAPE.out=matrix(0,15,1)
MdAPE.out=matrix(0,15,1)
for (i in 1:15){
  set.seed(123)
  w.qrnn=qrnn.fit(as.matrix(xtrain), as.matrix(ytrain),n.hidden = i, tau =0.5, iter.max = 1000,n.trials = 1,
                  lower = 0)
  fits.qrnn=qrnn.predict(x = as.matrix(xtrain), w.qrnn)
  et.in=abs(ytrain-fits.qrnn)
  pt.in=abs(100*et.in/ytrain)
  et2.in=et.in^2
  RMSE.in[i]=sqrt(mean(et2.in))
  MAE.in[i]=mean(et.in)
  MdAE.in[i]=median(et.in)
  MAPE.in[i]=mean(pt.in)
  MdAPE.in[i]=median(pt.in)
  fore.qrnn=qrnn.predict(x = as.matrix(xtest), parms = w.qrnn)
  et.out=abs(ytest-fore.qrnn)
  pt.out=abs(100*et.out/ytest)
  et2.out=et.out^2
  RMSE.out[i]=sqrt(mean(et2.out))
  MAE.out[i]=mean(et.out)
  MdAE.out[i]=median(et.out)
  MAPE.out[i]=mean(pt.out)
  MdAPE.out[i]=median(pt.out)
}
hasil.in=cbind(RMSE.in,MAE.in,MdAE.in,MAPE.in,MdAPE.in)
colnames(hasil.in) <- c("RMSE","MAE","MdAE","MAPE","MdAPE")
rownames(hasil.in) <- c(1:15)
hasil.in
writexl::write_xlsx(data.frame(hasil.in),"Hasil QRNN In-Sample.xlsx")

hasil.out=cbind(RMSE.out,MAE.out,MdAE.out,MAPE.out,MdAPE.out)
colnames(hasil.out) <- c("RMSE","MAE","MdAE","MAPE","MdAPE")
rownames(hasil.out) <- c(1:15)
hasil.out
writexl::write_xlsx(data.frame(hasil.out),"Hasil QRNN Out-Sample.xlsx")


