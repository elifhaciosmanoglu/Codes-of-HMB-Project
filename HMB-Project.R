data2 <- read_excel("C:/Users/Elif Hacıosmanoğlu/Desktop/data2.xlsx")
attach(data2)

tsrgdp<-ts(GDP,start=1970,freq=1)
tsfdi<-ts(FDI,start=1970,freq=1)
tsdomcredit<-ts(Domcredit,start=1970,freq=1)
tstrade<-ts(Trade,start=1970,freq=1)
tsgovspending<-ts(Finalconsumption,start=1970,freq=1)



logtsrgdp=log(tsrgdp)



dlogtsrgdp=diff(logtsrgdp)
dtsfdi=diff(tsfdi)
dtsdomcredit=diff(tsdomcredit)
dtstrade=diff(tstrade)
dtsgovspending=diff(tsgovspending)

plot(logtsrgdp)
plot(tsfdi)
plot(tsdomcredit)
plot(tstrade)
plot(tsgovspending)

acf(logtsrgdp)
acf(tsfdi)
acf(tsdomcredit)
acf(tstrade)
acf(tsgovspending)

acf(dlogtsrgdp)
acf(dtsfdi)
acf(dtsdomcredit)
acf(dtstrade)
acf(dtsgovspending)


summary(ur.df(logtsrgdp,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(logtsrgdp,type="drift",lags=3,selectlags=c("AIC")))
summary(ur.df(logtsrgdp,type="trend",lags=3,selectlags=c("AIC")))

summary(ur.df(dlogtsrgdp,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(dlogtsrgdp,type="drift",lags=3,selectlags=c("AIC")))

summary(ur.df(tsfdi,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(tsfdi,type="drift",lags=3,selectlags=c("AIC")))
summary(ur.df(tsfdi,type="trend",lags=3,selectlags=c("AIC")))

summary(ur.df(dtsfdi,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(dtsfdi,type="drift",lags=3,selectlags=c("AIC")))

summary(ur.df(tsdomcredit,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(tsdomcredit,type="drift",lags=3,selectlags=c("AIC")))
summary(ur.df(tsdomcredit,type="trend",lags=3,selectlags=c("AIC")))

summary(ur.df(dtsdomcredit,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(dtsdomcredit,type="drift",lags=3,selectlags=c("AIC")))

summary(ur.df(tstrade,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(tstrade,type="drift",lags=3,selectlags=c("AIC")))
summary(ur.df(tstrade,type="trend",lags=3,selectlags=c("AIC")))

summary(ur.df(dtstrade,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(dtstrade,type="drift",lags=3,selectlags=c("AIC")))

summary(ur.df(tsgovspending,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(tsgovspending,type="drift",lags=3,selectlags=c("AIC")))
summary(ur.df(tsgovspending,type="trend",lags=3,selectlags=c("AIC")))

summary(ur.df(dtsgovspending,type="none",lags=3,selectlags=c("AIC")))
summary(ur.df(dtsgovspending,type="drift",lags=3,selectlags=c("AIC")))






realreg=cbind(logtsrgdp,tsfdi,tsdomcredit,tstrade,tsgovspending)
VARselect(realreg,lag.max=4,type="const")
cointest<-ca.jo(realreg,K=2,type="eigen",ecdet="const")

summary(cointest)
model1=VAR(data.frame(dlogRGDP,dlogRFDI,dlogRCAPFORMT),type="const",lag.max=4,ic="AIC")
summary(model1)
model2=VECM(data.frame(logtsrgdp,tsfdi,tsdomcredit,tstrade,tsgovspending),lag=1,r=2,estim=("ML"))
summary(model2)
model3var<-vec2var(cointest,r=2)
serial.test(model3var,lags.pt=4,type="PT.asymptotic")
arch.test(model3var,lags.multi=4,multivariate.only=TRUE)
normality.test(model3var,multivariate.only = TRUE)
stab1<-stability(model2,type="OLS-CUSUM")
plot(stab1)
forecast<-predict(model3var,n.ahead=10,ci=0.95)
fanchart(forecast,names="logtsrgdp",main="Fanchart for log(RGDP)",xlab="Horizon",ylab="log(RGDP)")
fanchart(forecast,names="tsfdi",main="Fanchart for log(RFDI)",xlab="Horizon",ylab="log(RFDI)")



fevd1<-fevd(model3var,n.ahead=7)
plot(fevd1)
ts.plot(RGDPCN,xlab="year")
grangertest(dlogRGDP~dlogRFDI,order=2)
grangertest(dlogRFDI~dlogRGDP,order=2)
GDPirf<-irf(model3var,impulse="tsfdi",response="logtsrgdp",n.ahead=3,boot=TRUE,runs=1000)
plot(GDPirf,ylab="log(GDP)",main="Effect of FDI on GDP")
FDIirf<-irf(model3var,impulse="logtsrgdp",response="tsfdi",n.ahead=3,boot=TRUE,runs=1000)
plot(FDIirf,ylab="FDI as share of GDP",main="Effect of GDP on FDI")



summary(ur.kpss(logRGDPT,type="tau",lags="short"))
summary(ur.kpss(dlogRGDPT,type="tau",lags="short"))
summary(ur.kpss(logRFDI,type="tau",lags="short"))
summary(ur.kpss(dlogRFDI,type="tau",lags="short"))
summary(ur.kpss(logRDMSTCRDTPRVTT,type="tau",lags="short"))
summary(ur.kpss(dlogRDMSTCRDTPRVTT,type="tau",lags="short"))
summary(ur.kpss(logRCAPFORMT,type="tau",lags="short"))
summary(ur.kpss(logRCAPFORMT,type="tau",lags="short"))

summary(ur.pp(logRGDPT,type="Z-tau",model="trend",lags="short"))
summary(ur.pp(dlogRGDPT,type="Z-tau",model="constant",lags="short"))

summary(ur.pp(logRFDI,type="Z-tau",model="trend",lags="short"))
summary(ur.pp(dlogRFDI,type="Z-tau",model="constant",lags="short"))

summary(ur.pp(logRDMSTCRDTPRVTT,type="Z-tau",model="trend",lags="short"))
summary(ur.pp(dlogRDMSTCRDTPRVTT,type="Z-tau",model="constant",lags="short"))

summary(ur.pp(logRCAPFORMT,type="Z-tau",model="trend",lags="short"))
summary(ur.pp(dlogRCAPFORMT,type="Z-tau",model="constant",lags="short"))


