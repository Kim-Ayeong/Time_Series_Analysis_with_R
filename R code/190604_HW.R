setwd("C:/workspace/R/4-1)시계열 분석")

#ARMA(1,1) series with φ=0.7, θ=0.4, and n=72.
set.seed(54321)
series = arima.sim(n=72,list(ar=0.7,ma=-0.4))
head(series)
plot(series)

#method of moments
acf(series)$acf[1:2]
#conditional least squares estimates
arima(series,order=c(1,0,1),method='CSS')
#maximum likelihood estimates
arima(series,order=c(0,0,1),method='ML')

acf(series)
pacf(series)

library(forecast) #arima(p,d,q)
auto.arima(series)
tsdiag(auto.arima(series))
