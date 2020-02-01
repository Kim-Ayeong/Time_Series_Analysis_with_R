setwd("C:/workspace/R/4-1)시계열 분석")

df <- read.csv("동작구 평균 대기오염도.csv")
head(df)

series <- ts(df$NO2, frequency = 365, start = c(2018, 10))
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
