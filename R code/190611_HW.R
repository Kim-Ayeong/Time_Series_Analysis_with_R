setwd("C:/workspace/R/4-1)시계열 분석")
data <- read.csv("실업률.csv")

#1. data check
head(data)
tail(data)

ts_df <- ts(data$y, start=c(2012, 1), frequency=12)
plot.ts(ts_df, main="실업률", xlab="연도", ylab="지표") #nonstationary 확인

par(mfrow = c(1, 2))
acf(ts_df) #자기상관 확인
pacf(ts_df)

par(mfrow = c(1, 1))
ts_ss <- decompose(ts_df)
plot(ts_ss) #seasonal 확인

#2. Model Identification
#seasonal 제거
ts_nonss <- ts_df - ts_ss$seasonal 
plot.ts(ts_nonss, main="실업률", xlab="연도", ylab="지표")

#stationary 변환
#방법1. log transformation
ts_log <- log(ts_nonss)
plot.ts(ts_log, main="실업률", xlab="연도", ylab="지표")

#방법2. 차분(difference)
ts_dif1 <- diff(ts_log, differences = 1)
ts_dif2 <- diff(ts_log, differences = 2)
ts_dif3 <- diff(ts_log, differences = 3)

par(mfrow = c(1,3))
plot.ts(ts_dif1)
plot.ts(ts_dif2)
plot.ts(ts_dif3)

#방법3. 이동평균 > 이용X
library(TTR)
sma3 <- SMA(ts_nonss, n = 3)
sma6 <- SMA(ts_nonss, n = 6)
sma12 <- SMA(ts_nonss, n = 12)

par(mfrow = c(1,3))
plot.ts(sma3)
plot.ts(sma6)
plot.ts(sma12)

#adf.test로 최종 선택
library(tseries)
adf.test(ts_dif1, alternative = "stationary", k = 0)

ts_stat <- ts_dif1
par(mfrow = c(1,1))
plot.ts(ts_stat, main="실업률", xlab="연도", ylab="지표")

mean(ts_stat); sd(ts_stat) #평균, 표준편차 확인

#3. Parameter Estimation

par(mfrow = c(1, 2))
acf(ts_stat)  #MA(1)
pacf(ts_stat)  #AR(0)
fit1 <- arima(ts_stat, order = c(0,1,1))
fit1 #AIC : -284.21

#auto.arima 이용
auto.arima(ts_stat)
fit2 <- arima(ts_stat, order = c(0,0,1), seasonal = list(order = c(0,0,1), period = 12))
fit2 #AIC : -321.13 > 더 작으므로 fit2 선택

#4. validation

par(mfrow = c(1, 3))
plot(fit2$residuals)
hist(fit2$residuals)
acf(fit2$residuals)

#5. forecast

library(forecast)
auto.arima(tsmodel)
arima2 <- arima(tsmodel, order = c(0,1,1), seasonal = list(order = c(0,0,1), period = 12))
arima2
forecast2 <- forecast(arima2, h = 5)
forecast2
autoplot(forecast2)


