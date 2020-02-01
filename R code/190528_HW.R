#linear data
setwd("C:/workspace/R/4-1)시계열 분석")
df1 <- read.csv("경제활동 인구.csv")
head(df1)

ts_df1 <- ts(df1$y_hat, frequency = 12, start = c(2012, 1))
t1 <- time(ts_df1)
plot(ts_df1)

acf(ts_df1, lag.max = 20)
pacf(ts_df1, lag.mas = 20)

library(forecast)
auto.arima(diff(log(ts_df1))) #arima(p,d,q)
fit1 <- arima(log(ts_df1), c(0, 0, 2))
fit1


#quadratic data
df2 <- read.csv("수도권 아파트 매매가.csv")
head(df2)

ts_df2 <- ts(df2$y_hat, frequency = 12, start = c(2017, 5))
t2 <- time(ts_df2)
plot(ts_df2)

acf(ts_df2, lag.max = 20)
pacf(ts_df2, lag.mas = 20)

auto.arima(diff(log(ts_df2))) #arima(p,d,q)
fit2 <- arima(log(ts_df2), c(0, 1, 0))
fit2
