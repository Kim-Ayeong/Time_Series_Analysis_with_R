rm(list=ls())
library(forecast)

data1 <- read.csv("C:/workspace/R/시계열 분석/생활물가지수.csv")
data2 <- read.csv("C:/workspace/R/시계열 분석/실업률.csv")
data3 <- read.csv("C:/workspace/R/시계열 분석/코스피.csv")
str(data1); str(data2); str(data3) #데이터 파악

tsmodel1 <- ts(data1$생활물가지수, start=c(2012,1), frequency=12) #2012년 1월 시작, 총 12월로 분류
tsmodel1 #확인
plot(tsmodel1, main="생활물가지수", xlab="연도", ylab="지표")

tsmodel2 <- ts(data2$실업률, start=c(2012,1), frequency=12)
tsmodel2 #확인
plot(tsmodel2, main="실업률", xlab="연도", ylab="지표")

tsmodel3 <- ts(data3$코스피, start=c(2012,1), frequency=12)
tsmodel3 #확인
plot(tsmodel3, main="코스피(종가지수)", xlab="연도", ylab="지표")

