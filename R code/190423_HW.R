#Regression Model Forecasting

#Yt = b0 + b1*t
#et iid ~ N(0, sigma^2)
#E(Yt) = b0 + b1*t + E(et) = b0 + b1*t
#V(Yt) = v(et) = sigma^2

#Yt+1 = b0 + b1*t+1
#Yt+1 ~(asymtotal)~ N( E(Yt+1), V(Yt+1) )
#E(Yt+1) = b0 + b1*(t+1)
#V(Yt+1) = sigma^2
#terminal : E(Yt+1) +- 1.96*sqrt(V(Yt+1))

setwd("C:/workspace/R/4-1)시계열 분석")

#data1
df1 <- read.csv("경제활동 인구.csv")
head(df1)

ts_df1 <- ts(df1$y_hat, frequency = 12, start = c(2000, 1))
t1 <- time(ts_df1)

model1 <- lm(ts_df1 ~ time(ts_df1)); summary(model1)
plot(ts_df1); abline(model1, col="red")

#forecast를 위한 residual 분석
plot(y = rstudent(model1), x = as.vector(time(ts_df1)),
     xlab = 'Time', ylab = 'Standardized Residuals', 
     type='l')
#해석 : 평균(0) 주의에 랜덤하게 분포되어 있어야함
hist(rstudent(model1), xlab = 'Standardized Residuals')
#해석 : 중심으로부터 대칭?
qqnorm(rstudent(model1)); qqline(rstudent(model1), col="red")
#해석 : 직선과 비슷한 모양일수록 정규성을 만족함
acf(rstudent(model1))
#해석 : 

#forecast
E_Yt <- (-2.293e+04) + (1.146e+01)*t1[length(t1)]; E_Yt
E_Ytt <- (-2.293e+04) + (1.146e+01)*(t1[length(t1)] + (t1[length(t1)]-t1[length(t1)-1])); E_Ytt
V_Yt <- V_Ytt <- (12.61)*(12.61); V_Ytt #model1의 Residual standard error : 12.61
terminal <- c(E_Ytt - (1.96)*sqrt(V_Ytt), E_Ytt + (1.96)*sqrt(V_Ytt)); terminal



#data2
df2 <- read.csv("국내 총생산.csv")
head(df2)

ts_df2 <- ts(df2$y_hat, frequency = 4, start = c(2000, 1))
t2 <- time(ts_df2)

model2 <- lm(ts_df2 ~ time(ts_df2)); summary(model2)
plot(ts_df2); abline(model2, col="red")

#forecast를 위한 residual 분석
plot(y = rstudent(model2), x = as.vector(time(ts_df2)),
     xlab = 'Time', ylab = 'Standardized Residuals', 
     type='l')
#해석 : 평균(0) 주의에 랜덤하게 분포되어 있어야함
hist(rstudent(model2), xlab = 'Standardized Residuals')
#해석 : 중심으로부터 대칭?
qqnorm(rstudent(model2)); qqline(rstudent(model2), col="red")
#해석 : 직선과 비슷한 모양일수록 정규성을 만족함
acf(rstudent(model2))
#해석 : 

#forecast
E_Yt <- (-7.989e+03) + (3.995e+00)*t2[length(t2)]; E_Yt
E_Ytt <- (-7.989e+03) + (3.995e+00)*(t2[length(t2)] + (t2[length(t2)]-t2[length(t2)-1])); E_Ytt
V_Yt <- V_Ytt <- (1.127)*(1.127); V_Ytt #model1의 Residual standard error : 1.127
terminal <- c(E_Ytt - (1.96)*sqrt(V_Ytt), E_Ytt + (1.96)*sqrt(V_Ytt)); terminal



#data3
df3 <- read.csv("실업률.csv")
head(df3)

ts_df3 <- ts(df3$y_hat, frequency = 12, start = c(2012, 1))
t3 <- time(ts_df3)

model3 <- lm(ts_df3 ~ time(ts_df3)); summary(model3)
plot(ts_df3); abline(model3, col="red")

#forecast를 위한 residual 분석
plot(y = rstudent(model3), x = as.vector(time(ts_df3)),
     xlab = 'Time', ylab = 'Standardized Residuals', 
     type='l')
#해석 : 평균(0) 주의에 랜덤하게 분포되어 있어야함
hist(rstudent(model3), xlab = 'Standardized Residuals')
#해석 : 중심으로부터 대칭?
qqnorm(rstudent(model3)); qqline(rstudent(model3), col="red")
#해석 : 직선과 비슷한 모양일수록 정규성을 만족함
acf(rstudent(model3))
#해석 : 

#forecast
E_Yt <- (-2.337e+02) + (1.177e-01)*t3[length(t3)]; E_Yt
E_Ytt <- (-2.337e+02) + (1.177e-01)*(t3[length(t3)] + (t3[length(t3)]-t3[length(t3)-1])); E_Ytt
V_Yt <- V_Ytt <- (0.1606)*(0.1606); V_Ytt #model1의 Residual standard error : 0.1606
terminal <- c(E_Ytt - (1.96)*sqrt(V_Ytt), E_Ytt + (1.96)*sqrt(V_Ytt)); terminal

