e <- rnorm(100)
plot(e, main="Moving Average", 
     xlab="t", ylab="Yt", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="black")

#MA(1)
theta <- -0.9
MA_1 <- array(e[1])
for (i in 2:length(e)) {
  MA_1[i] <- e[i] - theta*e[i-1]
}
par(new=T)
plot(MA_1, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="red")

#시차=1 일 때, 상관계수
row1 = -theta/(1+theta*theta) #0.497
library(TSA)
plot(y=MA_1, x=zlag(MA_1),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
acf(MA_1)

#시차=2 일 때, 상관계수(=0)
plot(y=MA_1,x=zlag(MA_1, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)

#MA(2)
theta1 <- 1
theta2 <- -0.6
MA_2 <- array(e[1:2])
for (i in 3:length(e)) {
  MA_2[i] <- e[i] - theta1*e[i-1] - theta2*e[i-2]
}
par(new=T)
plot(MA_2, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="green")

#시차=1 일 때, 상관계수
row1 <- (-theta1+theta1*theta2)/(1+theta1*theta1+theta2*theta2) #-0.678
plot(y=MA_2, x=zlag(MA_2),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
acf(MA_2)

#시차=2 일 때, 상관계수
row2 <- -theta2/(1+theta1*theta1+theta2*theta2) #0.254
plot(y=MA_2,x=zlag(MA_2, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)

#시차=3 일 때, 상관계수(=0)
plot(y=MA_2,x=zlag(MA_2, 3),
     xlab=expression(Y[t-3]), ylab=expression(Y[t]), type='p', pch=16)

#MA(5)
theta3 <- 0.3
theta4 <- -0.2
theta5 <- 0.1
MA_5 <- array(e[1:5])
for (i in 6:length(e)) {
  MA_5[i] <- e[i] - theta1*e[i-1] - theta2*e[i-2] - theta3*e[i-3]
  - theta4*e[i-4] - theta5*e[i-5]
}
par(new=T)
plot(MA_5, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="blue")

#시차=1 일 때, 상관계수
tmp <- (1+theta1*theta1+theta2*theta2+theta3*theta3+theta4*theta4+theta5*theta5)
row1 <- (-theta1+theta1*theta2+theta2*theta3+theta3*theta4+theta4*theta5)/tmp #-0.744
plot(y=MA_5, x=zlag(MA_5),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
acf(MA_5)
#시차=2 일 때, 상관계수
row2 <- (-theta2+theta2*theta3+theta3*theta4+theta4*theta5)/tmp #0.136
plot(y=MA_5,x=zlag(MA_5, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)

#시차=3 일 때, 상관계수
row3 <- (-theta3+theta3*theta4+theta4*theta5)/tmp #-0.152
plot(y=MA_5,x=zlag(MA_5, 3),
     xlab=expression(Y[t-3]), ylab=expression(Y[t]), type='p', pch=16)

#시차=4 일 때, 상관계수
row4 <- (-theta4+theta4*theta5)/tmp #0.072
plot(y=MA_5,x=zlag(MA_5, 4),
     xlab=expression(Y[t-4]), ylab=expression(Y[t]), type='p', pch=16)

#시차=5 일 때, 상관계수
row5 <- -theta5/tmp #-0.04
plot(y=MA_5,x=zlag(MA_5, 5),
     xlab=expression(Y[t-5]), ylab=expression(Y[t]), type='p', pch=16)

#시차=6 일 때, 상관계수(=0)
plot(y=MA_5,x=zlag(MA_5, 6),
     xlab=expression(Y[t-6]), ylab=expression(Y[t]), type='p', pch=16)
