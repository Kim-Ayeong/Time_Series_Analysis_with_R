e <- rnorm(100)
plot(e, main="Autoregressive", 
     xlab="t", ylab="Yt", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="black")

#AR(1)
theta <- -0.9
AR_1 <- array(e[1])
for (i in 2:length(e)) {
  AR_1[i] <- theta*AR_1[i-1] + e[i]
}
par(new=T)
plot(AR_1, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="red")

#time lag = 1
library(TSA)
plot(y=AR_1, x=zlag(AR_1),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 2
plot(y=AR_1,x=zlag(AR_1, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)

acf(AR_1)

#AR(2)
theta1 <- 1
theta2 <- -0.6
AR_2 <- array(e[1:2])
for (i in 3:length(e)) {
  AR_2[i] <- theta1*AR_2[i-1] + theta2*AR_2[i-2] + e[i]
}
par(new=T)
plot(AR_2, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="green")

#time lag = 1
plot(y=AR_2, x=zlag(AR_2),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 2
plot(y=AR_2,x=zlag(AR_2, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 3
plot(y=AR_2,x=zlag(AR_2, 3),
     xlab=expression(Y[t-3]), ylab=expression(Y[t]), type='p', pch=16)

acf(AR_2)

#AR(5)
theta3 <- 0.3
theta4 <- -0.2
theta5 <- 0.1
AR_5 <- array(e[1:5])
for (i in 6:length(e)) {
  AR_5[i] <- theta1*AR_5[i-1] + theta2*AR_5[i-2] + theta3*AR_5[i-3]
  + theta4*AR_5[i-4] + theta5*AR_5[i-5] + e[i] 
}
par(new=T)
plot(AR_5, xlab="", ylab="", xlim=c(0,100), ylim=c(-5,5), 
     type="l", col="blue")

#time lag = 1
plot(y=AR_5, x=zlag(AR_5),
     xlab=expression(Y[t-1]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 2
plot(y=AR_5,x=zlag(AR_5, 2),
     xlab=expression(Y[t-2]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 3
plot(y=AR_5,x=zlag(AR_5, 3),
     xlab=expression(Y[t-3]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 4
plot(y=AR_5,x=zlag(AR_5, 4),
     xlab=expression(Y[t-4]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 5
plot(y=AR_5,x=zlag(AR_5, 5),
     xlab=expression(Y[t-5]), ylab=expression(Y[t]), type='p', pch=16)
#time lag = 6
plot(y=AR_5,x=zlag(AR_5, 6),
     xlab=expression(Y[t-6]), ylab=expression(Y[t]), type='p', pch=16)

acf(AR_5)
