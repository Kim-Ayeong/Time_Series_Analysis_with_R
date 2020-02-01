#0409

rm(list=ls())

#random walk model : 5 sample path
Y <- 0
e <- rnorm(100, mean=0, sd=1)
for (i in 2:length(e)){
  Y[i] <- Y[i-1] + e[i]
}
yrange <- c(-30, 30)
par(new=TRUE)
ts.plot(Y, main="Random Walk Model", xlab="", ylab="", ylim=yrange, col=5)

difrange = c(-3, 3)
par(new=TRUE)
ts.plot(diff(Y), main="white noise", xlab="", ylab="", ylim=difrange, col=5) #white noise

#moving average model : 5 sample path
e <- rnorm(100, mean=0, sd=1)
e <- rnorm(100, mean=-5, sd=2)
e <- rnorm(100, mean=5, sd=2)
Y <- c()
for (i in 2:length(e)) {
  Y[i] <- e[i-1]/2 + e[i]/2
}
yrange <- c(-10, 10)
par(new=TRUE)
ts.plot(Y, main="Moving Average model", xlab="", ylab="", ylim=yrange, col=5)

