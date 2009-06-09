require(robKalman)

##  AO model:
set.seed(361)
Eps <- as.ts(rnorm(100))
ar2 <- arima.sim(list(ar = c(1, -0.9)), 100, innov = Eps)
Binom <- rbinom(100, 1, 0.1)
Noise <- rnorm(100,sd = 10)
y <- ar2 + as.ts(Binom*Noise)

y.arGM <- arGM(y, 3)
y.ACMfilt <- ACMfilt(y, y.arGM)

plot(y)
lines(y.ACMfilt$filt, col=2)
lines(ar2,col="green")
