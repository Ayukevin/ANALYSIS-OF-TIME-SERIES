library(TSA)
library(tseries)
## 6.30
### (a)
n=100
theta = 0.4
phi = 0.8

acf_theory <- ARMAacf(ar = c(phi), ma = c(theta), lag.max = 30)

par( mfrow= c(1,2))
plot(0:30, acf_theory, type = "h", lwd = 2,
     xlab = "Lag", ylab = "Theoretical ACF",
     main = "Theoretical ACF")

### (b)
Yt =  arima.sim(n = n, list(ar = c(phi), ma =c(theta)), sd = 1)
acf(Yt, lag.max = 30, main = "Simulated sample ACF")

### (c)
eacf(Yt, ar.max = 7, ma.max = 7)

### (e)
n = 48
### (f)
n = 200

## 6.35
### (a)
par( mfrow= c(1,1))
data(deere3)
plot(deere3)
acf(deere3)

plot(diff(deere3))#差分處理後看起來較為平穩
acf(diff(deere3))

plot(diff(diff(deere3)))
acf(diff(diff(deere3)))

### (b)
par(mfrow=c(1,2))
acf(deere3) ## MA(2)
pacf(deere3) ## AR(1)

## 6.36
data(robot)
### (a)
par(mfrow=c(1,1))
plot(robot)

adf.test(robot) #p-value =  0.01
kpss.test(robot) #p-value =  0.01

###(b)
par(mfrow=c(1,2))
acf(robot)
pacf(robot)

### (c)
eacf(robot, ar.max = 7, ma.max = 7)

### (d)
res = armasubsets(robot, nar=7, nma = 7, ar.method = 'ols') #ARIMA(0,1,2) 
plot(res)

## 6.39
data(days)
### (a)
par(mfrow=c(1,1))
plot(days)

adf.test(days) #p-value =  0.01
kpss.test(days)

###(b)
par(mfrow=c(1,2))
acf(days)
pacf(days)

### (c)
### replace the unusual values with 35
days[63] = 35 #origin: 55
days[106] = 35 #origin: 49
days[129] = 35 #origin: 63
plot(days)
par(mfrow=c(1,2))
acf(days)
pacf(days)

res = armasubsets(days, nar=7, nma = 7, ar.method = 'ols') #ARIMA(0,1,2) 
plot(res)

eacf(days)

