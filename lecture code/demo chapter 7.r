library(TSA)
library(tseries)

## Simulate AR(1) with mean mu
n = 200
et = rnorm(n+1)
mu=1
phi = 0.5
yt=c(et[1]/sqrt(1-phi^2) + mu)

for(i in 2:(n+1)){

yt=c(yt,mu + phi*(yt[i-1]-mu) + et[i])

}

par(mfrow=c(3,1))
plot(yt,type='o')
acf(yt,ci.type='ma')
pacf(yt)

ar(yt,order.max=1,method='yw') # method of moments
ar(yt,order.max=1,method='ols') # conditional sum of squares
ar(yt,order.max=1,method='mle') # maximum likelihood



## Simulate AR(2) with mean 0
n = 200

et = rnorm(n+1)
var(et)
phia = 1.3
phib = -0.7

c2 = phia/(1-phib)
c1 = 1/sqrt(1-c2^2)

wt=c(c1*et[1])
wt=c(wt,c2*wt[1] + et[2])

for(i in 3:(n+1)){
wt=c(wt, phia*wt[i-1] + phib*wt[i-2]  + et[i])
}

par(mfrow=c(3,1))
plot(wt, type = "l")
acf(wt)
pacf(wt)

ar(wt,order.max=2,method='yw') # method of moments
ar(wt,order.max=2,method='ols') # conditional sum of squares
ar(wt,order.max=2,method='mle') # maximum likelihood

arima(wt,order=c(2,0,0),method='CSS',include.mean=F)
arima(wt,order=c(2,0,0),method='ML',include.mean=F)

## Simulate MA(1) with mean 0

n = 200

et = rnorm(n+1)
theta1 = 0.7

yt=c()


for(i in 2:(n+1)){
yt=c(yt, et[i]-theta1*et[i-1])
}

par(mfrow=c(3,1))
plot(yt, type = "l")
acf(yt)
pacf(yt)



acflag1=(acf(yt, plot=FALSE)$acf)[2]

(-1+sqrt(1-4*acflag1^2))/(2*acflag1)
(-1-sqrt(1-4*acflag1^2))/(2*acflag1)

arima(yt,order=c(0,0,1),method='CSS-ML',include.mean=F)
arima(yt,order=c(0,0,1),method='CSS',include.mean=F)
arima(yt,order=c(0,0,1),method='ML',include.mean=F)


# Exhibit 7.7
data(color)
par(mfrow=c(3,1))
plot(color, type = "l")
acf(color)
pacf(color)

par(mfrow=c(1,1))
acf(color,ci.type='ma')
eacf(color)# an error due to the inadequate lags
eacf(color, ar.max = 7, ma.max = 7)

arima(color,order=c(0,0,2))# remove lag 2
arima(color,order=c(0,0,1))
arima(color,order=c(1,0,0))


# Exhibit 7.8
win.graph(width=4, height=4,pointsize=8)
data(hare)
plot(hare, type = "l")
par(mfrow=c(2,1))
acf(hare)
pacf(hare)
arima(hare,order=c(6,0,0))

par(mfrow=c(1,1))
bxh=BoxCox.ar(hare)
bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.


par(mfrow=c(3,1))
plot(sqrt(hare), type = "l")
acf(sqrt(hare))
pacf(sqrt(hare))

arima(sqrt(hare),order=c(6,0,0))
arima(sqrt(hare),order=c(3,0,0))
arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA))


# Exhibit 7.9
data(oil.price)
par(mfrow=c(2,1))
plot(oil.price, type = 'o')
acf(oil.price)
BoxCox.ar(oil.price)
bxh=BoxCox.ar(oil.price,method = "ols",lambda = seq(-0.5, 0.5, 0.01))
bxh$mle
bxh$ci 

log.oil=log(oil.price)

par(mfrow=c(3,1))
plot(log.oil, type = "l")
acf(log.oil)
pacf(log.oil)

diff.log.oil=diff(log(oil.price))

par(mfrow=c(3,1))
plot(diff.log.oil, type = "l")
acf(diff.log.oil)
pacf(diff.log.oil)

arima(diff.log.oil,order=c(0,1,1)) #incorrect lag
arima(diff.log.oil,order=c(2,1,0)) #incorrect lag


arima(log(oil.price),order=c(0,1,1)) 
arima(log(oil.price),order=c(2,1,0))





