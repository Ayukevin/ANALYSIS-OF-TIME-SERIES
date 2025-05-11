library(TSA)
library(tseries)

## Augmented Dickey-Fuller test
n=500
et = rnorm(n)
adf.test(et)

yt = c(et[1])
for(i in 2:n){
yt = c(yt, yt[i-1] + et[i])
}
adf.test(yt)
kpss.test(yt)
adf.test(diff(yt))
kpss.test(diff(yt))

# dataset = color
data(color)

par(mfrow=c(3,1))
plot(color, type = "l")
acf(color)
pacf(color)
par(mfrow=c(1,1))
acf(color,ci.type='ma')
#wrong: an error due to the inadequate lags
eacf(color)
#correct: add the limit
eacf(color, ar.max = 7, ma.max = 7)

## data set = hare
win.graph(width=4, height=4,pointsize=8)
data(hare)
plot(hare, type = "l")
par(mfrow=c(2,1))
acf(hare)
pacf(hare)

par(mfrow=c(1,1))
bxh=BoxCox.ar(hare)
bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.
par(mfrow=c(3,1))
plot(hare^.5, type = "l")
acf(hare^.5)
pacf(hare^.5)
eacf(hare^.5) # an error due to the inadequate lags
eacf(hare^.5, ar.max = 7, ma.max = 7)

## dataset = oil.price
data(oil.price)
par(mfrow=c(1,1))
plot(oil.price, type = 'o')
BoxCox.ar(oil.price)
BoxCox.ar(oil.price,method = "ols",lambda = seq(-0.5, 0.5, 0.01))
plot(log(oil.price), type = 'o')
adf.test(log(oil.price))
kpss.test(log(oil.price))
yt = diff(log(oil.price))
adf.test(yt)
plot(yt)

par(mfrow=c(3,1))
plot(yt)
acf(yt)
pacf(yt)
eacf(yt)

#???
par(mfrow=c(1,1))
res = armasubsets(yt, nar=7, nma = 7, ar.method = 'ols')
plot(res)

