library(quantmod)
library(TSA)

getSymbols("JNJ", src = "yahoo", from = "2024-01-01", to = "2025-01-01")
## Note that if you want to include the day of 2015-12-31, you need to set the next day, i.e., 2016-01-01.

data = JNJ
day.data = data[,6]
n1 = dim(day.data)[1]
head(data)
data[n1] # the last data

weekly.data <- to.period(data, period = "weeks")
weekly.data = weekly.data[,6]
n2 = dim(weekly.data)[1]
head(weekly.data)

#(a) time series plot
plot(day.data, ylab ='stock price')
plot(weekly.data, ylab ='stock price')

#(b) box-cox tranformation
res1 = BoxCox.ar(day.data, lambda = seq(-2,6,0.01)) #λ=0.53
res2 = BoxCox.ar(weekly.data, lambda = seq(-6,8,0.01)) #λ=1.95

#(c) transformed values
trans1 = (day.data^0.53-1)/0.53
par(mfrow = c(1,2))
plot(trans1, main= 'day data,lanbda = 1.95')
acf(trans1)

trans2 = (weekly.data^1.95-1)/1.95
par(mfrow = c(1,2))
plot(trans2, main= 'week data,lanbda = 0.53')
acf(trans2)

#(d) difference
diff1 = diff(trans1)
diff1 = diff1[!is.na(diff1$JNJ.Adjusted), ]#差分處立後，第一筆值會是NA
head(diff1)
plot(diff1, main= 'day data,lanbda = 1.95') 
acf(diff1)

diff2 = diff(trans2)
plot(diff2, main= 'week data,lanbda = 0.53')
diff2 = diff2[!is.na(diff1$JNJ.Adjusted), ]
acf(diff2)




