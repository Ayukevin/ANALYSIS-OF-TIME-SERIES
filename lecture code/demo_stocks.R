install.packages("quantmod")
library(quantmod)

getSymbols("JNJ", src = "yahoo", from = "2015-01-01", to = "2016-01-01")
## Note that if you want to include the day of 2015-12-31, you need to set the next day, i.e., 2016-01-01.

data1 = JNJ
n1 = dim(data1)[1]
head(data1)
data1[n1]

weekly.data1 <- to.period(data1, period = "weeks")
head(weekly.data1)
