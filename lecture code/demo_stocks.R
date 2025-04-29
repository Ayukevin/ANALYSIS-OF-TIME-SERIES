install.packages("quantmod")
library(quantmod)

getSymbols("JNJ", src = "yahoo", from = "2024-01-01", to = "2025-01-01")
## Note that if you want to include the day of 2015-12-31, you need to set the next day, i.e., 2016-01-01.

data1 = JNJ
n1 = dim(data1)[1]
head(data1)
data1[n1]
write.csv(data1, file = "JNJ2024stock.csv", row.names = FALSE)


weekly.data1 <- to.period(data1, period = "weeks")
head(weekly.data1)

setwd(file_path)
df = read.csv('JNJ2024stock.csv',header = TRUE)
getwd()
df
plot(df)
