############################
## == winnebago DATASET== ## 
############################

#install.packages('tidyverse')
library(TSA)
data(winnebago)
winnebago

library(tidyverse)

#time series plot
yt = winnebago
plot(yt,type='o',ylab='y')

#least squares to fit a line to these data
model1 <- lm(yt~time(yt), data = yt)
summary(model1)
plot(yt,type='o',ylab='y')
abline(model1,col='red')

#standatdized residual plot
plot(y=rstudent(model1),x=as.vector(time(yt)),ylab='Standardized Residuals',
     xlab='Time',type='o')

#nature log of monthly sales
plot(y=log(yt),x=as.vector(time(yt)),ylab='nature log of monthly sales',
     xlab='Time',type='o')

#least squares to fit a line to the logged data
model_log = lm(log(yt)~time(yt))
summary(model_log)
plot(y=rstudent(model_log),x=as.vector(time(yt)),ylab='Standardized Residuals of ln(sales)',
     xlab='Time',type='o')

#least squares to fit a seasonal-means plus linear time trend to the logged sales time series
month = as.factor(cycle(yt)) 
model2 = lm(log(yt) ~ month + time(yt) -1)  # 加入時間趨勢
summary(model2)

#time series plot of the standardized residuals
plot(y=rstudent(model2),x=as.vector(time(yt)),
     ylab='nature log of standardized residuals',xlab='Time',type='o')

############################
## == prescrip DATASET== ## 
############################
data(prescrip)
prescrip
library(lattice)
library(pandoc)
#time series plot
xyplot(prescrip, ylab = "Prescription costs",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(prescrip)), col = 1)
       })

#the sequence of month-to-month percentage changes
pchange <- diff(prescrip) / prescrip
xyplot(pchange ~ time(prescrip), type = "l",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.xyplot(x, y, pch = as.vector(season(pchange)), col = 1)
       })

#least squares to fit a cosine trend with fundamental frequency 1/12 
t <- seq_along(pchange)  
pres_cos <- lm(pchange ~ cos(2 * pi * t / 12) + sin(2 * pi * t / 12))
summary(pres_cos)  

#the sequence of standardized residuals
xyplot(rstudent(pres_cos) ~ time(prescrip), type = "l")

