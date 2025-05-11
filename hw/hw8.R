library(TSA)
set.seed(123)
# 9_3
data(tempdub)
tempdub1=ts(c(tempdub,rep(NA,12)),start=start(tempdub),freq=frequency(tempdub)) 
plot(tempdub1)

#(a)
har.=harmonic(tempdub,1)
m5.tempdub=arima(tempdub,order=c(0,0,0),xreg=har.)
m5.tempdub

newhar.=harmonic(ts(rep(1,12), start=c(1976,1),freq=12),1)
plot(m5.tempdub,n.ahead=12,n1=c(1972,1),newxreg=newhar.,
     type='b',ylab='Temperature',xlab='Year')
result = predict(m5.tempdub,n.ahead=12,newhar.) 
result$pred[4]

#(b)
forecast_april_1976 = result$pred[4]
sd_resid = 3.719  # from title
ub = forecast_april_1976 + 1.96*sd_resid
lb = forecast_april_1976 - 1.96*sd_resid
c(lb,ub)

#(c)
har1977 <- harmonic(ts(rep(1, 12), start = c(1977, 1), freq = 12), 1)
har2009 <- harmonic(ts(rep(1, 12), start = c(2009, 1), freq = 12), 1)

pred1977 <- predict(m5.tempdub, n.ahead = 12, newxreg = har1977)
pred2009 <- predict(m5.tempdub, n.ahead = 12, newxreg = har2009)

forecast_april_1977 <- pred1977$pred[4]
forecast_april_1977 # 44.09622
forecast_april_2009 <- pred2009$pred[4]
forecast_april_2009 # 44.09622

# 9_8
data(electricity)
plot(electricity)

#(a)
log_elec = log(electricity)
t = time(electricity)

month_factors = factor(cycle(electricity))  # 12 months
fit <- lm(log_elec ~ t + month_factors)

summary(fit)

#(b)
# take the last five years of the series
start_year <- end(log_elec)[1] - 4  # 最近 5 年的起始年
end_year <- end(log_elec)[1]
log_last5 <- window(log_elec, start = c(start_year, 1))

future_time <- time(ts(rep(NA, 24), start = c(end_year + 1, 1), freq = 12))
#季節性資料加上月份factor
future_months <- factor(cycle(ts(rep(1, 24), start = c(end_year + 1, 1), freq = 12)))
newdata <- data.frame(t = future_time, month_factors = future_months)
pred <- predict(fit, newdata = newdata, interval = "prediction", level = 0.95)

# 合併時間序列
ts_pred <- ts(pred[, "fit"], start = c(end_year + 1, 1), freq = 12)
ts_lower <- ts(pred[, "lwr"], start = c(end_year + 1, 1), freq = 12)
ts_upper <- ts(pred[, "upr"], start = c(end_year + 1, 1), freq = 12)

# 將 log 轉換回原始單位
ts_pred_orig <- exp(ts_pred)
ts_lower_orig <- exp(ts_lower)
ts_upper_orig <- exp(ts_upper)
ts_last5_orig <- exp(log_last5)


ts_combined <- ts(c(ts_last5_orig, ts_pred_orig),
                  start = start(ts_last5_orig), freq = 12)


plot(ts_combined, col = "black", lwd = 2,
     ylim = range(ts_lower_orig, ts_upper_orig),
     ylab = "Electricity Generation", xlab = "Year",
     main = "Electricity Forecast with Seasonal Trend + 95% PI")


lines(ts_pred_orig, col = "blue", lwd = 2)
lines(ts_lower_orig, col = "blue", lty = 2)
lines(ts_upper_orig, col = "blue", lty = 2)


legend("topleft",
       legend = c("Observed (last 5 years)", "Forecast", "95% Prediction Interval"),
       col = c("black", "blue", "blue"), lty = c(1,1,2), bty = "n")



# 9_13
mu = 100
phi1= 0.7
theta= -0.5
n = 50
Yt =  arima.sim(n = n, list(ar = phi1, ma = theta), sd = 1) + (1-phi1)*mu

#(b)
train.Yt = Yt[1:40]
test.Yt = Yt[41:50]
plot(Yt)

fit = arima(train.Yt, order = c(1, 0, 1))
forecast_values <- predict(fit, n.ahead = 10)
pred_mean <- forecast_values$pred
pred_se <- forecast_values$se

plot(Yt, col = "black", lwd = 2,
     xlab = "Time", ylab = "Value", main = "ARMA(1,1) Process with Forecast")
lines(41:50, pred_mean, col = "blue", type = "o", pch = 16)  # 預測值


# 加入平均線
abline(h = mean(train.Yt), col = "red", lty = 2, lwd = 2)

legend("bottomleft", legend = c("Actual", "Forecast", "process mean"),
       col = c("black", "blue", "red"), 
       lty = c(1, 1, 2), pch = c(NA, 16, NA), bty = "n")

#(c) Compare the 12 forecasts with the actual values that you set aside
actual <- test.Yt
forecasted <- pred_mean

comparison <- data.frame(
  Time = 41:50,
  Actual = round(actual, 2),
  Forecast = round(forecasted, 2),
  Error = round(actual - forecasted, 2)
)

print(comparison)

#(d)Plot the forecasts together with 95% forecast limits

upper <- forecasted + 1.96 * pred_se
lower <- forecasted - 1.96 * pred_se


lines(41:50, upper, col = "red", lty = 2)
lines(41:50, lower, col = "red", lty = 2)
abline(h = mean(train.Yt), col = "gray", lty = 3)

legend("bottomleft", legend = c("Actual", "Forecast","process mean","95% Interval"),
       col = c("black", "blue",'gray', "red"), lty = c(1, 1, 2,2), 
       pch = c(16, 17, NA, NA), bty = "n")





# 9_15
theata = 0.8
theata1 = 10
n = 35
Yt =  arima.sim(n = n, list(order = c(0,1,1), ma= c(theata1)), sd = 1) + theata

train.Yt = Yt[1:30]
test.Yt = Yt[31:35]
plot(Yt)

fit <- arima(train.Yt, order = c(0, 1, 1))
forecast_values <- predict(fit, n.ahead = 5)

pred_mean <- forecast_values$pred
pred_se <- forecast_values$se

plot(Yt, col = "black", lwd = 2,
     xlab = "Time", ylab = "Value", main = "IMA(1,1) Process with Forecast")
lines(31:35, pred_mean, col = "blue", type = "o", pch = 16)  # 預測值


# 加入平均線
abline(h = mean(train.Yt), col = "red", lty = 2, lwd = 2)

legend("bottomleft", legend = c("Actual", "Forecast", "estimation of mean"),
       col = c("black", "blue", "red"), 
       lty = c(1, 1, 2), pch = c(NA, 16, NA), bty = "n")

#(c) Compare the 12 forecasts with the actual values that you set aside
actual <- test.Yt
forecasted <- pred_mean

comparison <- data.frame(
  Time = 31:35,
  Actual = round(actual, 2),
  Forecast = round(forecasted, 2),
  Error = round(actual - forecasted, 2)
)

print(comparison)

#(d)Plot the forecasts together with 95% forecast limits

upper <- forecasted + 1.96 * pred_se
lower <- forecasted - 1.96 * pred_se


lines(31:35, upper, col = "red", lty = 2)
lines(31:35, lower, col = "red", lty = 2)
abline(h = mean(train.Yt), col = "gray", lty = 3)

legend("topleft", legend = c("Actual", "Forecast","estimation of mean","95% Interval"),
       col = c("black", "blue",'gray', "red"), lty = c(1, 1, 2,2), 
       pch = c(16, 17, NA, NA), bty = "n")
