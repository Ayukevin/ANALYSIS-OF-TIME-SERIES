# model: AR(2) with mu =100
mu =100
phi1 = 1.5
phi2 = -0.75
n= 52

set.seed(123)
# (b) forecast the next 12 values and plot
Yt =  arima.sim(n = n, model = list(ar = c(phi1, phi2)), sd = 1) + (1-phi1-phi2)*mu
train.Yt = Yt[1:40]
test.Yt = Yt[41:52]

fit = arima(train.Yt, order = c(2, 0, 0))
forecast_values <- predict(fit, n.ahead = 12)
pred_mean <- forecast_values$pred
pred_se <- forecast_values$se

plot(Yt, col = "black", lwd = 2,
        xlab = "Time", ylab = "Value", main = "AR(2) Process with Forecast")
lines(41:52, pred_mean, col = "blue", type = "o", pch = 16)  # 預測值


# 加入平均線
abline(h = mean(train.Yt), col = "red", lty = 2, lwd = 2)

legend("topleft", legend = c("Actual", "Forecast", "Mean"),
       col = c("black", "blue", "red"), 
       lty = c(1, 1, 2), pch = c(NA, 16, NA), bty = "n")

#(c) Compare the 12 forecasts with the actual values that you set aside
actual <- test.Yt
forecasted <- pred_mean

comparison <- data.frame(
  Time = 41:52,
  Actual = round(actual, 2),
  Forecast = round(forecasted, 2),
  Error = round(actual - forecasted, 2)
)

print(comparison)

#(d)Plot the forecasts together with 95% forecast limits

upper <- forecasted + 1.96 * pred_se
lower <- forecasted - 1.96 * pred_se


lines(41:52, upper, col = "red", lty = 2)
lines(41:52, lower, col = "red", lty = 2)
abline(h = mean(train.Yt), col = "gray", lty = 3)

legend("topleft", legend = c("Actual", "Forecast","processed mean","95% Interval"),
       col = c("black", "blue",'gray', "red"), lty = c(1, 1, 2,2), 
       pch = c(16, 17, NA, NA), bty = "n")
