## Time series decomposition 
fit <- stl(AirPassengers, s.window = 12)
fit
plot(fit)
plot(fit$time.series[,1])

## moving averages
# install.packages("forecast")
library(forecast)
ma(AirPassengers, order = 12)
plot(AirPassengers)
lines(ma(AirPassengers, order = 12), col = "red")

## Easy way to do forecasting

fit <- auto.arima(AirPassengers)
pred <- predict(fit, n.ahead  = 36)
U <- pred$pred + 2*pred$se        # upper limit 95% confidence interval 
L <- pred$pred - 2*pred$se        # lower limit 95% confidence interval
ts.plot(AirPassengers, pred$pred, U, L, 
        col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), 
       col=c(1,2,4), lty=c(1,1,2), bty = 'n')


#### try with Eurostockmarket
data("EuStockMarkets")


## Stationary Series
## A series whose statistical properties such as mean, variance, autocorrelation,
## are also constant over time
set.seed(100)
e <- rnorm(1000)
plot(e, type = "l")
start <- 20
end <- start + 300
y <- e[start:end]
mean(y)
sd(y)

## Non-stationary series
set.seed(100)
e <- rnorm(1000)
x <- e
for (i in 3:1000){
  x[i] <- 1.5*x[i-1] - 0.5*x[i-2] + e[i]
}
plot(x, type = "l")

start <- 700
end <- start + 300
y <- x[start:end]
mean(y)
sd(y)


## Ways to generate stationary series 
# 1
set.seed(200)
e <- rnorm(1000)
x <- e
for (i in 2:1000){
  x[i] <- 0.8*x[i-1] + e[i]
}

plot(x, type = "l")
acf(x)
pacf(x)

# 2
set.seed(200)
e <- rnorm(1000)
x <- e
theta1 <- 0.95
for (i in 2:1000) {
  x[i]<- e[i] + theta1*e[i-1]
}

plot(x, type = "l")

pacf(x)
acf(x)
theta1/(1 + theta1^2)


## Forecasting using ARIMA model
## a) Making the series stationary

plot(AirPassengers)
plot(log(AirPassengers))
plot(diff(log(AirPassengers), lag = 12))
v1 <- diff(log(AirPassengers), lag = 12)
plot(v1)
v2 <- diff(v1)
plot(v2)

## test to check if the series is stationary
##install.packages("tseries")
library(tseries)
kpss.test(AirPassengers)
kpss.test(log(AirPassengers))
kpss.test(v1)
kpss.test(v2)

## Extract the parameter
acf(v2)
pacf(v2)

## Fit the data to arima model using the extracted parameter
fit <- arima(log(AirPassengers), order=c(0,1,1), 
             seasonal = list(order=c(0,1,1), period=12))

## Make the forcast
pred <- predict(fit, n.ahead = 3*12)
exp(pred$pred)
ts.plot(AirPassengers, exp(pred$pred), col=c(1,2), lty = c(1,1))

## Find the upper and lower limit
pred <- predict(fit, n.ahead = 3*12) 
U <- pred$pred + 2*pred$se        # upper limit 95% confidence interval 
L <- pred$pred - 2*pred$se        # lower limit 95% confidence interval

ts.plot(AirPassengers, exp(pred$pred), exp(U), exp(L), 
        col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), 
       col=c(1,2,4), lty=c(1,1,2), bty = 'n')




