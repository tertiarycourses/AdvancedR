################################################################################################################### 
#                                             Time Series Basics
###################################################################################################################

## using the Nile dataset
data(Nile)
print(Nile)
plot(Nile, xlab="Year", ylab="River Volume", main="Annual river volumme", type="b")   # can change "b" to "l"

start(Nile)  # start of measurements (time) > in this case year 1871 > can be observed with print(Nile)
end(Nile)    # last observation (time) > in this case year 1970  
frequency(Nile)  # how many observations oer unit time
deltat(Nile)     # observation time increment (fixed time interval between observations)
time(Nile)       # vector of tie indices
cycle(Nile)     # position of the cycle of each observation


## using the AirPassengers dataset
data(AirPassengers)
print(AirPassengers)
plot(AirPassengers, type="b")
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
deltat(AirPassengers)
time(AirPassengers)
cycle(AirPassengers)

#####################################################################################################################
#                                         Time Series Object
#####################################################################################################################

is.ts() # check if time series object

data=rnorm(50,1,2.5)
print(data)
plot(data, type="l")

time_series_quarterly=ts(data, start=2004, frequency=4)  # by specifying frequency > we put data in quarters
print(time_series_quarterly)
plot(time_series_quarterly)

time_series_monthly=ts(data, start=2004, frequency=12)  # by specifying frequency > we put data in months
print(time_series_monthly)
plot(time_series_monthly)
ts.plot(time_series_monthly)


####### European Stock Market
data(EuStockMarkets)
start(EuStockMarkets)
end(EuStockMarkets)
frequency(EuStockMarkets)

### see difference between plot and ts.plot

plot(EuStockMarkets)

ts.plot(EuStockMarkets, col=1:4, xlab="Year", ylab="Index Value",
        main="Major European Stocks from 1991-1998")

#### trend splotting

# use log transformation > linearize rapid growth trends
# use diff() > remove linear trends > changes in obserations
# use diff( ,lag=s) > remove seasonal differencing

dimnames(EuStockMarkets)
dax=EuStockMarkets[ ,1]  #> isolate just the dax time series
smi=EuStockMarkets[ ,2]
cac=EuStockMarkets[ ,3]
ftse=EuStockMarkets[ ,4]

ts.plot(dax)
plot(decompose(dax))

linear_dax=log(dax);ts.plot(linear_dax); length(linear_dax)
dz_dax=diff(dax); ts.plot(dz_dax); length(dz_dax)
dz_dax_s4=diff(dax,lag=4);ts.plot(dz_dax_s4); length(dz_dax_s4)  
dz_dax_s12=diff(dax,lag=12);ts.plot(dz_dax_s12); length(dz_dax_s12)
# we put 4 for quartely and 12 for monthly seasonal


##################### log returns #################################
dax_logReturns=diff(log(dax))
ts.plot(dax_logReturns)

##################### analysis plots ############################

stock=dax   # define our stock, we can change later

plot(decompose(stock))  # plot1

par(mfrow=c(2,2))
ts.plot(log(stock), main="log transform");ts.plot(diff(stock), main="difference");
ts.plot(diff(stock, lag=4), main="quaterly diff");ts.plot(diff(stock, lag=12), main="monhtly diff")

#################### asset pricing vs returns

### working with stock prices
ts.plot(cbind(dax,smi), col=c("red","blue"))     
plot(dax,smi, col=c("red","blue")) # see strong relationship in prices
### working with log returns
ts.plot(cbind(diff(log(dax)), diff(log(smi))), col=c("red","blue"))
plot(diff(log(dax)),diff(log(smi)), col=c("red","blue"))   # see strong relationship in log returns


### working with percent returns
eu_percent_returns=ts((EuStockMarkets[-1,]/EuStockMarkets[-1860,]-1)*100, 
                      start=c(1991,130),
                      frequency=260)

colMeans(eu_percent_returns)  # generate mean returns for each stock
apply(eu_percent_returns,2,var)
apply(eu_percent_returns,2,sd) # this is the risk


par(mfrow=c(2,2))  # plot histogram for each stock > percent returns
apply(eu_percent_returns, MARGIN=2, FUN=hist, main="Percent retruns")
# plot qqplot for each stock > percent returns
apply(eu_percent_returns, MARGIN=2, FUN=qqnorm, main="Percent retruns")
qqline(eu_percent_returns)


### plotting pairs of data
plot(EuStockMarkets)
pairs(EuStockMarkets)  # scatterplot matrix of prices

plot(diff(log(EuStockMarkets)))
pairs(diff(log(EuStockMarkets)))  # scatterplot matrix of returns


################## correlation and autocorrelation

#correlation and covariance are very impt for finance
mean(EuStockMarkets[1,]) # average price of DAX
mean(diff(log(EuStockMarkets[1,])))  # measures returns of DAX
sd(EuStockMarkets[1,])   #> measures risk of DAX
cor(EuStockMarkets)

# autocorrelation > study how each observation realted to recent past
#                 > more predictable
#  lag=1 cor=0.89  > if yesterday is high, today is likely to be high and viceversa
#  lag=2 cor=0.77  > if 2 days ago is high, today is also likely to be high

acf(dax, plot=FALSE)  # give the autocorrelation matrix across a series of days
acf(dax, plot=TRUE)

acf(dax, lag.max=10, plot=FALSE)  # define how many periods with lag.max


##########################################################################################
#                                   MODELS
##########################################################################################

##################### White Noise Model ############################################
     # stable stationary
     # over time > a fixed constant mean, constant variance, no correlation over time
     # no clear pattern
     # c(0,0,0)  > refers to white noise model

### simulation WN model
white_noise1=arima.sim(model=list(order=c(0,0,0)),n=100)
ts.plot(white_noise1)

white_noise2=arima.sim(model=list(order=c(0,0,0)),n=100, mean=120, sd=10)
ts.plot(white_noise2)

# > take note these are just simulations

### fit white noise model to our data (dax)

WNdax=arima(dax, order=c(0,0,0))
mean(dax)
var(dax)
WN_fitted=dax-residuals(WNdax)
ts.plot(dax)
points(WN_fitted, type="l", col="red", lty=2)

####################### Random Walk Model #######################################
        # unstable, non-stationary
        # over time > no specifice mean or variance, strong dependence over time,
        #            > its changes or increments are white noise (stable, stationary)
        # c(0,1,0)  > refers to randomw walk model

### simulation RW model
par(mfrow=c(2,2))
random_walk1=arima.sim(model=list(order=c(0,1,0)),n=100)
ts.plot(random_walk1)
random_walk1_diff=diff(random_walk1)
ts.plot(random_walk1_diff)
model_wn1=arima(random_walk1_diff, order=c(0,0,0)) # fit WN model to diff data
int_model_wn1=model_wn1$coef
ts.plot(random_walk1); abline(0, int_model_wn1)


random_walk2=arima.sim(model=list(order=c(0,1,0)),n=100, mean=1)
ts.plot(random_walk2)
random_walk2_diff=diff(random_walk2)
ts.plot(random_walk2_diff)
model_wn2=arima(random_walk2_diff, order=c(0,0,0))
int_model_wn2=model_wn2$coef
ts.plot(random_walk2); abline(0, int_model_wn2)

### fit random walk model to our data (dax)

RWdax=arima(dax, order=c(0,1,0))
mean(dax)
var(dax)
RW_fitted=dax-residuals(RWdax)
ts.plot(dax)
points(RW_fitted, type="l", col="red", lty=2)

######################## autoregressive model #######################################

# todays observation is regressed on yesterday's observation
# c(1,0,0)  > refers to AR model

### simulation AR model
x=arima.sim(model=list(ar=0.5), n=100)
y=arima.sim(model=list(ar=0.9), n=100)
z=arima.sim(model=list(ar=-0.75), n=100)

plot.ts(cbind(x,y,z))

acf(x)
acf(y)
acf(z)

a=arima.sim(model=list(ar=0.9), n=100)
b=arima.sim(model=list(ar=0.98), n=100)
c=arima.sim(model=list(order=c(0,0,0)),n=100)  # stimulate and plot WN model
d=arima.sim(model=list(order=c(0,1,0)),n=100)  # stimulate and plot RW model

plot.ts(cbind(a,b,c,d))

acf(a)
acf(b)
acf(c)
acf(d)

### fit AR model to our data (AirPassengers)
data(AirPassengers)  # doesn't work with dax
ARair=arima(AirPassengers, order=c(1,0,0))
AR_fitted=AirPassengers-residuals(ARair)
ts.plot(AirPassengers)
points(AR_fitted, type="l", col="red", lty=2)


### fit AR model to our data (Nile)
data(Nile)  
ARnile=arima(Nile, order=c(1,0,0))
AR_fitted=Nile-residuals(ARnile)
ts.plot(Nile)
points(AR_fitted, type="l", col="red", lty=2)

######### forecasting with AR model
ARnile=arima(Nile, order=c(1,0,0))
predict_AR=predict(ARnile)   # make first step forecast
predict_AR$pred[1]
predict(ARnile, n.ahead=10)   # predict 10 steps ahead


ARnile=arima(Nile, order=c(1,0,0))
# plot Nile with 95% prediction intervals
ts.plot(Nile, xlim=c(1871,1980)) #> leave some space for prediction
AR_forecast_pred=predict(ARnile, n.ahead=10)$pred
AR_forecast_se=predict(ARnile, n.ahead=10)$se
points(AR_forecast_pred, type="l", col="blue")
points(AR_forecast_pred - 2*AR_forecast_se, type="l", col="red", lty=2)
points(AR_forecast_pred + 2*AR_forecast_se, type="l", col="red", lty=2)


################### Simple Moving Average Model #####################

# todays observation is regressed on yesterday's noise
# c(0,0,1)  > refers to SMA model

### simulation SMA model
x=arima.sim(model=list(ma=0.5), n=100)
y=arima.sim(model=list(ma=0.9), n=100)
z=arima.sim(model=list(ma=-0.5), n=100)

plot.ts(cbind(x,y,z))

acf(x)
acf(y)
acf(z)

### fit SMA model to our data (Nile)
SMAnile=arima(Nile, order=c(0,0,1))
SMA_fitted=Nile-residuals(SMAnile)
ts.plot(Nile)
points(SMA_fitted, type="l", col="red", lty=2)


######### forecasting with SMA model
SMAnile=arima(Nile, order=c(0,0,1))
predict_SMA=predict(SMAnile)   # make first step forecast
predict_SMA$pred[1]
predict(SMAnile, n.ahead=10)   # predict 10 steps ahead


SMAnile=arima(Nile, order=c(0,0,1))
# plot Nile with 95% prediction intervals
ts.plot(Nile, xlim=c(1871,1980)) #> leave some space for prediction
SMA_forecast_pred=predict(SMAnile, n.ahead=10)$pred
SMA_forecast_se=predict(SMAnile, n.ahead=10)$se
points(SMA_forecast_pred, type="l", col="blue")
points(SMA_forecast_pred - 2*SMA_forecast_se, type="l", col="red", lty=2)
points(SMA_forecast_pred + 2*SMA_forecast_se, type="l", col="red", lty=2)

######################### comparing AR and SMA models ###########################

# AIC / BIC  > lower value indicates a better fitting model

cor(AR_fitted, SMA_fitted)

AIC(ARnile)
BIC(ARnile)

AIC(SMAnile)
BIC(SMAnile)


###################################################################################
#                                   SUMMARY
###################################################################################

data(EuStockMarkets)   # lets go back to euroStocksMarket dataset
eu=EuStockMarkets
dax=eu[ ,1]  #> isolate just the dax time series
smi=eu[ ,2]
cac=eu[ ,3]
ftse=eu[ ,4]

############################# basic plots
ts.plot(dax)
plot(decompose(dax))

par(mfrow=c(2,2))
linear_dax=log(dax);ts.plot(linear_dax); length(linear_dax)
dz_dax=diff(dax); ts.plot(dz_dax); length(dz_dax)
dz_dax_s4=diff(dax,lag=4);ts.plot(dz_dax_s4); length(dz_dax_s4)  
dz_dax_s12=diff(dax,lag=12);ts.plot(dz_dax_s12); length(dz_dax_s12)
# we put 4 for quartely and 12 for monthly seasonal

############################# returns & risk
ts.plot(diff(log(dax)))
### working with percent returns
eu_percent_returns=ts((eu[-1,]/eu[-1860,]-1)*100, 
                      start=c(1991,130),
                      frequency=260)

colMeans(eu_percent_returns)  # generate mean returns for each stock
apply(eu_percent_returns,2,var)
apply(eu_percent_returns,2,sd) # this is the risk

par(mfrow=c(2,2))  # plot histogram for each stock > percent returns
apply(eu_percent_returns, MARGIN=2, FUN=hist, main="Percent retruns")
# plot qqplot for each stock > percent returns
apply(eu_percent_returns, MARGIN=2, FUN=qqnorm, main="Percent retruns")
qqline(eu_percent_returns)

############################# plotting pairs of data
plot(eu)
pairs(eu)  # scatterplot matrix of prices

plot(diff(log(eu)))
pairs(diff(log(eu)))  # scatterplot matrix of returns

########################### autocorrelation
acf(dax, lag.max=10)

########################## model selection

### fit WN model to our data (dax)
WNdax=arima(dax, order=c(0,0,0))
WN_fitted=dax-residuals(WNdax)

RWdax=arima(dax, order=c(0,1,0))
RW_fitted=dax-residuals(RWdax)

ARdax=arima(dax, order=c(1,0,0))
AR_fitted=dax-residuals(ARdax)

SMAdax=arima(dax, order=c(0,0,1))
SMA_fitted=dax-residuals(SMAdax)

# plot each model with data
ts.plot(dax)
points(WN_fitted, type="l", col="red", lty=2)
points(RW_fitted, type="l", col="blue", lty=2)
points(AR_fitted, type="l", col="green", lty=2)
points(SMA_fitted, type="l", col="pink", lty=2)

AIC(WNdax);AIC(RWdax);AIC(ARdax);AIC(SMAdax)
BIC(WNdax);BIC(RWdax);BIC(ARdax);BIC(SMAdax)

#### > find the best model that fits and use that to predict >>



### fit WN model to our data (dax)
WNdax=arima(Nile, order=c(0,0,0))
WN_fitted=Nile-residuals(WNdax)

RWdax=arima(Nile, order=c(0,1,0))
RW_fitted=Nile-residuals(RWdax)

ARdax=arima(Nile, order=c(1,0,0))
AR_fitted=Nile-residuals(ARdax)

SMAdax=arima(Nile, order=c(0,0,1))
SMA_fitted=Nile-residuals(SMAdax)

# plot each model with data
ts.plot(Nile)
points(WN_fitted, type="l", col="red", lty=2)
points(RW_fitted, type="l", col="blue", lty=2)
points(AR_fitted, type="l", col="green", lty=2)
points(SMA_fitted, type="l", col="pink", lty=2)

AIC(WNdax);AIC(RWdax);AIC(ARdax);AIC(SMAdax)
BIC(WNdax);BIC(RWdax);BIC(ARdax);BIC(SMAdax)



