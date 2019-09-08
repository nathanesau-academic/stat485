# Bowley's data on values of British and Irish trade, 1855-1899

rm(list=ls())

options(warn=-1)
setwd("C:/Users/nesau/stat485/statproj/")

tradeDF <- read.csv("Bowley.csv", header=TRUE)
tradeTS <- tradeDF$Value

plot(tradeTS, type = 'l') # doesn't look stationary

# try taking diff
y <- diff(tradeTS)
plot(y, type='l')

# see if it's stationary
library(forecast)

# Find out which model to use
# Method 1: auto.arima function
auto.arima(y) # suggests MA(1) based on AIC
# let's try these models
# MA(1)
# AR(1)
# ARMA(1,1)

trade.ma <- arima(y, order=c(0,0,1), include.mean=FALSE)
trade.ar <- arima(y, order=c(1,0,0), include.mean=FALSE)
trade.arma <- arima(y, order=c(1,0,1), include.mean=FALSE)

# In report have a section on model selection - show AIC and EACF 

# compare AIC = 2k - 2lnL (where k is number of parameters). MA(1) has the smallest AIC00
ModelAIC <- data.frame(Model = c("MA", "AR", "ARMA"), AIC = c(trade.ma$aic, trade.ar$aic, trade.arma$aic))
ModelAIC

# use EACF to select model - suggests MA(1) model
TSA::eacf(y)

# In report show ACF and PACF - it looks stationary
acf(y)
pacf(y)

plot(y, type = 'l') # it looks like white noise

# let's say we select MA(1) model
model <- arima(y, order = c(0,0,1), include.mean=FALSE)

# In report, probably what you want is a table is a table of estimates
# using:
# (1) Method Moments
# (2) Conditional Sum Squares (Least Squares)
# (3) Unconditional Sum Squares (ML)

r1 <- TSA::acf(y, plot=FALSE)$acf[1]
mom.theta <- (-1 + sqrt(1 - 4*r1^2))/(2*r1)
css.theta <- -arima(y, order=c(0,0,1), include.mean=FALSE, method="CSS")$coef
mle.theta <- -arima(y, order=c(0,0,1), include.mean=FALSE, method="ML")$coef

mom.sigma2 <- var(y)/(1+mom.theta^2)
css.sigma2 <- arima(y, order=c(0,0,1), include.mean=FALSE, method="CSS")$sigma2
mle.sigma2 <- arima(y, order=c(0,0,1), include.mean=FALSE, method="ML")$sigma2

ParameterEstimates <- data.frame(Method = c("Moments", "CSS", "ML"),
                                 Theta = c(mom.theta, css.theta, mle.theta),
                                 Sigma2 = c(mom.sigma2, css.sigma2, mle.sigma2))

# After getting estimates is to residual analysis
# use Maximum likelhood estimates
theta <- mle.theta
sigma2 <- mle.sigma2
sigma <- sqrt(sigma2) # use this for simulation

residuals <- arima(y, order=c(0,0,1), include.mean=FALSE, method="ML")$residuals

# plot time series of residuals
plot(residuals, type = 'l')
acf(residuals)

qqnorm(residuals, ylim=c(-35, 35))
qqline(residuals) # residuals look ok

# for forecasting, use forecast package
model <- arima(y, order=c(0,0,1), include.mean=FALSE, method="ML")

forecast(model, h = 15) # Project for 1900, 1901, ..., 1914

# You can directly plot the forecast
plot(forecast(model, h = 15))

# but this is a forecast of the diff
# This code below is confusing but point is that we want to get confidence interval
# not of diff but of actual TS

df <- as.data.frame(forecast(model, h = 15))

predMean <- df$`Point Forecast` + tail(tradeDF$Value, 1)
predLow95 <- df$`Lo 95` + tail(tradeDF$Value, 1)
predHigh95 <- df$`Hi 95` + tail(tradeDF$Value, 1)

meanTS <- c(tradeDF$Value, predMean); meanTS[1:45] <- NA
lowTS <- c(tradeDF$Value, predLow95); lowTS[1:45] <- NA
highTS <- c(tradeDF$Value, predHigh95); highTS[1:45] <- NA

yNew <- c(tradeDF$Value, rep(NA, 15))

# plot of forecast
plot(yNew, type = 'l', ylim = c(0, 300), ylab = "Trade")
lines(meanTS, type = 'l', col = 'green')
lines(lowTS, type = 'l', col = 'red')
lines(highTS, type = 'l', col = 'blue')

abline(v=46, type='l', lty=2)

legend('bottomright', c("Mean pred", "95% Upper", "95% Lower"),
       col = c('green', 'blue', 'red'), lty = c(1,1,1))

# Simulation
# plot of what future trade could look like

# Model: Yt = et - theta * et-1

set.seed(1000)

plot(yNew, type = 'l', ylim=c(0, 305))

for(i in 1:10)
{ 
  x <- c(rep(NA, 45), as.numeric(simulate(model, nsim=15)) + tail(tradeDF$Value, 1))
  lines(x, col=i)
}