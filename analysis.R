setwd("C:/Users/nesau/stat485/")

#library(TSA)
library(forecast)

copper <- read.csv("copper.csv", header = TRUE, sep=",")
names(copper) <- c("Year", "Price")

copperTS <- copper$Price
plot(copperTS, type = 'l')

acf(copperTS)

y <- diff(copperTS)
plot(y, type='l')
acf(y)
TSA::eacf(y)

# it suggests a white noise model 
auto.arima(y)

aicWN <- arima(y, order=c(0,0,0), include.mean=FALSE)$aic
aicARMA <- arima(y, order=c(2,0,1), include.mean=FALSE)$aic

aicWN
aicARMA
arima(y, order=c(2,0,1), include.mean=FALSE)
arima(y, order=c(1,0,1), include.mean=FALSE)
arima(y, order=c(0,0,0), include.mean=FALSE)

model1 <- arima(y, order=c(2,0,1), include.mean=FALSE)

css.phi1 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="CSS")$coef[1]
css.phi2 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="CSS")$coef[2]
css.theta1 <- -arima(y, order=c(2,0,1), include.mean=FALSE, method="CSS")$coef[3]

mle.phi1 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")$coef[1]
mle.phi2 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")$coef[2]
mle.theta1 <- -arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")$coef[3]

css.sigma2 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="CSS")$sigma2
mle.sigma2 <- arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")$sigma2

ParameterEstimates <- data.frame(Method = c("CSS", "ML"),
                                 Phi1 = c(css.phi1, mle.phi1),
                                 Phi2 = c(css.phi2, mle.phi2),
                                 Theta1 = c(css.theta1, mle.theta1),
                                 Sigma2 = c( css.sigma2, mle.sigma2))

ParameterEstimates

output <- arima(y, order=c(2,0,1), include.mean=FALSE, method='ML')
confint(output)

residuals <- arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")$residuals

# plot time series of residuals
plot(residuals, type = 'l')
acf(residuals)

qqnorm(residuals, ylim=c(-5,5))
qqline(residuals) # residuals look ok

# forecast
model <- arima(y, order=c(2,0,1), include.mean=FALSE, method="ML")

plot(forecast(model, h = 15))