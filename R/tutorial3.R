# Date: Sept 29, 2016
# Author: Nathan Esau
# Description: Code for Stat 485 Tutorial (Seasonality)

# Seasonal Models

t <- 1:12
tt <- 2 * t

seasons <- c(10, 20, 30, 40, 10, 20, 30, 40, 10, 20, 30, 40)
data <- tt + seasons

model1 <- lm(data ~ 0 + as.factor(seasons))
model1 # print model1 info

model2 <- lm(data ~ 0 + time(data) + as.factor(seasons))
model2 # print model2 info

library(TSA) # install.packages("TSA")
data("beersales")

s <- season(beersales)

model0 <- lm(beersales ~ 0 + season(beersales))
model0

model1 <- lm(beersales ~ 0 + time(beersales) + season(beersales))
model1 # print model1 info

# predict 2016 sep beersale : 0.1683 * 2016 + (-319.6267)

pred <- 0.1683 * 2016 - 319.6267
print(pred)

model1_predict <- model1$fitted.values
model0_predict <- model0$fitted.values

plot.ts(y = beersales, x = time(beersales), type = 'l')
lines(y = model0_predict, x = time(beersales), type = 'l', col = 'blue')
lines(y = model1_predict, x = time(beersales), type = 'l', col = 'red')

# model 1 seems to be more accurate to the original data