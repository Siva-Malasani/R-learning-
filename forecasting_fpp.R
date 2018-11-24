# Read the data from Excel into R
library(readxl)
mydata <- read_xlsx("exercise1.xlsx")
# https://storage.googleapis.com/dimensionless/Analytics/exercise1.xlsx
mydata <- read_xlsx("https://storage.googleapis.com/dimensionless/Analytics/exercise1.xlsx",sheet = 1)
write.csv(x = mydata,file = "exercise1.csv")
mydata <- read.csv("https://storage.googleapis.com/dimensionless/Analytics/exercise1.csv")
mydata[,1]<-NULL
# Look at the first few lines of mydata

head(mydata)

# Create a ts object called myts
myts <- ts(mydata[,2:4], start = c(1981, 1), frequency = 4)

# Plot the data with facetting
plot(myts)
plot.ts()
autoplot(myts, facets = TRUE)

# Plot the data without facetting
autoplot(myts, facets = FALSE)
# Plot the gold, woolyrnq, and gas time series in separate plots.
# Plot the three series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
ggseasonplot(woolyrnq)
ggseasonplot(gas)
# Find the outlier in the gold series
goldoutlier <- which.max(gold)

# Look at the seasonal frequencies of the three series
frequency(gold)
frequency(woolyrnq)
frequency(gas)

# Load the fpp2 package
library(fpp2)

# Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = T)

# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start=1992)

# Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)
ggseasonplot(beer)
# Create an autoplot of the oil data
autoplot(oil)

# Create a lag plot of the oil data
gglagplot(oil)

# Create an ACF plot of the oil data
ggAcf(oil)

# Plot the annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1 

# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)
decompose(hyndsight)
plot(decompose(hyndsight,type = "multi"))
# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7

# White noise
set.seed(3)
wn <-ts(rnorm(36))
autoplot(wn)
ggAcf(wn)

#Pigs data 
pigs_sub<-window(pigs,start=1990)
autoplot(pigs_sub/1000)
ggAcf(pigs_sub/1000)
Box.test(pigs_sub,lag=24,type = "Lj")

# Plot the original series
autoplot(goog)

# Plot the differenced series
autoplot(diff(goog))

# ACF of the differenced series
ggAcf(goog)
ggAcf(diff(goog))

# Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")

# Naive and Snaive
fc<-naive(oil)
autoplot(oil,series="Data")+autolayer(fitted(fc),series="Fitted")
autoplot(residuals(fc))
checkresiduals(fc)
# Use naive() to forecast the goog series
fcgoog <- naive(goog,20)

# Plot and summarize the forecasts
autoplot(fcgoog)
summary(fcgoog)

# Use snaive() to forecast the ausbeer series
autoplot(ausbeer)
fcbeer <- snaive(ausbeer,16)

# Plot and summarize the forecasts
autoplot(fcbeer)
summary(fcbeer)

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()

# Do they look like white noise (TRUE or FALSE)
googwn <- TRUE

# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer%>%snaive()%>%checkresiduals()

# Do they look like white noise (TRUE or FALSE)
beerwn <- FALSE
# Splitting into train and test 
training <- window(oil,end=2003)
test <- window(oil, start = 2004)
fc<- naive(training,h=10)
autoplot(fc)+autolayer(test,series="test")
accuracy(fc,test)
# Create the training data as train
train <- subset(gold, end = 1000)
length(train)
train
# Compute naive forecasts and save to naive_fc
naive_fc <- naive(train, h = 108)

# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)

# Use accuracy() to compute RMSE statistics
accuracy(naive_fc, gold)
accuracy(mean_fc, gold)
autoplot(naive_fc)
# Assign one of the two forecasts as bestforecasts
bestforecasts <- naive_fc

# Create three training series omitting the last 1, 2, and 3 years
library(fpp)
train1 <- window(vn[, "Melbourne"], end = c(2010, 4))
train2 <- window(vn[, "Melbourne"], end = c(2009, 4))
train3 <- window(vn[, "Melbourne"], end = c(2008, 4))

# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)

# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]

# Plotting 
autoplot(fc1)
autoplot(fc2)
autoplot(fc3)
# Cross Validation 
set.seed(50)
e<- tsCV(oil,forecastfunction = naive,h = 1)
MSE <- mean(e^2,na.rm = T)
MSE
sq<- function(u){u^2}
for(h in 1:10)
{
  oil%>%tsCV(forecastfunction=naive,h=h)%>%sq%>%mean(na.rm=T)%>%print()
}
# Compute cross-validated errors for up to 8 steps ahead
e <- matrix(NA,nrow = 1000, ncol = 8)
for (h in 1:8)
  e[, h] <- tsCV(goog, forecastfunction = naive, h = h)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = mse)) + geom_point()

qplot(seq(1:8),mse)
# Simple exponential smoothing
oildata <- window(oil,start=1996)
autoplot(oildata)
fc<- ses(oildata,h=5)
summary(fc)
autoplot(fc)
# Use ses() to forecast the next 10 years of winning times
autoplot(marathon)
fc <- ses(marathon, h = 10)

# Use summary() to see the model parameters
summary(fc)

# Use autoplot() to plot the forecasts
autoplot(fc)

# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))

# Create a training set using subset.ts()
train <- subset(marathon, end = length(marathon) - 20)

# Compute SES and naive forecasts, save to fcses and fcnaive
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)
autoplot(fcses)
autoplot(fcnaive)
# Calculate forecast accuracy measures
accuracy(fcses,marathon)
accuracy(fcnaive,marathon)

# Save the best forecasts as fcbest
fcbest <- fcnaive

# Holt's linear trend
autoplot(AirPassengers)
AirPassengers%>%holt(h=5)%>%autoplot()


# Produce 10 year forecasts of austa using holt()
autoplot(austa)
fcholt <- holt(austa,h=10)

# Look at fitted model using summary()
summary(fcholt)

# Plot the forecasts
autoplot(fcholt)

# Check that the residuals look like white noise
checkresiduals(fcholt)

# Damped Trend
fc1 <- holt(austa,h=15,PI=F)
fc2 <- holt(austa,h=15,damped = T,PI=F)
autoplot(austa)+autolayer(fc1,series="Linear")+autolayer(fc2,series = "damped")

# Exponential Smoothing methods with trend and seasonality
aust<- window(austourists,start=2005)
fc1<-HoltWinters(aust,seasonal = "additive")
fc2 <- HoltWinters(aust,seasonal = "mult")
autoplot(aust)+autolayer(forecast(fc1)$mean,series="Additive")+autolayer(forecast(fc2)$mean,series="Multiplicative")

# Plot the data
autoplot(a10)

# Produce 3 year forecasts
fc <- hw(a10, seasonal = "multiplicative", h = 36)

# Check if residuals look like white noise
checkresiduals(fc)
whitenoise <- FALSE

# Plot forecasts
autoplot(fc)

# Create training data with subset()
autoplot(hyndsight)
train <- subset(hyndsight, end = length(hyndsight)-28)

# Holt-Winters additive forecasts as fchw
fchw <- hw(train, seasonal ="additive", h = 28)

# Seasonal naive forecasts as fcsn
fcsn <- snaive(train,h=28)

# Find better forecasts with accuracy()
accuracy(fchw,hyndsight)
accuracy(fcsn, hyndsight)

# Plot the better forecasts
autoplot(fchw)

# ETS models 
autoplot(ausair)
ets(ausair)
ausair%>%ets()%>%forecast()%>%autoplot()

# Monthly steroid drug sales
autoplot(h02)
ets(h02)
h02%>%ets()%>%forecast()%>%autoplot()


# Fit ETS model to austa in fitaus
autoplot(austa)
fitaus <- ets(austa)

# Check residuals
checkresiduals(fitaus)

# Plot forecasts
autoplot(forecast(fitaus))

# Repeat for hyndsight data in fiths
fiths <- ets(hyndsight)
checkresiduals(fiths)
autoplot(forecast(fiths))

# Which model(s) fails test? (TRUE or FALSE)
fitausfail <- FALSE
fithsfail <- TRUE

# ETS vs Seasonal Naive 
# Applying ETS on seasonal data 
cement <- window(qcement,start=1994)
autoplot(cement)
# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

# Apply tsCV() for both methods
e1 <- tsCV(cement,fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)

# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm=T)
mean(e2^2,na.rm=T)

# Copy the best forecast MSE
bestmse <- mean(e2^2,na.rm=T)


# When does ETS fails 

# Plot the lynx series
autoplot(lynx)

# Use ets() to model the lynx series
fit <- ets(lynx)

# Use summary() to look at model and parameters
summary(fit)

# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot()

# Transformations for variable stabilization 
autoplot(usmelec)
autoplot(usmelec^0.5)
autoplot(usmelec^0.333)
autoplot(log(usmelec))
autoplot(-1/usmelec)

BoxCox.lambda(usmelec)

usmelec%>%ets(lambda=-0.5738)%>%forecast(h=60)%>%autoplot()
# Plot the series
autoplot(a10)

# Try four values of lambda in Box-Cox transformations i.e 0.0,0.1,0.2,0.3
a10 %>% BoxCox(lambda = 0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()



# Compare with BoxCox.lambda()
BoxCox.lambda(a10)

# Non seasonal differencing for seasonality 

# Plot the US female murder rate
autoplot(wmurders)

# Plot the differenced murder rate
autoplot(diff(wmurders))

# Plot the ACF of the differenced murder rate
ggAcf(diff(wmurders))
ggAcf(wmurders)
# Seasonal differencing for stationarity

# Plot the data
autoplot(h02)
autoplot(log(h02))
# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)
ggAcf(difflogh02)
# Take another difference and plot
ddifflogh02 <- diff(difflogh02)
autoplot(ddifflogh02)

# Plot ACF of ddifflogh02
ggAcf(ddifflogh02)

# Automatic ARIMA models for non-seasonal time series

# Fit an automatic ARIMA model to the austa series
autoplot(austa)
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)
residualsok <- TRUE

# Summarize the model
summary(fit)

# Find the AICc value and the number of differences used
AICc <- -14.46
d <- 1

# Plot forecasts of fit
fit %>% forecast(h = 10) %>%autoplot()

# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0,1,1), include.constant = F) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order=c(2,1,3),include.constant=T) %>% forecast() %>% autoplot()

# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order=c(0,0,1),include.constant=T) %>% forecast()%>% autoplot()

# Plot forecasts from an ARIMA(0,2,1) model with no constant

austa %>% Arima(order=c(0,2,1),include.constant=F) %>% forecast()%>% autoplot()


# Comparing auto.arima() and ets() on non-seasonal data

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h=1)

# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa,farima, h=1)

# Find MSE of each model class
mean(e1^2,na.rm=T)
mean(e2^2,na.rm=T)

# Plot 10-year forecasts using the best model class
austa %>% farima(h=10) %>% autoplot()
austa %>% fets(h=10) %>% autoplot()
# Automatic ARIMA models for seasonal time series
autoplot(debitcards)
fit<-auto.arima(debitcards,lambda = 0)
fit
fit%>%forecast(h=36)%>%autoplot()

# Check that the logged h02 data have stable variance
autoplot(h02)
h02 %>% log()%>% autoplot()

# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02,lambda=0)

# Summarize the fitted model
summary(fit)

# Record the amount of lag-1 differencing and seasonal differencing used
d <- 1
D <- 1

# Plot 2-year forecasts
fit %>% forecast(h=24) %>% autoplot()

# Exploring auto.arima() options

# Find an ARIMA model for euretail
autoplot(euretail)
fit1 <- auto.arima(euretail)
summary(fit1)
# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)
summary(fit2)
# AICc of better model
AICc <- 68.39

# Compute 2-year forecasts from better model
fit2 %>% forecast(h = 8) %>% autoplot()

# Comparing auto.arima() and ets() on seasonal data

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007,4))

# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find better model based on RMSE
accuracy(fc1,qcement)
accuracy(fc2,qcement)
bettermodel <- fit2

advert
