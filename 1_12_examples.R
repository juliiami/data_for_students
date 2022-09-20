##### Time series analysis

data(AirPassengers)
plot(AirPassengers)

s_dec <- decompose(AirPassengers, type ="additive")
plot(s_dec)

#s_dec <- decompose(AirPassengers, type ="multiplicative")
#plot(s_dec)

#s_dec <- stl(AirPassengers, s.window='periodic')
#plot(s_dec)

stationary <- diff(diff(AirPassengers, 12),1)
plot(stationary)
acf(stationary)
pacf(stationary)

library(forecast)

fit <- auto.arima(AirPassengers)
plot(forecast(fit))

fit <- ets(AirPassengers)
plot(forecast(fit))


##### Anomaly detection

library(xts)
library(chron)
library(tis)

taxi <- read.csv('https://raw.githubusercontent.com/numenta/NAB/master/data/realKnownCause/nyc_taxi.csv')

taxi <- aggregate(taxi$value, list(format(as.POSIXlt(taxi$timestamp),"%y-%m-%d %H")), sum) 

taxi <- xts(taxi$x, order.by = as.POSIXlt(taxi$Group.1, format = "%y-%m-%d %H"))

# Create additional features
weekday <- index(taxi)$wday
yearday <- index(taxi)$yday
hour <- index(taxi)$hour
is_weekend <- is.weekend(index(taxi))
is_holiday <- isHoliday(index(taxi))


taxi <- merge(taxi, cbind(weekday, yearday,hour,is_weekend,is_holiday))

plot(taxi$taxi)

boxplot(taxi$taxi)
hist(taxi$taxi) #long tail

### Grubbs' test

library(outliers)

grubbs.test(as.ts(taxi$taxi)) #close to zero -> outlier

#which.max(taxi$taxi)
#plot(taxi$taxi)
#lines(taxi$taxi[which.max(taxi$taxi)], col='red', type='o', pch=16)


#### Time series anomalies

### SARIMA

library(forecast)

fit <- auto.arima(taxi$taxi, seasonal = TRUE)
res <- taxi$taxi
res$fitted <- fit$fitted
plot(res)
lines(res$taxi[abs(res$taxi-res$fitted)>15000], col='red', type='o', pch=16)

taxi[abs(res$taxi-res$fitted)>15000]


### Seasonal Hybrid ESD

#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

ad <- AnomalyDetectionVec(as.data.frame(taxi$taxi),max_anoms=0.001, period=24, direction='both', plot=TRUE)
ad$anoms
ad$plot

taxi[ad$anoms$index,]


#### Distance based methods

### kNN

library(FNN)
data_nn <- get.knn(scale(taxi), k = 5)
score <- rowMeans(data_nn$nn.dist)

plot(hour ~ taxi, data = taxi, cex = sqrt(score), pch = 20)
plot(yearday ~ taxi, data = taxi, cex = sqrt(score), pch = 20)

plot(taxi$taxi)
lines(taxi$taxi[score>1], col='red', type='o', pch=16)
taxi[score>1]


### LOF

library(dbscan)

score <- lof(scale(taxi), minPts  = 5)

plot(hour ~ taxi, data = taxi, cex = sqrt(score), pch = 20)
plot(yearday ~ taxi, data = taxi, cex = sqrt(score), pch = 20)

plot(taxi$taxi)
lines(taxi$taxi[score>2], col='red', type='o', pch=16)
taxi[score>2]


#### Classification based methods

### One-class SVM

library(e1071)

model <- svm(taxi, type='one-classification', nu = 0.0001)  
model

summary(model) 
pred <- predict(model, taxi)
table(pred)

plot(taxi$taxi)
lines(taxi$taxi[pred==FALSE], col='red', type='o', pch=16)

taxi[pred==FALSE]


### Isolation forests

library(isofor)

# Build an isolation tree 
data_tree <- iForest(taxi, nt = 1)

tree_score <- predict(data_tree, newdata = taxi)

hist(tree_score, breaks = 40)

plot(hour ~ taxi, data = taxi, cex = sqrt(tree_score), pch = 20)
plot(yearday ~ taxi, data = taxi, cex = sqrt(tree_score), pch = 20)


plot(taxi$taxi)
lines(taxi$taxi[tree_score>0.8], col='red', type='o', pch=16)

taxi[tree_score>0.8]

# Fit isolation forest
data_forest <- iForest(taxi, nt = 100, phi = 3000)

score <- predict(data_forest, newdata = taxi)

hist(score)

plot(hour ~ taxi, data = taxi, cex = sqrt(score), pch = 20)
plot(yearday ~ taxi, data = taxi, cex = sqrt(score), pch = 20)

plot(taxi$taxi)
lines(taxi$taxi[score>0.65], col='red', type='o', pch=16)

taxi[score>0.65]