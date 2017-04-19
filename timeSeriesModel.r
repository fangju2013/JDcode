setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\userBehaviorTimeSerieResult\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(lubridate)
library(xgboost)
library(glmnet)

tmpRes <- fread(paste0(resultPath,'tmpRes.csv'),header=T)
modelData <- tmpRes
modelData <- data.frame(modelData,stringsAsFactors = F)
modelData <- charToNumFunc(modelData)

modelData <- modelData[order(modelData$time),][-1,2:ncol(modelData)]

tmpRow <- which(modelData$time1typeCate24==0)
for(i in tmpRow){
  tmpCol <- which(modelData[i,]==0)
  for(j in tmpCol){
    modelData[i,j] <- ceiling(mean(modelData[,j]))
  }
}

# get the train data and the test data 
modelData <- data.frame(modelData,stringsAsFactors = F)
trainData <- modelData[1:(nrow(modelData)-5),] 
trainX <- trainData[,2:ncol(trainData)] %>% as.matrix
trainY <- trainData$targ
maxTmp <- which.max(trainY)
trainY[maxTmp] <- ceiling(mean(trainY[-maxTmp]))

testData <- modelData[(nrow(modelData)-4):nrow(modelData),]  
testX <- testData[,2:ncol(testData)] %>% as.matrix()
testY <- testData$targ

dtrain <- xgb.DMatrix(data = trainX,label = trainY)
dtest <- xgb.DMatrix(data = testX,label = testY)

#train the data using xgboost
para.train <- list(eta = 0.1,
                   max_depth = 1,
                   objective = 'reg:linear')

set.seed(123)
bsts.train<-xgb.train(booster='gbtree',data=dtrain,paras=para.train,verbose=1,nround=100,
                        eval_metric='rmse',watchlist=list('train'=dtrain,'test'=dtest))
xgb.importance(colnames(trainX),model = bsts.train)

#train the data using Lasso
lasso.train <- cv.glmnet(x=trainX,y=trainY,family=c('gaussian'),nfolds = 4)
plot(lasso.train)
coef(lasso.train)
bestLam <- lasso.train$lambda.min
modelData <- as.matrix(modelData)
lasso.pred <- predict(lasso.train,newx=modelData[,2:ncol(modelData)], s=bestLam)
mean((lasso.pred - modelData[,1])^2)
var(modelData[,1])

# train the data using the linear regression 
linReg <- lm(targ~.,data = modelData)
summary(linReg)


charToNumFunc <- function(df){
  for(i in names(df)[-1]){
    df[,i] <- as.character(df[,i]) %>% as.numeric()
  }
  return(df)
}


#using the time series model
 #using the arima model
library(ggplot2)
library(forecast)
library(fUnitRoots)
 # get the target time series data
targData <- modelData$targ %>% as.character() %>% as.numeric()
locMax <- which.max(targData)
targData[locMax] <- ceiling(mean(targData[-locMax]))
plot.ts(targData)
 # divide the train and test time series data
trainTargData <- targData[1:(length(targData)-5)]
testTargData <- targData[(length(targData)-4):length(targData)]
plot.ts(trainTargData)
 # using ARIMA model to train the model 
diffTargData <- diff(trainTargData,differences = 1)
plot.ts(diffTargData)
acf(diffTargData,lag.max = 20)
acf(diffTargData,lag.max = 20,plot = FALSE)
pacf(diffTargData,lag.max = 20)
pacf(diffTargData,lag.max = 20,plot = FALSE)
targArima <- arima(trainTargData,order = c(1,1,0))
targArimaForecast <- forecast.Arima(targArima,h=5,level=c(99.5))
acf(targArimaForecast$residuals,lag.max = 20)
Box.test(targArimaForecast$residuals,lag = 20,type = 'Ljung-Box')
plot.ts(targArimaForecast$residuals)
targArimaForecast$mean - testTargData

 #using the simple moving average
library(TTR)
targSMA5 <- SMA(targData,n=5)
plot.ts(targSMA5)
targSMA5
targData
 

 #using the simple exponential smoothing 
  ## using the Holt exponential smoothing method which has no trend and seasonal term 
library(forecast)
targforecasts <- HoltWinters(targData,beta = F,gamma = F,l.start = targData[1])
targforecasts$fitted
targforecasts$SSE
plot(targforecasts)

targforecastsPred <- forecast.HoltWinters(targforecasts,h=5)
plot.forecast(targforecastsPred)

acf(targforecastsPred$residuals[!is.na(targforecastsPred$residuals)],lag.max = 20)
Box.test(targforecastsPred$residuals,lag=20,type='Ljung-Box')
plot.ts(targforecastsPred$residuals)


 #using the Holt exponential smoothing method which has trend term but has no seasonal term 
targforecasts1 <- HoltWinters(targData,gamma = F)
plot(targforecasts1)

targforecasts1 <- HoltWinters(targData,gamma = F,l.start = targData[1],
                            b.start = targData[2]-targData[1])
plot(targforecasts1)
targforecasts1Pred <- forecast.HoltWinters(targforecasts1,h=5)
plot.forecast(targforecasts1Pred)

acf(targforecasts1Pred$residuals[!is.na(targforecasts1Pred$residuals)],lag.max = 20)
Box.test(targforecasts1Pred$residuals,lag = 20,type = 'Ljung-Box')
plot.ts(targforecasts1Pred$residuals)


