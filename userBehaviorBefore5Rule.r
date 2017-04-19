setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\userBehaviorBeforeOrderDay\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)

# get the analysis data 
raw.data1 <- fread('JData_Action_201602.csv',header = T,stringsAsFactors = F)
raw.data2 <- fread('JData_Action_201603.csv',header = T,stringsAsFactors = F)
raw.data3 <- fread('JData_Action_201603_extra.csv',header = T,stringsAsFactors = F)
raw.data4 <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)
setkey(raw.data1,cate)
setkey(raw.data2,cate)
setkey(raw.data3,cate)
setkey(raw.data4,cate)
raw.data1 <- raw.data1[cate==8]
raw.data2 <- raw.data2[cate==8]
raw.data3 <- raw.data3[cate==8]
raw.data4 <- raw.data4[cate==8]

raw.data.l <- list(raw.data1,raw.data2,raw.data3,raw.data4)
raw.data <- rbindlist(raw.data.l)
analysisData <- raw.data

result <- analysisData[,.(value=getUserBehaviorRuleFunc(time,type,timeLag=3)),by=user_id]
tmpdf <- do.call(rbind,str_split(result$value,'#'))
colnames(tmpdf) <- varName
statsResult <- cbind(user_id = result$user_id,tmpdf)
statsResult <- data.frame(statsResult,stringsAsFactors = F)
statsResult <- statsResult[statsResult$browseNum > -1,]
table(statsResult$allNum)



getUserBehaviorRuleFunc <- function(time,type,thrsd = 10,timeLag=5){
  tmpDf <- data.frame(time,type,stringsAsFactors = F)
  tmp <- which(tmpDf$type == 4)
  if(length(tmp)>0){
    tmptime <- time[tmp]
    orderTime <- min(tmptime)
    orderTime <- as.Date(orderTime)
    tmpdf <- tmpDf[tmpDf$time<(orderTime-timeLag),]
    browseNum <- length(which(tmpdf$type==1))
    cartNum <- length(which(tmpdf$type==2))
    deleteNum <- length(which(tmpdf$type==3))
    attenNum <- length(which(tmpdf$type==5))
    clickNum <- length(which(tmpdf$type==6))
  }else{
    browseNum <- -1
    cartNum <- -1
    deleteNum <- -1
    attenNum <- -1
    clickNum <- -1
  }
  allNum <- sum(browseNum,cartNum,deleteNum,attenNum,clickNum)
  allNum <- ifelse(allNum>=thrsd,thrsd,allNum)
  browseNum <- ifelse(browseNum>=thrsd,thrsd,browseNum)
  cartNum <- ifelse(cartNum>=thrsd,thrsd,cartNum)
  deleteNum <- ifelse(deleteNum>=thrsd,thrsd,deleteNum)
  attenNum <- ifelse(attenNum>=thrsd,thrsd,attenNum)
  clickNum <- ifelse(clickNum>=thrsd,thrsd,clickNum)
  statsResult <- cbind(browseNum,cartNum,deleteNum,attenNum,clickNum,allNum)
  varName <<- colnames(statsResult)
  statsResult <- paste0(statsResult,collapse = '#')
  return(statsResult)
}

