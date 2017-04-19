setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\beforeOrderDeleteCart\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(lubridate)

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

result <- analysisData[,.(deleteOrNot=statsDeleteOrderFunc(time,type)),by=user_id]
statsDeleteOrNotRes <- table(result$deleteOrNot)

write.csv(statsDeleteOrNotRes,file=paste0(resultPath,'beforeOrderDeleteCartOrNot.csv'),
          fileEncoding='gbk',row.names = F)

statsDeleteOrderFunc <- function(time,type){
  tmpdf <- data.frame(time,type,stringsAsFactors = F)
  tmp <- which(tmpdf$type==4)
  if(length(tmp)>0){
    tmptime <- time[tmp]
    orderTime <- min(tmptime)
    tmpdf <- tmpdf[tmpdf$time<orderTime,]
    deleteNum <- length(which(tmpdf$type==3))
  }else{
    deleteNum <- -1
  }
  deleteNum <- ifelse(deleteNum>0,'delete','notDelete')
  return(deleteNum)
}
