setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\weekOrderNumStats\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(lubridate)
library(reshape2)

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

tmptime <- as_date(analysisData[,time])
analysisData[,time:=tmptime]

result <- analysisData[,.(typeNum=statsWeekNumFunc(type)),by=.(time)]
setorder(result,time)
orderWeek <- weekdays(result$time)
result <- result[,orderWeek:=orderWeek]

write.csv(result,file=paste0(resultPath,'orderWeekNum.csv'),fileEncoding = 'gbk',row.names=F)

statsWeekNumFunc <- function(type){
  type4Num <- length(which(type==4))
  return(type4Num)
}




