setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\cate8OrderDayNum\\'

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

tmptime <- as_date(analysisData[,time])
analysisData[,time:=tmptime]

result <- analysisData[,.(typeNum=statsDayNumFunc(type)),by=.(time)]
setorder(result,time)
tmpdf <- do.call(rbind,str_split(result$typeNum,'#'))
colnames(tmpdf) <- resultName
statsResult <- cbind(time = as.character(result$time),tmpdf)
statsResult <- data.frame(statsResult,stringsAsFactors = F)
statsResult <- charToNumFunc(statsResult)

write.csv(statsResult,file=paste0(resultPath,'cate8OrderDayNum.csv'),fileEncoding = 'gbk',row.names=F)


charToNumFunc <- function(df,rawType='time'){
  for(i in names(df)){
    if(i !=  rawType){
      df[,i] <- as.character(df[,i])%>%as.numeric()
    }
  }
  return(df)
}

statsDayNumFunc <- function(type){
  typeNum1 <- length(which(type == 1))
  typeNum2 <- length(which(type == 2))
  typeNum3 <- length(which(type == 3))
  typeNum4 <- length(which(type == 4))
  typeNum5 <- length(which(type == 5))
  typeNum6 <- length(which(type == 6))
  result <- cbind(typeNum1,typeNum2,typeNum3,typeNum5,typeNum6,typeNum4)
  resultName <<- colnames(result)
  result <- paste0(result,collapse = '#')
  return(result)
}

