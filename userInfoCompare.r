setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\OrderCate8BasicInfo\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)

raw.data1 <- fread('JData_Action_201602.csv',header = T,stringsAsFactors = F)
raw.data2 <- fread('JData_Action_201603.csv',header = T,stringsAsFactors = F)
raw.data3 <- fread('JData_Action_201603_extra.csv',header = T,stringsAsFactors = F)
raw.data4 <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)
userInfo <- fread('JData_User.csv',header = T,stringsAsFactors = F)

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

result <- analysisData[,.(orderCase=getOrderNumFunc(type)),by=user_id]
setkey(result,orderCase)
result <- result[orderCase=='order']
result <- result[,orderCase:=NULL]

OrderCate8UserInfo <- merge(result,userInfo,by='user_id')
write.csv(OrderCate8UserInfo,file=paste0(resultPath,'orderCate8UserInfo.csv'),
          fileEncoding='gbk',row.names=F)

ageP1 <- prop.table(table(userInfo$age))[-2]
ageP2 <- prop.table(table(OrderCate8UserInfo$age))
maxAgeDiff <- max(abs(ageP1-ageP2))

sexP1 <- prop.table(table(userInfo$sex))
sexP2 <- prop.table(table(OrderCate8UserInfo$sex))
maxSexDiff <- max(abs(sexP1-sexP2))

levelP1 <- prop.table(table(userInfo$user_lv_cd))
levelP2 <- prop.table(table(OrderCate8UserInfo$user_lv_cd))
maxLevelDiff <- max(abs(levelP1-levelP2))

tmpTime1 <- userInfo$user_reg_dt
yearDiff1 <- as.numeric(year(now())) - as.numeric(year(tmpTime1))
tmpTime2 <- OrderCate8UserInfo$user_reg_dt
yearDiff2 <- year(now()) - year(tmpTime2)
userInfo[,user_reg_dt:=yearDiff1]
OrderCate8UserInfo[,user_reg_dt:=yearDiff2]
yearDiffP1 <- prop.table(table(userInfo$user_reg_dt))[-14]
yearDiffP2 <- prop.table(table(OrderCate8UserInfo$user_reg_dt))
maxYearDiff <- max(abs(yearDiffP1 - yearDiffP2))

basicInfoDiff <- cbind(maxAgeDiff,maxSexDiff,maxLevelDiff,maxYearDiff)

write.csv(basicInfoDiff,file=paste0(resultPath,'basicInfoDiff.csv'),fileEncoding='gbk',row.names=F)

getOrderNumFunc <- function(type){
  orderNum <- length(which(type == 4))
  orderCase <- ifelse(orderNum>0,'order','notOrder')
  return(orderCase)
}



