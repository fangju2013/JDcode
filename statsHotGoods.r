setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\statsHotGoods\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)

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


# stats the hot style goods by each behaior and all behaviors
result <- analysisData[,.(typeNum=statsHotGoodsFunc(type,behavior=6)),by=.(sku_id)]
setorder(result,-typeNum,sku_id) 

# write the result into csv 
write.csv(result,file = paste0(resultPath,'hotGoodsType6.csv'),fileEncoding = 'gbk',row.names = F)
quantile(result[,typeNum])
plot(1:length(result[,typeNum]),result[,typeNum])
sum(result[,typeNum]>20000)

statsHotGoodsFunc <- function(type,behavior = 'all'){
  if(is.numeric(behavior)){
    typeNum <- length(which(type == behavior)) 
  }else{
    type1 <- length(which(type == 1))
    type2 <- length(which(type == 2))
    type4 <- length(which(type == 4))
    type5 <- length(which(type == 5))
    type6 <- length(which(type == 6))
    typeNum <- sum(type1,type2,type4,type5,type6)
  }
  return(typeNum)
}