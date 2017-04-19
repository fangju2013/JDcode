setwd('E:\\JDproject\\JData')
library(data.table)
library(dtplyr)
library(dplyr)
library(stringr)

raw.data <- fread('JData_Action_201604.csv',header = T)
analysisData <- raw.data
time1 <- str_sub(raw.data$time,1,13)
analysisData[,time := time1]
setkey(analysisData,time,type,cate)
pred0328 <- analysisData[cate == 8 & type%in%c(1,2,5) & time >= '2016-04-15 23']
predResult0328 <- pred0328[,.(sku_id=findsku(sku_id)),.(user_id)]
predResult0328$user_id <- predResult0328$user_id %>% as.integer()
predResult0328$sku_id <- predResult0328$sku_id %>% as.integer()
write.csv(predResult0328,file = 'predResult0328.csv',fileEncoding = 'utf8',
          row.names = F)


findsku<-function(sku_id){
  tmp<-table(sku_id)
  return(names(tmp)[which.max(tmp)])
}


raw.data <- fread('JData_Action_201604.csv',header = T)
analysisData <- raw.data
time1 <- str_sub(raw.data$time,1,16)
analysisData[,time := time1]
setkey(analysisData,time,type,cate)
pred0328a <- analysisData[cate == 8 & type%in%c(1,2,5) & time >= '2016-04-15 23:50']
predResult0328a <- pred0328a[,.(sku_id=findsku(sku_id)),.(user_id)]
predResult0328a$user_id <- predResult0328a$user_id %>% as.integer()
predResult0328a$sku_id <- predResult0328a$sku_id %>% as.integer()
write.csv(predResult0328a,file = 'predResult0328a.csv',fileEncoding = 'utf8',
          row.names = F)



