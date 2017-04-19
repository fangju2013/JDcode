rm(list=ls());gc()
setwd('E:/JDproject/JData/newData')

library(data.table)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)
library(dtplyr)
library(sampling)
library(Matrix)
library(xgboost)

options(scipen = 200)

#读取数据
jdata.action02<-fread("JData_Action_201602.csv")
jdata.action03<-fread("JData_Action_201603.csv")
jdata.action04<-fread("JData_Action_201604.csv")
jdata.Comment<-fread("JData_Comment.csv")
jdata.Product<-fread("JData_Product.csv")
jdata.user<-fread("JData_User.csv")

#保存可用的数据集合
actionData <- rbind(jdata.action02,jdata.action03,jdata.action04)
actionData <- distinct(actionData)
dateFormat <- str_sub(actionData[,time],1,10)
actionData <- actionData[,dateFormat:=dateFormat]

rm(jdata.action02,jdata.action03,jdata.action04)
gc()

save.image("E:/JDproject/JData/newData/jdData.RData")

load("jdData.RData")


#数据分析
actionData <- actionData %>% data.table

tmpData <- actionData[,.(user_id,sku_id,time,type)]
usku <- actionData[type == 4 & time > "2016-02-07 00:00:00"][,.(user_id,sku_id)]

tmpData <- tmpData[,uk:=paste0(tmpData$user_id,tmpData$sku_id)]
usku <- usku[,uk:=paste0(usku$user_id,usku$sku_id)]

tt <- merge(tmpData,usku,by="uk")

zz <- setDT(tt)[,min(time),by=.(user_id.x,sku_id.x,type)]

nn <- dcast(zz,user_id.x+sku_id.x~type) %>% data.table
nn <- nn[,diffHour:=round(as.numeric((ymd_hms(nn$`4`)-ymd_hms(nn$`1`))/3600),2)]
options(scipen = 200)

mm<-nn[diffHour>0 & !is.na(diffHour)]
length(mm$diffHour[mm$diffHour<48])

