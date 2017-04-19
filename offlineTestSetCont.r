rm(list=ls());gc()
setwd('E:/JDproject/JData/newData')
resultPath <- 'E:/JDproject/JDstatsAnalysisResult/featureConstruct/'

library(data.table)
library(dplyr)
library(reshape2)
library(dtplyr)
library(stringr)
library(lubridate)

#get the basic data concluding the user behavior data,comment data,product data;
#and userinfo data
load("jdData.RData")

#线下测试集构造:
#这里取前5天的交互期窗口，后期根据情况调整;
#取前五天加入购物车没有购买而且没有删除购物车的UI（user+sku）对,然后用这些UI对跟后面5天
#做interset，购买1，非购买0;
tmp <- actionData[dateFormat<"2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-2)]
tmp <- tmp[,.N,by=.(user_id,sku_id,cate,brand,type)]
tmp <- dcast(tmp,user_id+sku_id+cate+brand~type,fill=0) %>% data.table 
offLineTestSample <- tmp[tmp$`4`==0 & tmp$`2`!=0 & tmp$`3`==0]
offLineTestSample <- offLineTestSample[,.(user_id,sku_id,cate,brand)]

offLineTest<-actionData[dateFormat>="2016-04-11"&dateFormat<as.character(as.Date("2016-04-11")+5)]
offLineTest <- offLineTest[type==4]
offLineTest <- offLineTest[,.(user_id,sku_id,cate,brand)]

offLineTestPos <- fintersect(offLineTestSample,offLineTest)
offLineTestNeg <- fsetdiff(offLineTestSample,offLineTestPos)#构造训练集的时候考虑先直接抽样

offLineTestPos <- offLineTestPos[,flag:=1]
offLineTestNeg <- offLineTestNeg[,flag:=0]
offLineTestAction <- rbind(offLineTestPos,offLineTestNeg)

#取构造测试集所需数据;
action1<-actionData[dateFormat<"2016-04-11"&dateFormat>=as.character(as.Date("2016-04-11")-28)]
appendData<-data.table(user_id=rep(1,6),
                       sku_id=rep(1,6),
                       time=as.character(ymd_hms(paste("2016-04-11","00:00:00"))-dhours(0.5)),
                       model_id=rep(NA,6),
                       type=c(1:6),
                       cate=1,
                       brand=1,
                       dateFormat=as.character(as.Date("2016-04-11")-ddays(1))
)
action1<-rbind(action1,appendData)
action1$diffHour<-round(as.numeric((ymd_hms("2016-04-11 00:00:00")-ymd_hms(action1$time))/3600),2)

#取user_id,sku_id,brand,user_id+sku_Id,user_id+brand的滑动窗口特征值;
#共5*9*6=270维特征;
diffDate <- c(1,2,3,5,7,10,14,21,28)

dtNames<-c(paste0("u",diffDate),
           paste0("sku",diffDate),
           paste0("brand",diffDate),
           paste0("uk",diffDate),
           paste0("ub",diffDate))

dateFormatChr<-c('dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-1)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-2)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-3)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-5)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-7)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-10)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-14)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-21)',
                 'dateFormat < "2016-04-11" & dateFormat>=as.character(as.Date("2016-04-11")-28)')

dateFormatChrRep<-rep(dateFormatChr,5)

tmp<-offLineTestAction

for(i in dtNames){
  print(i)
  j=dateFormatChrRep[match(i,dtNames)]
  if(i %in% dtNames[c(1:9)]){ 
    eval(parse(text = paste0(i,"=dcast(setDT(filter(action1,",j,"))[,.N,by=.(user_id,type)],
                             user_id~type,fill=0)")))
    eval(parse(text = paste0("colnames(",i,")[c(2:7)]=paste0(","'",i,"',","'","type","',",
                             "c(1:6)",")")))
    eval(parse(text = paste0("tmp<-merge(x=tmp,y=",i,",by.x = ","'","user_id","'",",by.y = ","'",
                             "user_id","'",",all.x=T)")))
  }
  else if(i %in% dtNames[c(10:18)]){
    eval(parse(text = paste0(i,"=dcast(setDT(filter(action1,",j,"))[,.N,by=.(sku_id,type)],
                             sku_id~type,fill=0)")))
    eval(parse(text = paste0("colnames(",i,")[c(2:7)]=paste0(","'",i,"',","'","type","',",
                             "c(1:6)",")")))
    eval(parse(text = paste0("tmp<-merge(x=tmp,y=",i,",by.x = ","'","sku_id","'",",by.y = ","'",
                             "sku_id","'",",all.x=T)")))
  }
  else if(i %in% dtNames[c(19:27)]){
    eval(parse(text = paste0(i,"=dcast(setDT(filter(action1,",j,"))[,.N,by=.(brand,type)],
                             brand~type,fill=0)")))
    eval(parse(text = paste0("colnames(",i,")[c(2:7)]=paste0(","'",i,"',","'","type","',",
                             "c(1:6)",")")))
    eval(parse(text = paste0("tmp<-merge(x=tmp,y=",i,",by.x = ","'","brand","'",",by.y = ","'",
                             "brand","'",",all.x=T)")))
  }
  else if(i %in% dtNames[c(28:36)]){
    eval(parse(text = paste0(i,"=dcast(setDT(filter(action1,",j,"))[,.N,by=.(user_id,sku_id,type)],
                             user_id+sku_id~type,fill=0)")))
    eval(parse(text = paste0("colnames(",i,")[c(3:8)]=paste0(","'",i,"',","'","type","',",
                             "c(1:6)",")")))
    eval(parse(text = paste0("tmp<-merge(x=tmp,y=",i,",by = ","c('","user_id","'",",","'",
                             "sku_id","')",",all.x=T)")))
  }
  else{
    eval(parse(text = paste0(i,"=dcast(setDT(filter(action1,",j,"))[,.N,by=.(user_id,brand,type)],
                             user_id+brand~type,fill=0)")))
    eval(parse(text = paste0("colnames(",i,")[c(3:8)]=paste0(","'",i,"',","'","type","',",
                             "c(1:6)",")")))
    eval(parse(text =paste0("tmp<-merge(x=tmp,y=",i,",by = ","c('","user_id","',","'","brand",
                            "')",",all.x=T)")))
  }
}

offLineTestData <- tmp

#UI特征构造:
#01-排序特征构造(数值型排序,只考虑五天,对用户浏览、点击、加入购物车的同一brand下有大于两种sku的,
#用户+商品添加标签*CountFlag;对*CountFlag==1的user_id+sku_id根据type对应数值进行排序,
#生成变量*CountRank;
ukNames<-c(paste0("uk",c(1,2,3,5),"type",1),paste0("uk",c(1,2,3,5),"type",6),
           paste0("uk",c(1,2,3,5),"type",2))

for(i in ukNames){
  print(i)
  eval(parse(text = paste0("mm<-offLineTestData[,c(","'","user_id","',","'","sku_id","',","'",
                           "brand","',","'",i,"')]")))
  eval(parse(text = paste0("nn<-data.table(setDT(mm)[",i,"!=0][,.N,by=.(user_id,brand)][N>1],",i,
                           "CountFlag=1)")))
  eval(parse(text = paste0("offLineTestData<-merge(offLineTestData,nn[,c(1,2,4)],by=c(","'",
                           "user_id","',","'","brand","'","),all.x = T)")))
  eval(parse(text = paste0("tmp<-offLineTestData[",i,"CountFlag==1,c(","'","user_id","',","'",
                           "sku_id","',","'","brand","',","'",i,"')][,",i,"CountRank:=rank(-",i,"),
                           by=.(user_id,brand)]")))
  eval(parse(text = paste0("offLineTestData<-merge(offLineTestData,distinct(tmp[,c(1,2,3,5)]),
                           by=c(","'","user_id","',","'","sku_id","',","'","brand","'","),all.x = T)")))
}

#02-计算28天内,每对UI操作(浏览、加购、关注、点击)最早、最晚时间点距离目标时间点的距离;

ukMinHour<-dcast(action1[,min(diffHour),by=c("user_id","sku_id","type")],user_id+sku_id~type)
colnames(ukMinHour)[3:8]<-paste0("ukMinHourtype",c(1:6))
ukMaxHour<-dcast(action1[,max(diffHour),by=c("user_id","sku_id","type")],user_id+sku_id~type)
colnames(ukMaxHour)[3:8]<-paste0("ukMaxHourtype",c(1:6))

offLineTestData<-merge(offLineTestData,ukMinHour,by=c("user_id","sku_id"),all.x = T)
offLineTestData<-merge(offLineTestData,ukMaxHour,by=c("user_id","sku_id"),all.x = T)

#03-UI最近1个小时、2个小时、5个小时、12个小时行为分段统计;
dhour<-c(1,2,5,12)

for(i in dhour){
  eval(parse(text = paste0("ukdHours",i,"<-dcast(setDT(filter(action1,time>as.character(ymd_hms(","'",
                           "2016-04-11 00:00:00","'",")-dhours(x =",i,"))))[,.N,by=.(user_id,sku_id,
                           type)],user_id+sku_id~type,fill=0)")))
  eval(parse(text = paste0("colnames(ukdHours",i,")[c(3:8)]=paste0(","'","ukdHours",i,"',","'","type",
                           "',","c(1:6)",")")))
  eval(parse(text = paste0("offLineTestData<-merge(x=offLineTestData,y=","ukdHours",i,",by=","c('",
                           "user_id","','","sku_id","')",",all.x=T)")))
}


# U特征构造:
#01-计算28天内,每个U操作(浏览、加购、关注、点击)最早、最晚时间点距离目标时间点的距离;
udiffHour<-dcast(setDT(action1)[,min(diffHour),by=.(user_id,type)],user_id~type,fill=0)
colnames(udiffHour)[2:7]<-paste0(c("udiffHourType"),c(1:6))
offLineTestData<-merge(offLineTestData,udiffHour,by="user_id",all.x = T)

#02-用户最近1个小时、2个小时、5个小时、12个小时用户行为分段统计,后续考虑加入用户时间
#分段统计: 0-1,1-2时间段等;
tmpData<-filter(action1,dateFormat==as.character(as.Date("2016-04-11")-ddays(1)))

dhour<-c(1,2,5,12)

for(i in dhour){
  eval(parse(text = paste0("udHours",i,"<-dcast(setDT(filter(tmpData,time>as.character(ymd_hms(","'",
                           "2016-04-11 00:00:00","'",")-dhours(x =",i,"))))[,.N,by=.(user_id,type)],
                           user_id~type,fill=0)")))
  eval(parse(text = paste0("colnames(udHours",i,")[c(2:7)]=paste0(","'","udHours",i,"',","'","type",
                           "',","c(1:6)",")")))
  eval(parse(text = paste0("offLineTestData<-merge(x=offLineTestData,y=","udHours",i,",by=","'",
                           "user_id","'",",all.x=T)")))
}

#03-用户基本信息加入;
offLineTestData<-merge(offLineTestData,jdata.user,by="user_id",all.x = T)
offLineTestData$user_reg_tm<-NULL
offLineTestData$age<-as.factor(offLineTestData$age)
offLineTestData$sex<-as.factor(offLineTestData$sex)
offLineTestData$user_lv_cd<-as.factor(offLineTestData$user_lv_cd)

fwrite(offLineTestData,file = paste0(resultPath,'offLineTestData.csv'))
save(offLineTestData,file = paste0(resultPath,"offLineTestData.RData"))

