setwd('E:\\JDproject\\JData')

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(lubridate)

# get the analysis data 
raw.data1 <- fread('JData_Action_201602.csv',header = T,stringsAsFactors = F)
analysisData <- raw.data1
setkey(analysisData,cate)
analysisData <- analysisData[cate == 8]
tmptime <- as_date(analysisData$time)
tmphour <- hour(analysisData$time)
analysisData[,c('time','hour'):= list(tmptime,tmphour)]

# calculate the probability according to the user_id,sku_id,user_id and sku_id 
result <- analysisData[,.(value=getTimeHourPropFunc(time,type,hour,behavior = 2)),by = user_id]
tmpdf <- do.call(rbind,str_split(result$value,"#"))
colnames(tmpdf) <- allPropName
statsResult <- cbind(user_id=result$user_id,tmpdf)
statsResult <- data.frame(statsResult,stringsAsFactors = F)

# stats the number of ordering in each period of time  
statsPropResultFunc(statsResult,behavior = 2)

# the function is calculate the number of ordering in each period of time 
statsPropResultFunc <- function(df,behavior=1,needlessVar=c('user_id','sku_id','behavior')){
  propStats <- c()
  for(i in names(df)){
    if(!i%in%needlessVar){
      tmp <- sum(ifelse(df[,i]>0,1,0))
      names(tmp) <- i
      propStats <- append(propStats,tmp)
    }
  }
  names(behavior) <- 'behavior'
  propStats <- append(behavior,propStats)
  return(propStats)
}


# stats different wins of the user behaviors in each period of time which will get the 
# probability in each behavior
getTimeHourPropFunc <- function(time,type,hour,behavior=1,win=5,cutGroup=c(1:6)){
  end <- max(time)
  cutTime <- c()
  for(i in cutGroup){
    tmptime <- time[time>end-i*win & time<=end-(i-1)*win]
    tmp <- c(rep(i,length(tmptime)))
    cutTime <- append(cutTime,tmp)
  }
  tmpdf <- data.frame(type=type,time=time,hour=hour,cutTime=cutTime,stringsAsFactors=F)
  MornOrderNum <- c()
  NoonOrderNum <- c()
  AfterOrderNUm <- c()
  NightOrderNum <- c()
  MidnightOrderNum <- c()
  for(j in cutGroup[-length(cutGroup)]){
    cutTmpMorndf <- tmpdf[tmpdf$cutTime==j & tmpdf$hour%in%c(7:11),]
    cutTmpNoondf <- tmpdf[tmpdf$cutTime==j & tmpdf$hour%in%c(12:13),]
    cutTmpAfterdf <- tmpdf[tmpdf$cutTime==j & tmpdf$hour%in%c(14:18),]
    cutTmpNightdf <- tmpdf[tmpdf$cutTime==j & tmpdf$hour%in%c(19:23),]
    cutTmpMidnightdf <- tmpdf[tmpdf$cutTime==j & tmpdf$hour%in%c(0:6),]
    typeMorn <- length(which(cutTmpMorndf$type == behavior))
    typeNoon <- length(which(cutTmpNoondf$type == behavior))
    typeAfter <- length(which(cutTmpAfterdf$type == behavior))
    typeNight <- length(which(cutTmpNightdf$type == behavior))
    typeMidnight <- length(which(cutTmpMidnightdf$type == behavior))
    cutNextTmpdf <- tmpdf[tmpdf$cutTime==(j+1),]
    type4 <- length(which(cutNextTmpdf$type==4))
    if(typeMorn>0 & type4>0){
      MornOrderNum <- append(MornOrderNum,1)
    }else{
      MornOrderNum <- append(MornOrderNum,0)
    }
    if(typeNoon>0 & type4>0){
      NoonOrderNum <- append(NoonOrderNum,1)
    }else{
      NoonOrderNum <- append(NoonOrderNum,0)
    }
    if(typeAfter>0 & type4>0){
      AfterOrderNUm <- append(AfterOrderNUm,1)
    }else{
      AfterOrderNUm <- append(AfterOrderNUm,0)
    }
    if(typeNight>0 & type4>0){
      NightOrderNum <- append(NightOrderNum,1)
    }else{
      NightOrderNum <- append(NightOrderNum,0)
    }
    if(typeMidnight>0 & type4>0){
      MidnightOrderNum <- append(MidnightOrderNum,1)
    }else{
      MidnightOrderNum <- append(MidnightOrderNum,0)
    }
  }
  MornOrderProp <- round(sum(MornOrderNum)/length(MornOrderNum),2)
  NoonOrderProp <- round(sum(NoonOrderNum)/length(NoonOrderNum),2)
  AfterOrderProp <- round(sum(AfterOrderNUm)/length(AfterOrderNUm),2)
  NightOrderProp <- round(sum(NightOrderNum)/length(NightOrderNum),2)
  MidnightOrderProp <- round(sum(MidnightOrderNum)/length(MidnightOrderNum),2)
  behavior <- behavior
  allProp <- cbind(behavior,MornOrderProp,NoonOrderProp,AfterOrderProp,NightOrderProp,
                   MidnightOrderProp)
  allPropName <<- colnames(allProp)
  result <- paste0(allProp,collapse = '#')
  return(result)
} 