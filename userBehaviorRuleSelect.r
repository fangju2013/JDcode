setwd('E:\\JDproject\\JData')

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)

raw.data1 <- fread('JData_Action_201602.csv',header = T)
raw.data2 <- fread('JData_Action_201603.csv',header = T)
raw.data3 <- fread('JData_Action_201603_extra.csv',header = T)
raw.data4 <- fread('JData_Action_201604.csv',header = T)

# get the necessary analysis data
analysisData <- raw.data1
setkey(analysisData,cate)
analysisData <- analysisData[cate == 8]
tmptime <- as.Date(analysisData$time)
analysisData <- analysisData[,time:=tmptime]

# according to the user_id,sku_id or user_id and sku_id to calculate the prop 
g.p <- group_by(analysisData,user_id)
result <- summarise(g.p,vaule=statsNumFunc(time,type))
result <- data.frame(result,stringsAsFactors = F)
tmpdf <- do.call(rbind,str_split(result$vaule,'#'))
colnames(tmpdf) <- allPropName
statsResult <- cbind(user_id = result$user_id,tmpdf)
statsResult <- data.frame(statsResult,stringsAsFactors = F)

statsPropResult(statsResult)

# stats the statsResult and get the user behavior which is most important 
statsPropResult <- function(df,needlessVar=c('user_id','sku_id')){
  propStats <- c()
  for(i in names(df)){
    if(!i %in% needlessVar){
      tmp <- sum(ifelse(df[,i]>0,1,0))
      names(tmp) <- i
      propStats <- append(propStats,tmp)
    }
  }
  return(propStats)
}


# stats different wins of the user behaviors which will get the probability in 
# each behavior 
statsNumFunc <- function(time,type,win=5,cutGroup=c(1:6)){
  end <- max(time)
  cutTime <- c()
  for(i in cutGroup){
    tmptime <- time[time>end-i*win & time <= end-(i-1)*win]
    tmp <- c(rep(i,length(tmptime)))
    cutTime <- append(cutTime,tmp)
  }
  tmpdf<-data.frame(type=type,time=time,cutTime=cutTime,stringsAsFactors=F)
  browseOrderNum <- c()
  cartOrderNum <- c()
  deleteOrderNUm <- c()
  attenOrderNUm <- c()
  clickOrderNum <- c()
  for(j in cutGroup[-length(cutGroup)]){
    cutTmpdf <- tmpdf[tmpdf$cutTime == j,]
    type1 <- length(which(cutTmpdf$type == 1))
    type2 <- length(which(cutTmpdf$type == 2))
    type3 <- length(which(cutTmpdf$type == 3))
    type5 <- length(which(cutTmpdf$type == 5))
    type6 <- length(which(cutTmpdf$type == 6))
    cutNextTmpdf <- tmpdf[tmpdf$cutTime == (j+1),]
    type4 <- length(which(cutNextTmpdf$type == 4))
    if(type1>0 & type4>0){
      browseOrderNum <- append(browseOrderNum,1)
    }else{
      browseOrderNum <- append(browseOrderNum,0)
    }
    if(type2>0 & type4>0){
      cartOrderNum <- append(cartOrderNum,1)
    }else{
      cartOrderNum <- append(cartOrderNum,0)
    }
    if(type3>0 & type4>0){
      deleteOrderNUm <- append(deleteOrderNUm,1)
    }else{
      deleteOrderNUm <- append(deleteOrderNUm,0)
    }
    if(type5>0 & type4>0){
      attenOrderNUm <- append(attenOrderNUm,1)
    }else{
      attenOrderNUm <- append(attenOrderNUm,0)
    }
    if(type6>0 & type4>0){
      clickOrderNum <- append(clickOrderNum,1)
    }else{
      clickOrderNum <- append(clickOrderNum,0)
    }
  }
  browseOrderProp <- round(sum(browseOrderNum)/length(browseOrderNum),2)
  cartOrderProp <- round(sum(cartOrderNum)/length(cartOrderNum),2)
  deleteOrderProp <- round(sum(deleteOrderNUm)/length(deleteOrderNUm),2)
  attenOrderProp <- round(sum(attenOrderNUm)/length(attenOrderNUm),2)
  clickOrderProp <- round(sum(clickOrderNum)/length(clickOrderNum),2)
  allProp <- cbind(browseOrderProp,cartOrderProp,deleteOrderProp,attenOrderProp,
                   clickOrderProp)
  allPropName <<- colnames(allProp)
  result <- paste0(allProp,collapse = '#')
  return(result)
}
