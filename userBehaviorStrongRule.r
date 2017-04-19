setwd('E:\\JDproject\\JData')

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(ggplot2)

#get the analysis data 
raw.data4 <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)
analysisData <- raw.data4
setkey(analysisData,cate)
analysisData <- analysisData[cate==8]

# get the stats variables 
result <- analysisData[,.(value=getUserBehaviorFunc(time,type)),by=user_id]
tmpdf <- do.call(rbind,str_split(result$value,'#'))
colnames(tmpdf) <- varName
statsResult <- cbind(user_id = result$user_id,tmpdf)
statsResult <- data.frame(statsResult,stringsAsFactors = F)
statsResult <- charToNumFunc(statsResult)

statsResult <- statsResult[statsResult$browseNum > -1,]
statsMat <- as.matrix(statsResult)
behavorStats <- apply(statsMat[,2:6],1,sum)
sum(behavorStats<1)

behavorStats <- behavorStats %>% as.numeric 
behviorCut <- cut(behavorStats,breaks = unique(quantile(behavorStats,probs=seq(0,1,0.2),
                                                  na.rm = T)),include.lowest = T)
behviorCut <- behviorCut[!is.na(behviorCut)]
p <- ggplot(data = data.frame(x = behviorCut), mapping = aes(x = factor(x), y = ..count..)) + 
  geom_bar(stat = 'count', fill = 'steelblue', colour = 'darkgreen')
p


charToNumFunc <- function(df){
  df <- data.frame(df,stringsAsFactors = F)
  for(i in names(df)){
    df[,i] <- as.character(df[,i]) %>% as.numeric
  }
  return(df)
}


getUserBehaviorFunc <- function(time,type){
  tmpDf <- data.frame(time,type,stringsAsFactors = F)
  tmp <- which(tmpDf$type == 4)
  if(length(tmp)>0){
    tmp <- min(tmp)
    orderTime <- time[tmp]
    tmpdf <- tmpDf[tmpDf$time<orderTime,]
    browseNum <- length(which(tmpdf$type==1))
    cartNum <- length(which(tmpdf$type==2))
    deleteNum <- length(which(tmpdf$type==3))
    attenNum <- length(which(tmpdf$type==5))
    clickNum <- length(which(tmpdf$type==6))
  }else{
    browseNum <- -1
    cartNum <- -1
    deleteNum <- -1
    attenNum <- -1
    clickNum <- -1
  }
  statsResult <- cbind(browseNum,cartNum,deleteNum,attenNum,clickNum)
  varName <<- colnames(statsResult)
  statsResult <- paste0(statsResult,collapse = '#')
  return(statsResult)
}

