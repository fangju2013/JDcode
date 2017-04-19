setwd('E:\\JDproject\\JData')

library(data.table)
library(dtplyr)
library(dplyr)
library(stringr)

raw.data <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)
analysisData <- raw.data
setkey(analysisData,time,type,cate)
testData <- analysisData[type%in%c(2,4)&cate==8&time>='2016-04-01']

# stats the cate is 8 and the time lag is 3,when add in cart,whether will order
# in the next 3 days 
g.p <- group_by(testData,user_id)
sumResult <- summarise(g.p,value = cartOrderFunc(cate,time,type))
sumResult <- data.frame(sumResult,stringsAsFactors = F)
tmpdf <- do.call(rbind,str_split(sumResult$value,'#')) 
colnames(tmpdf) <- c('cate','timeLag1','timeLag2')
tmpdf <- cbind(user_id = sumResult$user_id,tmpdf)
result <- data.frame(tmpdf,stringsAsFactors = F)
result <- data.table(result)
setkey(result,timeLag1,timeLag2)
result <- result[timeLag1 == 1 | timeLag2 == 1]
result

cartOrderFunc <- function(cate,time,type){
  tmpdf <- data.frame(cate = cate,time = time,type = type,stringsAsFactors = F)
  cateGroup <- group_by(tmpdf,cate)
  result <- summarise(cateGroup,value = tmpFunc(type,time))
  result <- data.frame(result,stringsAsFactors = F)
  result <- paste(result$cate,result$value,sep = '#')
  result <- paste(result,collapse = '/')
  return(result)
}

# stats whether will order in the next window according to add the cart   
tmpFunc <- function(type,time,win = 5,cutGroup = c(1,2,3)){
  time <- as.Date(time)
  end <- max(time)
  tmpdf <- data.frame(type = type,time = time)
  cutTime <- c()
  for(i in cutGroup){
    tmptime <- time[time > end - i*win & time <= end - (i-1)*win]
    tmp <- c(rep(i,length(tmptime)))
    cutTime <- append(cutTime,tmp)
  }
  tmpdf$cutTime <- cutTime
  cartOrderCount <- c()

  for(j in cutGroup[-length(cutGroup)]){
    cutTmpdf <- tmpdf[tmpdf$cutTime == j,]
    type2 <- which(cutTmpdf$type == 2)
    cutNextTmpdf <- tmpdf[tmpdf$cutTime == (j+1),]
    type4 <- which(cutNextTmpdf$type == 4)
    type24 <- intersect(type2,type4)
    tmp <- if_else(length(type24)>0,1,0)
    cartOrderCount <- append(cartOrderCount,tmp)
  }
  result <- paste(cartOrderCount,collapse = '#')
  return(result)
}




