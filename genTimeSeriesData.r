setwd('E:\\JDproject\\JData')
resultPath <- 'E:\\JDproject\\JDstatsAnalysisResult\\userBehaviorTimeSerieResult\\'

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(lubridate)

raw.data1 <- fread('JData_Action_201602.csv',header = T,stringsAsFactors = F)
raw.data2 <- fread('JData_Action_201603.csv',header = T,stringsAsFactors = F)
raw.data3 <- fread('JData_Action_201603_extra.csv',header = T,stringsAsFactors = F)
raw.data4 <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)

tmptime1 <- str_sub(raw.data1[,time],1,10)
tmptime2 <- str_sub(raw.data2[,time],1,10)
tmptime3 <- str_sub(raw.data3[,time],1,10)
tmptime4 <- str_sub(raw.data4[,time],1,10)

analysisData1 <- raw.data1[,time:=tmptime1]
analysisData2 <- raw.data2[,time:=tmptime2]
analysisData3 <- raw.data3[,time:=tmptime3]
analysisData4 <- raw.data4[,time:=tmptime4]

analysisData.l <- list(analysisData1,analysisData2,analysisData3,analysisData4)
analysisData <- rbindlist(analysisData.l)
result <- analysisData[,.(value=cateTypeNumFunc(type,cate)),by=time]
result <- setorder(result,time)

# result1 <- analysisData1[,.(value=cateTypeNumFunc(type,cate)),by=time]
# result2 <- analysisData2[,.(value=cateTypeNumFunc(type,cate)),by=time]
# result3 <- analysisData3[,.(value=cateTypeNumFunc(type,cate)),by=time]
# result4 <- analysisData4[,.(value=cateTypeNumFunc(type,cate)),by=time]
# result.l <- list(result1,result2,result3,result4)
# result <- rbindlist(result.l)
# result <- setorder(result,time)

tmpdf <- do.call(rbind,str_split(result$value,'#'))
colnames(tmpdf) <- resName
statsRes <- cbind(time=result$time,tmpdf)
statsRes <- data.frame(statsRes,stringsAsFactors = F)
statsRes <- charToNumFunc(statsRes)
statsRes <- data.table(statsRes,stringsAsFactors = F)


win = 5
tmpRes <- c()
for(i in nrow(statsRes):(2*win)){
  tmp <- statsRes[(i-2*win+1):(i-win),3:ncol(statsRes)]
  tmp <- t(as.matrix(tmp)) %>% as.vector() %>% unlist()
  tmp <- c(unlist(as.vector(as.matrix(statsRes[i,1:2]))),tmp)
  tmpRes <- rbind(tmpRes,tmp)
}
tmpResName <- c()
for(i in 1:win){
  tmp <- paste0('time',i,resName[2:length(resName)])
  tmpResName <- c(tmpResName,tmp)
}
tmpResName <- c('time','targ',tmpResName)
colnames(tmpRes) <- tmpResName
tmpRes <- data.frame(tmpRes,stringsAsFactors = F)

write.csv(tmpRes,file=paste0(resultPath,'tmpRes.csv'),fileEncoding='gbk',row.names=F)


charToNumFunc <- function(df){
  for(i in names(df)[-1]){
    df[,i] <- as.character(df[,i]) %>% as.numeric()
  }
  return(df)
}

cateTypeNumFunc <- function(type,cate){
  typeCate14 <- sum(type == 1 & cate == 4)
  typeCate24 <- sum(type == 2 & cate == 4)
  typeCate34 <- sum(type == 3 & cate == 4)
  typeCate44 <- sum(type == 4 & cate == 4)
  typeCate54 <- sum(type == 5 & cate == 4)
  typeCate64 <- sum(type == 6 & cate == 4)
  
  typeCate15 <- sum(type == 1 & cate == 5)
  typeCate25 <- sum(type == 2 & cate == 5)
  typeCate35 <- sum(type == 3 & cate == 5)
  typeCate45 <- sum(type == 4 & cate == 5)
  typeCate55 <- sum(type == 5 & cate == 5)
  typeCate65 <- sum(type == 6 & cate == 5)
  
  typeCate16 <- sum(type == 1 & cate == 6)
  typeCate26 <- sum(type == 2 & cate == 6)
  typeCate36 <- sum(type == 3 & cate == 6)
  typeCate46 <- sum(type == 4 & cate == 6)
  typeCate56 <- sum(type == 5 & cate == 6)
  typeCate66 <- sum(type == 6 & cate == 6)
  
  typeCate17 <- sum(type == 1 & cate == 7)
  typeCate27 <- sum(type == 2 & cate == 7)
  typeCate37 <- sum(type == 3 & cate == 7)
  typeCate47 <- sum(type == 4 & cate == 7)
  typeCate57 <- sum(type == 5 & cate == 7)
  typeCate67 <- sum(type == 6 & cate == 7)
  
  typeCate18 <- sum(type == 1 & cate == 8)
  typeCate28 <- sum(type == 2 & cate == 8)
  typeCate38 <- sum(type == 3 & cate == 8)
  typeCate58 <- sum(type == 5 & cate == 8)
  typeCate68 <- sum(type == 6 & cate == 8)
  
  typeCate19 <- sum(type == 1 & cate == 9)
  typeCate29 <- sum(type == 2 & cate == 9)
  typeCate39 <- sum(type == 3 & cate == 9)
  typeCate49 <- sum(type == 4 & cate == 9)
  typeCate59 <- sum(type == 5 & cate == 9)
  typeCate69 <- sum(type == 6 & cate == 9)
  
  typeCate110 <- sum(type == 1 & cate == 10)
  typeCate210 <- sum(type == 2 & cate == 10)
  typeCate310 <- sum(type == 3 & cate == 10)
  typeCate410 <- sum(type == 4 & cate == 10)
  typeCate510 <- sum(type == 5 & cate == 10)
  typeCate610 <- sum(type == 6 & cate == 10)
  
  typeCate111 <- sum(type == 1 & cate == 11)
  typeCate211 <- sum(type == 2 & cate == 11)
  typeCate311 <- sum(type == 3 & cate == 11)
  typeCate411 <- sum(type == 4 & cate == 11)
  typeCate511 <- sum(type == 5 & cate == 11)
  typeCate611 <- sum(type == 6 & cate == 11)
  
  targ <- sum(type == 4 & cate == 8)
  
  result <- cbind(targ,typeCate14,typeCate24,typeCate34,typeCate44,typeCate54,typeCate64,
                  typeCate15,typeCate25,typeCate35,typeCate45,typeCate55,typeCate65,
                  typeCate16,typeCate26,typeCate36,typeCate46,typeCate56,typeCate66,
                  typeCate17,typeCate27,typeCate37,typeCate47,typeCate57,typeCate67,
                  typeCate18,typeCate28,typeCate38,typeCate58,typeCate68,
                  typeCate19,typeCate29,typeCate39,typeCate49,typeCate59,typeCate69,
                  typeCate110,typeCate210,typeCate310,typeCate410,typeCate510,typeCate610,
                  typeCate111,typeCate211,typeCate311,typeCate411,typeCate511,typeCate611)
  
  resName <<- colnames(result)
  result <- paste0(result,collapse='#')
  return(result)
} 


