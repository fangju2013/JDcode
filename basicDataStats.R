setwd('E:\\JDproject\\JData\\newData')

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(knitr)
library(lubridate)

#get the basic data concluding the user behavior data,comment data,product data and userinfo data

raw.data1 <- fread('JData_Action_201602.csv',header = T,stringsAsFactors = F)
raw.data2 <- fread('JData_Action_201603.csv',header = T,stringsAsFactors = F)
raw.data3 <- fread('JData_Action_201604.csv',header = T,stringsAsFactors = F)
commentData <- fread('JData_Comment.csv',header = T,stringsAsFactors = F)
productData <- fread('JData_Product.csv',header = T,stringsAsFactors = F)
userInfo <- fread('JData_User.csv',header = T,stringsAsFactors = F)

userBehaviorData.l <- list(raw.data1,raw.data2,raw.data3)
userBehaviorData <- rbindlist(userBehaviorData.l)
rm(raw.data1,raw.data2,raw.data3)
gc()

#basic user info summary 
userInfo$user_lv_cd<-userInfo$user_lv_cd%>%as.character%>%as.factor
userInfo$sex<-userInfo$sex%>%as.character%>%as.factor
userInfo$user_reg_tm<-userInfo$user_reg_tm%>%as.character%>%as.Date
userInfo$age<-userInfo$age%>%as.character%>%as.factor
userInfo%>%summary()
 #detect the user_id is unique or not 
userInfo$user_id%>%length == userInfo$user_id%>%unique%>%length
 #data sample
userInfo %>%head%>%kable
 #variable interactive plot
p <- ggplot(userInfo,aes(user_lv_cd))
p + geom_bar(aes(fill=age),position = 'fill') + facet_grid(sex~.,scales = 'free') 

p <- ggplot(userInfo,aes(sex))
p + geom_bar(aes(fill=age),position = 'fill') + facet_grid(user_lv_cd~.,scales = 'free')

#comments info  
commentData$dt[commentData$dt=='NULL'] <- NA
commentData$dt <- commentData$dt%>%as.Date
commentData$has_bad_comment<-commentData$has_bad_comment%>%as.character%>%as.factor
commentData$comment_num<-commentData$comment_num%>%as.character%>%as.factor
commentData$bad_comment_rate<-commentData$bad_comment_rate%>%as.numeric
commentData%>%summary
 #detect the sku_id id unique or not
commentData$sku_id%>%length == commentData$sku_id%>%unique%>%length
commentData$sku_id%>%unique%>%length       #总共46546个不同的sku_id
commentData%>%head%>%kable
 #variable interactive plot
ggplot(commentData, aes(bad_comment_rate, ..density.., colour = comment_num)) +
  geom_freqpoly(linetype = 1,size = 1)
