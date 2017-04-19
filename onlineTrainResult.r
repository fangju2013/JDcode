rm(list=ls());gc()
resultPath <- 'E:/JDproject/JDstatsAnalysisResult/featureConstruct/'


load(paste0(resultPath,"offLineTrainData.RData"))
load(paste0(resultPath,"offLineTestData.RData"))
load(paste0(resultPath,"onLineTestData.RData"))

train.data <- rbind(offLineTrainData,offLineTestData)
test.data <- onLineTestData

train.data[is.na(train.data)] <- 0
test.data[is.na(test.data)] <- 0

#抽样
sampleID<-strata(train.data,stratanames = "flag",size = c(2281*9,2281),method = "srswor")
train.data<-train.data[rownames(train.data)%in%sampleID$ID_unit,]

test.data$cate<-as.factor(test.data$cate)
train.data$cate<-as.factor(train.data$cate)

factorVar<-colnames(train.data)[sapply(train.data,is.factor)]

delete<-c("user_id","sku_id","brand")

train.data<-setDF(train.data[,c(4:368)])
test.data<-setDF(test.data[,c(4:367)])

#稀疏化
sparse_train_data<-cbind(data.frame(as.matrix(sparse.model.matrix(~.-1,train.data[,c("cate","age","sex","user_lv_cd")]))),
                         train.data[,colnames(train.data)[!colnames(train.data)%in%c("cate","age","sex","user_lv_cd")]])
colnames(sparse_train_data)

sparse_test_data<-cbind(data.frame(as.matrix(sparse.model.matrix(~.-1,test.data[,c("cate","age","sex","user_lv_cd")]))),
                        test.data[,colnames(test.data)[!colnames(test.data)%in%c("cate","age","sex","user_lv_cd")]])

dtrain=xgb.DMatrix(data = as.matrix(sparse_train_data[,-22]),label=sparse_train_data$flag)
dtest=xgb.DMatrix(data = as.matrix(sparse_test_data))



#train the model
onxgb<-xgb.train(booster = "gbtree",
                 objective="binary:logistic",
                 eta=0.1,
                 nrounds = 417,
                 gamma=0,
                 max_depth=5,
                 subsample=0.8,
                 colsample=0.8,
                 lambda=800,
                 eval_metric="auc",
                 data = dtrain,
                 verbose = 1,
                 scale_pos_weight=9
)

xgb.importance(colnames(sparse_train_data),model=onxgb)

cvxgb<-xgb.cv(
  booster = "gbtree",
  objective="binary:logistic",
  eta=0.1,
  nrounds = 1000,
  gamma=0,
  max_depth=5,
  subsample=0.8,
  colsample=0.8,
  lambda=800,
  eval_metric="auc",
  data = dtrain,
  verbose = 1,
  scale_pos_weight=9,
  early_stopping_rounds = 50,
  nfold = 5
)


pred<-predict(onxgb,dtest)

load(paste0(resultPath,"onLineTestData.RData"))
colnames(onLineTestData)
result<-arrange(cbind(onLineTestData,pred)[cate==8,c(1,2,3,4,368)],-pred)
result<-distinct(result,user_id,.keep_all = T)
head(result[c(1:1000),],180)

result0417e <- head(result[,c(1,2)],800)
write.csv(result0417e,file=paste0('E:/JDproject/JDpredResult/',"result0417e.csv"),row.names = F)
