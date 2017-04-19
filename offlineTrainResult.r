rm(list=ls());gc()
resultPath <- 'E:/JDproject/JDstatsAnalysisResult/featureConstruct/'

load(paste0(resultPath,"offLineTrainData.RData"))
load(paste0(resultPath,"offLineTestData.RData"))

offLineTestData[is.na(offLineTestData)]<-0
offLineTrainData[is.na(offLineTrainData)]<-0

dim(offLineTrainData)
dim(offLineTestData)

table(offLineTrainData$flag)

#抽样

sampleID<-strata(offLineTrainData,stratanames = "flag",size = c(1712*9,1712),method = "srswor")
offLineTrainData<-offLineTrainData[rownames(offLineTrainData)%in%sampleID$ID_unit,]

offLineTestData$cate<-as.factor(offLineTestData$cate)
offLineTrainData$cate<-as.factor(offLineTrainData$cate)



factorVar<-colnames(offLineTrainData)[sapply(offLineTrainData,is.factor)]

levels(offLineTestData$age)<-levels(offLineTrainData$age)

delete<-c("user_id","sku_id","brand")

offLineTrainData<-setDF(offLineTrainData[,c(4:368)])
offLineTestData<-setDF(offLineTestData[,c(4:368)])

#稀疏化

sparse_train_data<-cbind(data.frame(as.matrix(sparse.model.matrix(~.-1,offLineTrainData[,c("cate","age","sex","user_lv_cd")]))),
                         offLineTrainData[,colnames(offLineTrainData)[!colnames(offLineTrainData)%in%c("cate","age","sex","user_lv_cd")]])


sparse_test_data<-cbind(data.frame(as.matrix(sparse.model.matrix(~.-1,offLineTestData[,c("cate","age","sex","user_lv_cd")]))),
                        offLineTestData[,colnames(offLineTestData)[!colnames(offLineTestData)%in%c("cate","age","sex","user_lv_cd")]])

colnames(sparse_train_data)


dtrain=xgb.DMatrix(data = as.matrix(sparse_train_data[,-22]),label=sparse_train_data$flag)
dtest=xgb.DMatrix(data = as.matrix(sparse_test_data[,-22]),label=sparse_test_data$flag)



#

offxgb<-xgb.train(booster = "gbtree",
                  objective="binary:logistic",
                  eta=0.1,
                  nrounds = 400,
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

xgb.importance(colnames(sparse_train_data),model=offxgb)

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


pred<-predict(offxgb,dtest)

load(paste0(resultPath,"offLineTestData.RData"))
result<-arrange(cbind(offLineTestData,pred)[cate==8,c(1,2,3,4,5,369)],-pred)
result<-distinct(result,user_id,.keep_all = T)


result[c(1:131),]
table(head(result,100)$flag)



#评测

evaluation<-function(xx_da,xx_pred){
  ngtu<-length(intersect(xx_da$user_id,xx_pred$user_id))
  ngtus<-length(intersect(paste(xx_da$user_id,xx_da$sku_id),paste(xx_pred$user_id,xx_pred$sku_id)))
  nda<-nrow(xx_da)
  npred<-nrow(xx_pred)
  p11<-ngtu/npred;r11<-ngtu/nda;f11<-6*r11*p11/(5*r11+p11)
  p12<-ngtus/npred;r12<-ngtus/nda;f12<-5*r12*p12/(2*r12+3*p12)
  print(paste("p11:",p11,"r11:",r11,"f11:",f11))
  print(paste("p12:",p12,"r12:",r12,"f12:",f12))
  print(paste("score:",0.4*f11+0.6*f12))
  
}



xx_da<-offLineTestData[flag==1&cate==8,c(1:2)]
xx_pred<-head(result[,c(1:2)],131)

evaluation(xx_da,xx_pred)

tmp<-c()
for(i in 1:500){
  xx_pred<-head(result[,c(1:2)],i)
  t<-evaluation(xx_da,xx_pred)
  tmp<-c(tmp,t)
}

which(max(tmp[c(-1,-2)])==tmp)

evaluation(xx_da,xx_pred)
