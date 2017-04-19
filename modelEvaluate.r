library(dplyr)
library(MLmetrics)


#训练数据的效果评估方法：

cv_pred <- seq(0,1,0.002)
cv_pred <- sample(cv_pred,200,replace = F)
predTrue <- data.frame(cv_pred=cv_pred)
flag <- ifelse(cv_pred > 0.7,1,0) 
predTrue$flag <- flag

cv_pred <- predTrue$cv_pred
flagOverdue <- predTrue$flag

qq = quantile(cv_pred, probs=seq(0,1,0.05))
yy = rep(NA,20)
xx = rep(NA,20)
for (i in c(1:20)) {
  yy[i] = mean(flagOverdue[cv_pred<=qq[i+1]])
  xx[i] = mean(cv_pred<=qq[i+1])
}
yy[is.na(yy)]=0
plot(xx, yy, type="b", xlab="quantiles", ylab="cumulative DQ rate",main="zmLoan")

#测试效果的评估方法：

Fn<-ecdf(orig_cv_pred$cv_pred)
cv_pred1<-Fn(cv_pred) 

qq = seq(0,1,0.05)
yy = rep(NA,20)
xx = rep(NA,20)
for (i in c(1:20)) {
  yy[i] = mean(flagOverdue[cv_pred1<=qq[i+1]])
  xx[i] = mean(cv_pred1<=qq[i+1])
}
yy[is.na(yy)]=0
plot(qq[-1], yy, type="b", xlab="quantiles", ylab="cumulative DQ rate",main="msLoan")







