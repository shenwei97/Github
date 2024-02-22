if(!require("timeROC")) install.packages("timeROC")
library("timeROC")



train_set$lp <- predict(cox3, newdata = train_set,type ="lp")

time_roc_train <- timeROC(
  T = train_set$TEVENT,  #结局时间
  delta = train_set$Status,  #生存结局 
  marker = train_set$lp,  #预测变量
  cause = 1,  #阳性结局指标值
  weighting="marginal",  #权重计算方法，默认marginal，采用KM计算删失分布
  times = c(3, 5, 7),
  ROC = TRUE,  #保存sensitivities 和 specificties值
  iid = F  #iid = TRUE 才会保存置信区间，但是样本量大了后，耗时耗资源
)

plot(title="",time_roc_train,col="DodgerBlue",time=3,lty=1,lwd = 2,family = "serif")
plot(time_roc_train,time=5,lty=1,lwd = 2,add=TRUE,col="LightSeaGreen",family = "serif")#add=TRUE指在前一条曲线基础上新增
plot(time_roc_train,time=7,lty=1,lwd = 2,add=TRUE,col="DarkOrange",family = "serif" )

## 有置信区间才运行下面代码

#计算train-1年的AUC
train3y <- paste0("train 3 year AUC (95%CI) = ", 
                  sprintf("%.3f",time_roc_train$AUC[1]) ," (",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[1,1]/100)," - ",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[1,2]/100),")")

#计算train-3年的AUC
train5y <- paste0("train 5 year AUC (95%CI) = ", 
                  sprintf("%.3f",time_roc_train$AUC[2]) ," (",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[2,1]/100)," - ",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[2,2]/100),")")

#计算train-5年的AUC
train10y <- paste0("train 10 year AUC (95%CI) = ", 
                  sprintf("%.3f",time_roc_train$AUC[3]) ," (",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[3,1]/100)," - ",
                  sprintf("%.3f",confint(time_roc_train, level = 0.95)$CI_AUC[3,2]/100),")")

#train AUC 多时间点合一
plot(title="",time_roc_train,col="DodgerBlue",time=12,lty=1,lwd = 2,family = "serif") #绘制ROC曲线       
plot(time_roc_train,time=12*3,lty=1,lwd = 2,add=TRUE,col="LightSeaGreen",family = "serif")#add=TRUE指在前一条曲线基础上新增
plot(time_roc_train,time=12*5,lty=1,lwd = 2,add=TRUE,col="DarkOrange",family = "serif" )
legend("bottomright",c(train1y,train3y,train5y),col=c("DodgerBlue","LightSeaGreen","DarkOrange"),lty=1,lwd=2)