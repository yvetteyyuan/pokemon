pkdf <- read.csv('Pokemon.csv')
summary(pkdf)

pk_logit_model <- glm(Legendary~Attack+Sp_Atk+Sp_Def+HP,data=pkdf,family='binomial')
summary(pk_logit_model)

(exp(0.029982)-1)*100
#attack increas by 1 pt, chance of lengendary increases by 3.04%

library(rpart)
pk_tree_model<-rpart(Legendary ~ Attack + Sp_Atk + Sp_Def + HP, data = pkdf,
                     method='class')
rpart.plot(pk_tree_model,type=1,extra=1,box.palette=c('pink','green'),branch.lty=3,
           shadow.col='gray')
summary(pk_tree_model)

library(ROCR)
pk_predict_logit<-predict(pk_logit_model,pkdf,type='response')
print(pk_predict_logit)
#103 this pokemon 31.9% being legendary, low chance being legendary
pk_predict_logit[103]
#this pokemon 98.3% being legendary, high chance being legendary
pk_predict_logit[799]

pk_predict_tree <- predict(pk_tree_model,pkdf,type='prob')
print(pk_predict_tree)

pk_logit_prediction<-prediction(pk_predict_logit,pkdf$Legendary)  
pk_tree_prediction<-prediction(pk_predict_tree[,2],pkdf$Legendary)  #there are two cols, so [,2]

pk_performance_logit<-performance(pk_logit_prediction,'tpr','fpr')
pk_performance_tree<-performance(pk_tree_prediction,'tpr','fpr')

plot(pk_performance_logit,col='blue',lty=3,lwd=3)
plot(pk_performance_tree,col='black',lty=3,lwd=3,add=T)
#the logistic model has more accurate predictions for a pokemon being legendary because it has greater area under the curve
# in the low false positive rate area.
