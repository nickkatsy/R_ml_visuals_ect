library(dplyr)
library(boot)
library(MASS)
library(caret)
library(ROCR)


df <- read.csv('C:/ml/python/data/heart.csv',sep=',',header=TRUE)

head(df,10)

age <- df$age
sex <- factor(df$sex)
cp <- factor(df$cp)
trestbps <- df$trestbps
chol <- df$chol
fbs <- factor(df$fbs)
restecg <- factor(df$restecg)
thalach <- df$thalach
exang <- factor(df$exang)
oldpeak <- df$oldpeak
slope <- factor(df$slope)
ca <- factor(df$ca)
thal <- factor(df$thal)
target <- factor(df$target)

lm <- glm(target~polym(cp,trestbps,restecg,thalach,slope),data=df,family=binomial())
pred <- predict.glm(lm,type=c('response'))



prob_seq <- seq(0.01,1,0.01)





costfunc = function(observations, pred_prob){
  weight1 = 1               
  weight0 = 1               
  c1 <- (target==1)&(pred_prob<optimal_cutoff)
  c0 <- (target==0)&(pred_prob>=optimal_cutoff)
  cost <- mean(weight1*c1 + weight0*c0)
  return(cost)              
} 



cv_cost = rep(0, length(prob_seq))
for(i in 1:length(prob_seq)){
  optimal_cutoff = prob_seq[i]
  set.seed(123)
  cv_cost[i] = cv.glm(data=df,glmfit=lm,cost=costfunc, K=5)$delta[2]
}


plot(prob_seq,cv_cost)
optimal_cutoff_cv = prob_seq[which(cv_cost==min(cv_cost))]
optimal_cutoff_cv
min(cv_cost)

pred_prob <- predict.glm(lm,type=c('response'))


class_prediction <- ifelse(pred_prob > optimal_cutoff_cv, 1, 0)
class_prediction <- factor(class_prediction)
approve <- factor(target)

confusionMatrix(class_prediction, approve, positive="1")

class_at_opt <- (pred_prob>optimal_cutoff_cv)*1
table(class_at_opt, target, dnn = c("Predicted", "True"))



roc <- performance(prediction(pred_prob, target), "tpr", "fpr")
plot(roc, colorize=TRUE)


pred <- prediction(pred_prob, target)
unlist(slot(performance(pred, "auc"), "y.values"))





