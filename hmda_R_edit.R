library(dplyr)
library(boot)
library(MASS)
library(caret)
library(ROCR)
library(quantmod)
library(stargazer)


hmda_data <- read.csv('C:/ml/R/projects/hmda_sw.txt',sep='\t',header=TRUE)




#Data Cleaning


occupancy <- as.factor(hmda_data$s5)
occupancy <- ifelse(occupancy == 1,1,0)
occupancy <- factor(occupancy)

approve <- ifelse(hmda_data$s7 == 3,0,1)
approve <- as.factor(approve)




county <- factor(hmda_data$s11)

hmda_data$s13 <- ifelse(hmda_data$s13 == 5,1,0)
race <- factor(hmda_data$s13)



hmda_data$s15 <- ifelse(hmda_data$s15 == 1,1,0)
gender <- factor(hmda_data$s15)

income <- as.numeric(hmda_data$s17)


married <- hmda_data$s23a
married <- as.factor(married)

maritial_status <- ifelse(married == 'M',1,0)
maritial_status[is.na(maritial_status)] <- 0
maritial_status <- factor(maritial_status)


self_employed <- factor(hmda_data$s27a)


monthly_income <- as.numeric(hmda_data$s31a)


purchase_price <- as.numeric(hmda_data$s33)

liquid_assets <- as.numeric(hmda_data$s35)




credit_history <- factor(hmda_data$s40)
credit_history <- ifelse(credit_history == 1,1,0)
summary(credit_history)



chmp<- factor(hmda_data$s42)



chcp <- factor(hmda_data$s43)


chpr <- as.factor(hmda_data$s44)


debt_to_expense <- as.numeric(hmda_data$s45)

di_ratio <- as.numeric(hmda_data$s46)


pmi_denied <- factor(hmda_data$s53)



net_worth <- as.numeric(hmda_data$netw)

education <- as.numeric(hmda_data$school)

unemployment <- hmda_data$uria

pmi_sought <- factor(hmda_data$s52)

unverifiable <- factor(hmda_data$s56)

summary(hmda_data)

# summary statistics


lm <- glm(approve~di_ratio,data=hmda_data,family=binomial)

di_pred <- predict.glm(lm,link=c('response'))

plot(di_ratio,di_pred,
     xlab = 'Di Ratio',
     ylab = 'Prediction',
     type='p',
     pch=20)


county_lm <- glm(approve~county,data=hmda_data,family=binomial)

county_pred <- predict.glm(county_lm,link=c('response'))



lm_race <- glm(approve~race,data=hmda_data,family=binomial)

pred_race <- predict.glm(lm_race,link=c('response'))

histogram(race,pred_race,
          type='p',
          xlab = '0 = Black, 1= White',
          ylab = 'Pred',
          pch=20)




lm_gender <- lm(approve~gender,data=hmda_data,family=binomial)

pred_gender <- predict.glm(lm_gender,link=c('response'))


histogram(gender,pred_gender,
          type='p',
          xlab='1=Male, 0=Female',
          ylab='Predictions',
          pch=20)



lm_income <- glm(approve~income,data=hmda_data,family=binomial)
pred_income <- predict.glm(lm_income,link=c('response'))





lm_maritial_status <- glm(approve~maritial_status,data=hmda_data,family=binomial)

pred_maritial_status <- predict.glm(lm_maritial_status,link=c('response'))




boxplot(maritial_status, pred_maritial_status,
        type = "p",
        xlab = "Not_married=0, Married=1",
        ylab = "Predicted Probability of Approval",
        pch = 20)




lm_self_emplyed <- glm(approve~self_employed,data=hmda_data,family=binomial)

pred_self_employed <- predict.glm(lm_self_emplyed,link=c('response'))

plot(self_employed,pred_self_employed,
     type='p',
     xlab='Not Self Employed = 0, Self_Employed = 1',
     ylab= 'Pred',
     pch=19)




lm_monthly_income <- glm(approve~monthly_income,data=hmda_data,family=binomial)

pred_monthly <- predict.glm(lm_monthly_income,link=c('response'))


scatter.smooth(monthly_income,pred_monthly,
               type='p',
               xlab = 'Monthly Income',
               ylab = 'Predictions',
               pch=20)






lm_dte <- glm(approve~debt_to_expense,data=hmda_data,family=binomial)

pred_debt_to_expense <- predict.glm(lm_dte,link=('response'))


plot(debt_to_expense, pred_debt_to_expense,
     type = "l",
     xlab = "Debt_to_Expense",
     ylab = "Predicted Probability of Approval",
     col = "blue",
     pch = 20)



lm_chmp <- glm(approve~chmp,data=hmda_data,family=binomial)
pred_chmp <- predict.glm(lm_chmp,link=('response'))


boxplot(chmp, pred_chmp,
        type = "p",
        xlab = "No Missed Payments = 0,One or More Missed Payments = 1 ",
        ylab = "Predicted Probability of Approval",
        pch = 20)





lm_credit_history <- glm(approve~credit_history,data=hmda_data,family=binomial)

pred_cred_history <- predict.glm(lm_credit_history,link=c('response'))


plot(credit_history,pred_cred_history,
     type='l',
     xlab = 'Approved = 0,Applicant does not meet Guidelines = 1 ',
     ylab='Predictions',
     pch=20)



lm_chcp <- glm(approve~chcp,data=hmda_data,family=binomial)
pred_chcp <- predict.glm(lm_chcp,link=c('response'))


plot(chcp,pred_chcp,
     type='p',
     xlab = 'Applicant has small or no deliquent accounts=0,applicant has >=1 violations = 3.5:6',
     ylab='pred',
     pch=20)

lm_di_ratio <- glm(approve~di_ratio,data=hmda_data,family=binomial)
pred_di_ratio <- predict.glm(lm_di_ratio,link=('response'))


scatter.smooth(di_ratio,pred_di_ratio,
               type='p',
               xlab='Di_ratio in Dollars',
               ylab='Predictions',
               pch=20)


lm_pmi <- glm(approve~pmi_denied,data=hmda_data,family=binomial)

pred_pmi <- predict.glm(lm_pmi,link=('response'))


plot(pmi_denied,pred_pmi,
     type='p',
     xlab= 'PMI Approved = 0, PMI Denied = 1',
     ylab='pred',
     pch=20)



lm_netw <- glm(approve~net_worth,data=hmda_data,family=binomial)
pred_netw <- predict.glm(lm_netw,link=c('response'))


plot(net_worth,pred_netw,
     type='p',
     xlab='Applicant Net Worth in Dollars',
     ylab='predictions',
     pch=20)

lm_education <- glm(approve~education,data=hmda_data,family=binomial)
pred_education <- predict.glm(lm_education,link=('response'))


scatter.smooth(education, pred_education,
               type = "l",
               xlab = "Level of Education Attained",
               ylab = "Pred",
               pch = 20)


lm_unemployment <- glm(approve~unemployment,data=hmda_data,family=binomial)
pred_unemployment <- predict.glm(lm_unemployment,link=c('response'))


plot(unemployment,pred_unemployment,
     type='p',
     xlab='Unemployment Probability',
     ylab='Pred',
     pch=18)



lm_assets <- glm(approve~liquid_assets,data=hmda_data,family=binomial)
pred_assets <- predict.glm(lm_assets,link=('response'))

scatter.smooth(liquid_assets, pred_assets,
               type = "p",
               xlab = "Liquid Assets in Thousands",
               ylab = "Predicted Probability of Approval",
               pch = 20)






lm_chpr <- glm(approve~chpr,data=hmda_data,family=binomial)

pred_chpr <- predict.glm(lm_chpr,link=c('response'))

scatter.smooth(chpr,pred_chpr,
               type='p',
               xlab = 'Credit History Public Records',
               ylab='Predicted Probablilities',
               pch=20)



lm_unverifiable <- glm(approve~unverifiable,data=hmda_data,family=binomial)


pred_unverifiable <- predict.glm(lm_unverifiable,link=c('response'))


plot(unverifiable,pred_unverifiable,
     xlab = 'Unverifiable Infomation',
     ylab = 'Predictions',
     pch= 20)


lm_pmi_sought <- glm(approve~pmi_sought,data=hmda_data,family=binomial)

pred_pmi_sought <- predict.glm(lm_pmi_sought,link=c('response'))

plot(pmi_sought,pred_pmi_sought,
     type = 'p',
     xlab = 'Private Mortgage Insurance Requested',
     ylab = 'Predictions of Approval',
     pch=20)



#ROC and AUC


df1 <- data.frame(x=polym(occupancy,race,gender,income,maritial_status,credit_history,di_ratio,pmi_denied,unverifiable,pmi_sought,degree=1),y=approve)
model1 <- glm(approve~polym(occupancy,race,gender,income,maritial_status,credit_history,di_ratio,pmi_denied,unverifiable,pmi_sought,degree=1),data=hmda_data,family=binomial)




pred_prob <- predict.glm(model1,type=c('response'))


roc <- performance(prediction(pred_prob, approve), "tpr", "fpr")
plot(roc, colorize=TRUE)


pred <- prediction(pred_prob, approve)
unlist(slot(performance(pred, "auc"), "y.values"))

