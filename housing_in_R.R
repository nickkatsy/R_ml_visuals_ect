library(dplyr)
library(boot)
library(MASS)
library(caret)
library(stargazer)



df <- read.csv('https://raw.githubusercontent.com/nickkas/python_ml_ect_/master/Housing.csv',sep=',',header = TRUE)


head(df,10)

mainroad <- factor(df$mainroad)
mainroad <- ifelse(mainroad == 'yes',1,0)

guestroom <- factor(df$guestroom)
guestroom <- ifelse(guestroom == 'no',0,1)

basement <- factor(df$basement)
basement <- ifelse(basement == 'yes',1,0)
basment <- factor(basement)

hotwaterheating <- ifelse(df$hotwaterheating == 'no',0,1)
hotwaterheating <- factor(hotwaterheating)

airconditioning <- ifelse(df$airconditioning == 'yes',1,0)
airconditioning <- factor(airconditioning)

prefarea <- ifelse(df$prefarea == 'no',0,1)
prefarea <- factor(prefarea)

furnishingstatus <- factor(df$furnishingstatus)

furnishingstatus <- factor(furnishingstatus,levels = c('unfurnished','semi-furnished','furnished'))
furnishingstatus <- as.numeric(furnishingstatus)
furnishingstatus <- factor(furnishingstatus)


lm1 <- lm(df$price~df$area)
stargazer(lm1,type='text')

pred <- predict(lm1,link=c('response'))

summary(pred)

scatter.smooth(df$area,df$price,
               xlab = 'Area',
               ylab= 'Price')


scatter.smooth(pred,df$price,
               xlab = 'predicted price',
               ylab = 'price')


lm2 <- lm(df$price~df$parking)

stargazer(lm2,type='text')

pred_parking <- fitted.values(lm2)

plot(pred_parking,df$price)


model <- lm(df$price~polym(df$area,df$parking,furnishingstatus,basement,prefarea,df$area,df$stories
                      ,hotwaterheating,guestroom,airconditioning,mainroad,degree=4,raw = T))
                      

summary(model)

pred_model <- predict(model,link=c('response'))

plot(pred_model,df$price)







