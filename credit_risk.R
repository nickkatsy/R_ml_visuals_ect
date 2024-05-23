library(ROCR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(MASS)
library(boot)



df <- read.csv('https://raw.githubusercontent.com/nickkatsy/python_ml_ect_/master/credit_risk.csv',sep = ',')

head(df,10)
str(df)
colnames(df)
colnames(is.character(df))
class(is.character(names(df)))

df <- df %>%
  mutate(
    Home = as.numeric(factor(Home)),
    Intent = as.numeric(factor(Intent)),
    Default = ifelse(Default == 'Y',1,0),
  )


colnames(df)
str(df)
#ggplot2 


ggplot(df, aes(x = factor(Default), fill = factor(Home))) +
  geom_bar() +
  labs(x = 'Default(1 == Yes, 0 == No)', y = 'Count') +
  theme_classic()


ggplot(df,aes(x=factor(Default),y=Amount)) +
  geom_boxplot() +
  theme_classic()

ggplot(df,aes(x=factor(Default),y=Age)) +
  geom_boxplot() +
  theme_classic()

#just discount python, such a terrible '"langauge"'
#This feels like switchinf from keyboard and mouse back to controller


df1 <- df[, !names(df) %in% 'Id', drop = FALSE]
df1[is.na(df1)] <- mean(is.na(colnames(df1)))

#ok, lets go I guess


model1 <- glm(Default ~ Income + Home, data = df1, family = binomial)
pred <- predict.glm(model1, type = "response")
pred_prob <- prediction(pred, df$Default)

auc <- unlist(slot(performance(pred_prob, "auc"), "y.values"))
print(auc)

model2 <- glm(Default~.,data=df1,family = 'binomial')
pred2 <- predict.glm(model2,type="response")

pred_prob2 <- prediction(pred2,df$Default)
auc2 <- unlist(slot(performance(pred_prob2,"auc"),"y.values"))
print(auc2)
