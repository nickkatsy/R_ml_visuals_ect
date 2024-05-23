library(dplyr)
library(boot)
library(MASS)
library(tidyverse)
library(ROCR)
library(caret)
library(ggplot2)

df <- read.csv('https://raw.githubusercontent.com/LeeMorinUCF/QMB6358S22/main/demo_24_Classification/credit.csv')

head(df,10)
str(df)
view(df)
df <- df %>%
  mutate(duration = as.factor(duration),
         purpose = as.numeric(factor(purpose)),
         status = as.numeric(factor(status)),
         housing = as.numeric(factor(housing)),
         checkingstatus1 = as.numeric(factor(checkingstatus1)),
         history = as.numeric(factor(history)),
         savings = as.numeric(factor(savings)),
         otherplans = as.numeric(factor(otherplans)),
         employ = as.numeric(factor(employ)),
         others = as.numeric(factor(others)),
         property = as.numeric(factor(property)),
         job = as.numeric(factor(job)),
         tele = as.numeric(factor(tele)),
         foreign = as.numeric(factor(foreign)),
         Default = as.factor(Default)
  )

df$Default

ggplot(df, aes(x = Default, fill = history)) +
  geom_bar() +
  theme_minimal()

ggplot(df,aes(x=property,y=Default)) +
  geom_boxplot() +
  theme_classic()

ggplot(df,aes(x=purpose,y=Default)) +
  geom_boxplot() +
  theme_classic()
str(df)
model1 <- glm(Default~purpose,data=df,family='binomial')
pred_prob <- predict.glm(model1,type=c('response'))
predictions <- prediction(pred_prob,df$Default)



full_model <- glm(Default~.,data=df,family = 'binomial')
pred_prob2 <- predict.glm(full_model,type = c('response'))
roc <- performance(prediction(pred_prob2, df$Default), "tpr", "fpr")
unlist(slot(performance(prediction(pred_prob2, df$Default), "auc"), "y.values"))
plot(roc,colorize=TRUE)

