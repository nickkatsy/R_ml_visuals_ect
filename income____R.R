library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ROCR)
library(pROC)
#discount python
#goofy syntax and slow

df <- read.csv('C:/ML/python/data/income.csv',sep=',')
head(df,10)
unique(df)
str(df)
is.na(str(colnames(df)))
is.factor(str((colnames(df))))

df <- df %>%
  mutate(
    sex = ifelse(sex == 'Male', 1, 0),
    income = ifelse(income == '<=50K', 1, 0),
    race = ifelse(race == 'White', 1, 0),
    native.country = as.numeric(factor(native.country)),
    relationship = as.numeric(factor(relationship)),
    workclass = as.numeric(factor(workclass)),
    marital.status = as.numeric(factor(marital.status)),
    education = as.numeric(factor(education))
  )


head(df)
str(df)


ggplot(df, aes(x = factor(sex), fill = factor(income))) +
  geom_bar() +
  labs(x = "Sex", y = "Count", fill = "Income") +
  theme_minimal()

ggplot(df, aes(x = factor(income), y = education)) +
  geom_boxplot() +
  labs(x = 'Income', y = 'Education') +
  theme_minimal()

ggplot(df,aes(x = education,fill=factor(race))) +
  geom_bar() +
  labs(x='Education',y='Race') +
  theme_classic()


#like glms or something

model_1 <- glm(df$income~.,data=df,family='binomial')
pred_prob <- predict.glm(model_1,type=c('response'))
model_summary_1 <- summary(model_1)
print(model_summary_1$coefficients)


pred_prob <- prediction(pred_prob, df$income)


unlist(slot(performance(pred_prob, "auc"), "y.values"))

