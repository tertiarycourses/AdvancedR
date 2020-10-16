library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

# Covariance and Correlation
df<-data.frame(X=c(90,90,60,60,30),Y=c(60,90,60,60,30))
cov(df)
cor(df)

# Activity: Covariance and Correlation
heart<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                   "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
heart %>%
  select(age,chol,fbs,thalach,exang) %>%
  cov() 

heart %>%
  select(age,chol,fbs,thalach,exang) %>%
  cor() 

# Linear Regression
x <- 1:5
y <- c(1.3,4.3,5.5,8.4,14.2)
plot(x,y)

m <- lm(y~x)
summary(m)
predict(m,data.frame(x=6))
abline(m)

# Activitiy Liear Regression
age <- heart$age
chol <- heart$chol
plot(age,chol)

m <- lm(chol~age)
predict(m,data.frame(age=60))
abline(m)

# Multivariate Regression
m <- lm(mpg~wt+hp,data=mtcars)

# Activity: Multivariate Regression
age <- heart$age
chol <- heart$chol
thalach<- heart$thalach

m <- lm(chol~age+thalach)
predict(m,data.frame(age=60,thalach=180))

# Hypothesis Testing
boxplot(extra~group,data=sleep)
t.test(extra~group,data=sleep)
t.test(extra~group,data=sleep,alternative="less")

# Activity : Hypothesis Testing
boxplot(weight~feed,data=chickwts)
d <- subset(chickwts,feed == "casein" | feed =="horsebean")
t.test(weight~feed,data=d)
t.test(weight~feed,data=chickwts.test,alternative='less')
t.test(weight~feed,data=chickwts.test,alternative='greater')

# ANOVA

m <- aov(weight~feed,data=chickwts)
summary(m)

# Activity : Hypothesis Testing
shampoo = data.frame('A'=c(36.6,39.2,30.4,37.1,34.1),'B' = c(17.5,20.6,18.7,25.7,22.0),'C'=c(15.0,10.4,18.9,10.5,15.2))

shampoo <- as_tibble(shampoo)

shampoo %>%
  gather(brand, effect) %>%
  boxplot(effect~brand,.)

shampoo %>%
  gather(brand, effect) %>%
  aov(effect~brand,.)


iris