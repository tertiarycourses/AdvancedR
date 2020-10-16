library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)


# Mean vs Media
data <- c(3,NA,4,6,NA,10,2)
mean(data,trim=0.1)
mean(data,na.rm = TRUE)
summary(data)

heart<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                   "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

# Summary
heart <- as_tibble(heart)
summary(heart)

# Activity: Mean and Media
heart$chol %>% mean(trim=0)
heart$chol %>% mean(trim=0.1)
heart$chol %>% mean(trim=0.2)
heart$chol %>% mean(trim=0.3)

heart$chol %>% median()

# Custom Summary
heart %>%
  group_by(fbs) %>%
  summarise(
    avg_chol=mean(chol, na.rm=TRUE),
    sd_chol=sd(chol, na.rm=TRUE))

# Activity: Custom Summary
vaccination <- read_excel("vaccination.xlsx")
vaccination %>%
  group_by(vaccination_type) %>%
  mutate(dose=no_of_doses_in_thousands) %>%
  summarise(
    avg_dose=mean(dose, na.rm=TRUE),
    sd_dose=sd(dose, na.rm=TRUE))

# Sampling 
n <- 10 # Sample count
set.seed(1)
x <- rnorm(n, 10)

sd(x) # Sample stabdard deviaton
sqrt(sum((x - mean(x))^2)/(n - 1)) #Sample standard deviation
sqrt(sum((x - mean(x))^2)/(n)) # Population standard deviation
