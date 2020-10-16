library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

# Import data
dengue <- read_csv("dengue.csv")
dengue_xls <- read_excel("dengue.xlsx")

# Select data
dengue %>%
  select(year,number)

# Filter data
dengue %>%
  filter(year==2018)

# Filter data based on multiple conditions
dengue %>%
  filter(year==2017 | year==2018 )

dengue %>%
  filter(year==2018,type_dengue=='Dengue' )

dengue %>%
  filter(year==2018) %>%
  filter(type_dengue=='Dengue')

# Missing values
dengue %>%
  filter(is.na(number))

dengue %>%
  filter(!complete.cases(.))

dengue %>%
  filter(complete.cases(.))

# Mutate data
dengue %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek))

# Visual aata
dengue %>%
  filter(complete.cases(.)) %>%
  filter(type_dengue=='Dengue') %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>%
  select(date,number) %>%
  plot()

# Save data 
dengue_filtered <- dengue %>%
  filter(complete.cases(.)) %>%
  filter(type_dengue=='Dengue') %>%
  mutate(date = ymd(paste0(year,"-01-01"))+weeks(eweek)) %>%
  select(date,number)

write_csv(dengue_filtered, path = "dengue_filtered.csv")

# Activity
vaccination <- read_excel("vaccination.xlsx")

vaccination %>%
  filter(complete.cases(.)) %>%
  filter(vaccination_type=='Poliomyelitis') %>%
  mutate(date = ymd(paste0(year,"-01-01"))) %>%
  mutate(doses = no_of_doses_in_thousands) %>%
  select(date,doses) %>%
  plot()

# Tibble
sleep
as_tibble(sleep)

heart<- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart <- as_tibble(heart)

# Activity: Gather
shampoo = data.frame('A'=c(36.6,39.2,30.4,37.1,34.1),'B' = c(17.5,20.6,18.7,25.7,22.0),'C'=c(15.0,10.4,18.9,10.5,15.2))

shampoo <- as_tibble(shampoo)
shampoo %>%
  gather(brand, effect)

# Data join

df1 = data_frame(name=c('Ally','Steve','John'),age=c(45,46,47))
df2 = data_frame(name=c('Ally','Belinda','John'),age=c(45,48,47))

left_join(df1,df2,by='name')
right_join(df1,df2,by='name')
inner_join(df1,df2,by='name')
full_join(df1,df2,by='name')

# Group By
sleep %>% 
  group_by(group) %>%
  summarize(avg_extra=mean(extra))

