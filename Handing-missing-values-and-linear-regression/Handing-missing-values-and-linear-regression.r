data <- read.csv("/Datasets/Health_data.csv", header = TRUE, sep = ",") 
library(tidyverse)
data 
head(data)
tail(data)
summary(data)


data$AGE
is.na(data$AGE)
data1=data %>% mutate(AGE=replace(AGE,is.na(AGE),median(AGE,na.rm=TRUE))) 
data1
data$HEIGHT
is.na(data$HEIGHT)
data2=data1 %>% mutate(HEIGHT=replace(HEIGHT,is.na(HEIGHT),median(HEIGHT,na.rm=TRUE))) 
data2
data$WEIGHT
is.na(data$WEIGHT)
data3=data2%>% mutate(WEIGHT=replace(WEIGHT,is.na(WEIGHT),median(WEIGHT,na.rm=TRUE))) 
data3
data$BMI
is.na(data$BMI)
data4=data3%>% mutate(BMI=replace(BMI,is.na(BMI),median(BMI,na.rm=TRUE))) 
data4
data$BMR
is.na(data$BMR)
data5=data4%>% mutate(BMR=replace(BMR,is.na(BMR),median(BMR,na.rm=TRUE))) 
data5



table(data5$GENDER)
male_data=subset(data5,GENDER=="M")
male_data
female_data=subset(data5,GENDER=="F")
female_data

#Creating the contingency table for the chi square test GENDER
frequency=table(data5$GENDER,data5$Exercise)

#CHI-SQUARE test
print(chisq.test(frequency))

#correlation between height and weight
cor(data5$HEIGHT,data5$WEIGHT,use="everything")
cor(data5$HEIGHT,data5$WEIGHT,use="everything",method="spearman")

#Linear relationship bwt height and weight
relation=lm(data5$WEIGHT~data5$HEIGHT)
relation
plot(data5$WEIGHT~data5$HEIGHT)
abline(relation)
summary(relation)

#multiple linear regression for BMI based on HEIGHGT AND WEIGHT
multiple_linear=lm(data5$BMI~data5$HEIGHT+data5$WEIGHT)
multiple_linear
summary(multiple_linear)
