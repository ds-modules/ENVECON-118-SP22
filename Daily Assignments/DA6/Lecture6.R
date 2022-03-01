#Lecture6

#-------------------------------------------
#install needed R packages
#you will learn this in Sections
#you only need to install them once then only call them using library()
#-------------------------------------------
#call them in
library(psych)
library(tidyverse)
library(haven)

#-------------------------------------------
#set your working directory
#-------------------------------------------
#setwd("/Users/sberto/Desktop/")
setwd("/Users/sofiavillas-boas/Dropbox/EEP118_Spring2021/Lectures/Lecture6")

#-------------------------------------------
#1. Read in data and see the top rows to see column names etc
#-------------------------------------------
#read in Lecture6 data set
#read in a Stata dataset
my_data <- read_dta("Lecture6.dta")
head(my_data)

#-------------------------------------------
#summary stats of data
#-------------------------------------------
describe(my_data,skew=FALSE)


#------------------------------------------
#generate log of wage to be the dependent variable
my_data <- mutate(my_data, lwage = log(wage))

describe(my_data$lwage, skew=FALSE)



#reg0
reg0<-lm(lwage ~ educ+exper+female+services,my_data)
summary(reg0)

#reg1
reg1<-lm(lwage ~ educ+exper+female,my_data)
summary(reg1)

#reg2
reg2<-lm(lwage ~ educ+exper,my_data)
summary(reg2)

#reg3
reg3<-lm(lwage ~ educ,my_data)
summary(reg3)

#-----------------------------------------------------------------------------------------------
#run regressions 
#-----------------------------------------------------------------------------------------------
reg1<-lm(lwage ~ educ+exper+female,my_data)
summary(reg1)


#make combined scatter plot of crime rate data and fitted values of crime rate given regression estimates
ggplot(data = my_data) + 
  geom_point(aes(x=polpc, y=crmrte, color = "data")) +
  geom_point(aes(x=polpc, y=crmrte_hat, color = "fitted")) + 
  labs(x = "Police Per Capita",
       y = "Crime Rate",
      title = "Crime Rate (Red) and Predicted Crime Rate (Blue) and Police Per Capita")



