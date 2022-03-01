#Lecture5

#-------------------------------------------
#install needed R packages
#you will learn this in Sections
#you only need to install them once then only call them using library()
#-------------------------------------------
#for reading escell data file install the package below
install.packages("readxl")
#for OLS linear regression model install below
install.packages("lmsupport")
#for summary stats install below
install.packages("psych")
#for scatter plots etc install ggplot2
install.packages("ggplot2")
#install haven
install.packages("haven")
install.packages("data.table")
install.packages("dplyr")
install.packages("foreign")
#call them in
library(lmSupport)
library(readxl)
library(psych)
library(ggplot2)
library(haven)
library(data.table)
library(dplyr)
library(foreign)
#-------------------------------------------
#set your working directory
#-------------------------------------------
#setwd("/Users/sberto/Desktop/")
setwd("/Users/sofiavillas-boas/Dropbox/EEP118_Spring2021/Lectures/Lecture5")

#-------------------------------------------
#1. Read in data and see the top rows to see column names etc
#-------------------------------------------
my_data <- read.csv("Lecture5.csv")
head(my_data)

#summarize data
describe(my_data)

#lecture 4 only used year=87
my_data2 <- my_data[my_data$year ==87,] 
head(my_data2)

#regression
regLecture4 <- lm(crmrte ~ polpc,my_data2)
#show output
summary(regLecture4)

#predicted crime rate
my_data2$crmrte_hat <- regLecture4$fitted.values

#--------------------------------------------------------------------------------------
#in section you will learn how to plot scatter plots etc and also in lecture 5 in a
#jupyter notebook with me
#--------------------------------------------------------------------------------------

#-------------------------------------------
#for Lecture 5 scatter plot Y and Yhat
#-------------------------------------------
fig1<-plot(my_data2$crmrte,my_data2$crmrte_hat, main="Scatter of Crime Rate and Predicted Crime Rate",
           xlab="Crime Rate", ylab="Predicted Crime Rate")


#--------------------------------------------------------------------------------------
# vary sample size and show how standard errors of beta hats change
#--------------------------------------------------------------------------------------

#load sample N=100
#read in a Stata dataset
sample100 <- read_dta("sample100.dta")

#load sample N=400
#read in a Stata dataset
sample400 <- read_dta("sample400.dta")

#use sample N=630
#read in a Stata dataset
sample630<-my_data

#-----------------------------------------------------------------------------------------------
#run regressions and compare the standard errors of the beta hats as the sample size N increases
#-----------------------------------------------------------------------------------------------

reg100<-lm(crmrte~polpc,sample100)
summary(reg100)
reg400<-lm(crmrte~polpc,sample400)
summary(reg400)

#--------------------------------------------------------------------------------
#in Jupyter notebook you compare summary(regLecture4) using N=90 with N=630 below
#regression
regLectureN630 <- lm(crmrte ~ polpc,my_data)
#show output
summary(regLectureN630)


#generate fitted values
my_data$crmrte_hat<-regLectureN630$fitted.values

#make combined scatter plot of crime rate data and fitted values of crime rate given regression estimates
scatter_data_fittedVals <- ggplot(data = my_data) + geom_point(aes(x=polpc, y=crmrte, color = "data")) +
  geom_point(aes(x=polpc, y=crmrte_hat, color = "fitted")) + 
  xlab("Police Per Capita") + ylab("Crime Rate ") +
    ggtitle("Crime Rate (Red) and Predicted Crime Rate (Blue) and Police Per Capita")
scatter_data_fittedVals

