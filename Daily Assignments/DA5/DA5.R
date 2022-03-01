#Daily Assignment 5

#-------------------------------------------
#install needed R packages
#you will learn this in Sections
#you only need to install them once then only call them using library()
#-------------------------------------------
#for reading escell data file install the package below
#install.packages("readxl")
#for OLS linear regression model install below
#install.packages("lmsupport")
#for summary stats install below
#install.packages("psych")
#for scatter plots etc install ggplot2
#install.packages("ggplot2")
#install haven
#install.packages("haven")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("foreign")
#call them in

# Loading packages
library(dplyr)
library(haven)
library(readr)
library(knitr)
library(haven)
library(dplyr)
library(readxl)
library(psych)
library(ggplot2)
library(stats4)
library(lmSupport)
library(magrittr)
library(qwraps2)
library(stargazer)
#-------------------------------------------
#set your working directory
#-------------------------------------------
#setwd("/Users/sberto/Desktop/")
setwd("/Users/sofiavillas-boas/Dropbox/EEP118_Spring2021/Daily Assignments/5-DA-Lecture5")

#-------------------------------------------
#1. Read in data and see the top rows to see column names etc
#-------------------------------------------
#read in DA5 data set
#read in an excell dataset
my_data <- read_excel("opec.xlsx")
head(my_data)

#-------------------------------------------
#summary stats of data
#-------------------------------------------
describe(my_data,skew=FALSE)

#scatter plot
scatter_data <- ggplot(data = my_data) + geom_point(aes(x=capacity, y=margCost, color = "data")) +
  xlab("Production Capacity  (thousand barrels/day)") + ylab("Marginal Costs (Dollars)") +
  ggtitle("Marginal Costs and Capacity  (thousand barrels/day)")
scatter_data

#-----------------------------------------------------------------------------------------------
#run regressions 
#-----------------------------------------------------------------------------------------------
reg1<-lm(margCost ~ capacity,my_data)
summary(reg1)

my_data$margCosts_hat <-reg1$fitted.values

scatter_data_fittedVals <- ggplot(data = my_data) + geom_point(aes(x=capacity, y=margCost, color = "data")) +
  geom_point(aes(x=capacity, y=margCosts_hat, color = "fitted")) + 
  xlab("Production Capacity  (thousand barrels/day)") + ylab("Marginal Costs (Dollars)") +
  ggtitle("Marginal Costs (Red) Predicted Marginal Costs (Blue) and Capacity")
scatter_data_fittedVals

