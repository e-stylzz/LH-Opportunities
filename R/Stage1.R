##### File 1: Environment Setup #####
#Setup working directory, bring in data, and load libraries

#Set working directory
setwd("~/GitHub/Opportunities")
#Load Data
data <- read.csv("~/GitHub/Opportunities/src/opportunitydata.csv", na.strings=c("","NULL"))  


library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library('randomForest')
library('party')
library('ggplot2')
library('lubridate')
library('ROCR')
library('dplyr')