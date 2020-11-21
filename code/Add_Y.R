#setwd("C:/Users/13989/Desktop/Stat628-Module3/data")  #set your own path
library("Matrix")
load("covariates.RData")
Y=read.csv("burger.csv",colClasses=c("NULL", NA, "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL"))
save(list=c("Y","X"), file = "covariates_withY.RData")
