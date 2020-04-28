library(data.table)
library(tidyverse)
library(readxl)
bus_bts <- read.csv(file="/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/제주도_버스승차_인원_예측/bus_bts.csv", header = T)
View(bus_bts)

test <- read.csv(file="/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/제주도_버스승차_인원_예측/test.csv", header = T)
View(test)

train <- read.csv(file="/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/제주도_버스승차_인원_예측/data.csv", header = T)
View(train)
str(bus_bts)
