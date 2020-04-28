rm(list=ls())
data <- read.csv("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/preprocess.csv", header = T, fileEncoding = "CP949")
### MODELING --------------------------------------------
#1 안쓰이는 변수 제거 및 train,test 나누기
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)

data_backup <- data
data$dong2 <- as.numeric(data$dong2)
data$bus_route_id2 <- as.numeric(data$bus_route_id2)
data$station_code2 <- as.numeric(data$station_code2)
data$route_station_weekday2 <- as.numeric(data$route_station_weekday2)
str(data,list.len=nrow(data)) #전부 numeric


data <- data_backup

library(dplyr)
data_jeju <- data %>% filter(`si_제주시`==1)
data_jeju <- data_jeju[sample(nrow(data_jeju), 60000),]
nrow(data_jeju)

data_seo <- data %>% filter(`si_서귀포시`==1)
data_seo <- data_seo[sample(nrow(data_seo), 60000),]
nrow(data_seo)

data_jejudo <- rbind(data_jeju, data_seo) #제주시 + 서귀포시 12만
str(data_jejudo)

#train , test data 나누기
data_train <- data_jejudo %>% filter(cue==0)
data_test <- data_jejudo %>% filter(cue==1)

#제주,서귀포 구분없이 필요없는 공통 열 제거
data_train_clean <- data_train %>% select(-c(X,id,date,bus_route_id,station_code,station_name,weekday,route_station,bus_route_id_weekday,station_code_weekday,route_station_weekday,mean_bus_weekday_ride,mean_station_weekday_ride,mean_route_station_weekday_ride,location,cue,route_station2,dong,si,dist_jeju,dist_gosan,dist_seongsan,dist_po,dist_name))
data_test_clean <- data_test %>% select(-c(X,id,date,bus_route_id,station_code,station_name,weekday,route_station,bus_route_id_weekday,station_code_weekday,route_station_weekday,mean_bus_weekday_ride,mean_station_weekday_ride,mean_route_station_weekday_ride,location,cue,route_station2,dong,si,dist_jeju,dist_gosan,dist_seongsan,dist_po,dist_name))

data_train_clean$dong2[is.na(data_train_clean$dong2)] <- 0

str(data_train_clean)
# 90개 column들을 가진 dataset 4개 만들기
xvarname <- data_train_clean %>% select(-X18.20_ride) %>% names()
yvarname <- "X18.20_ride"

set.seed(123)
input_var1 <- sample(xvarname, 40)
set.seed(234)
input_var2 <- sample(xvarname, 40)
set.seed(345)
input_var3 <- sample(xvarname, 40)
set.seed(456)
input_var4 <- sample(xvarname, 40)


input1 <- data_train_clean %>% select(input_var1,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input2 <- data_train_clean %>% select(input_var2,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input3 <- data_train_clean %>% select(input_var3,X18.20_ride) #40개 변수 뽑은 dataset (y포함)
input4 <- data_train_clean %>% select(input_var4,X18.20_ride) #40개 변수 뽑은 dataset (y포함)

test_x1 <- data_test_clean %>% select(input_var1)
test_x2 <- data_test_clean %>% select(input_var2)
test_x3 <- data_test_clean %>% select(input_var3)
test_x4 <- data_test_clean %>% select(input_var4)


#2 1퍼뽑아서 *4 (seed 4번) = mtry  4개 (oob)
set.seed(123)
train_123 <- sample_n(input1,nrow(input1)*0.01)
x_train_123 <- train_123 %>% select(-X18.20_ride)
y_train_123 <- train_123$X18.20_ride

set.seed(234)
train_234 <- sample_n(input2,nrow(input2)*0.01)
x_train_234 <- train_234 %>% select(-X18.20_ride)
y_train_234 <- train_234$X18.20_ride

set.seed(345)
train_345 <- sample_n(input3,nrow(input3)*0.01)
x_train_345 <- train_345 %>% select(-X18.20_ride)
y_train_345 <- train_345$X18.20_ride

set.seed(456)
train_456 <- sample_n(input4,nrow(input4)*0.01)
x_train_456 <- train_456 %>% select(-X18.20_ride)
y_train_456 <- train_456$X18.20_ride


library(randomForest)
set.seed(123)
bestmtry_123 <- tuneRF(x_train_123,y_train_123,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_123)

set.seed(234)
bestmtry_234 <- tuneRF(x_train_234,y_train_234,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_234)

set.seed(345)
bestmtry_345 <- tuneRF(x_train_345,y_train_345,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_345)

set.seed(456)
bestmtry_456 <- tuneRF(x_train_456,y_train_456,stepFactor=1.5,improve=1e-5,ntree=100)
print(bestmtry_456)

#RF FIT
input1_x <- input1 %>% select(-X18.20_ride)
rf.fit_123 = randomForest(y= input1$X18.20_ride, x=input1_x, mtry = , ntree = 100, importance = T)

input2_x <- input2 %>% select(-X18.20_ride)
rf.fit_234 = randomForest(y= input2$X18.20_ride, x=input2_x, mtry = , ntree = 100, importance = T)

input3_x <- input3 %>% select(-X18.20_ride)
rf.fit_345 = randomForest(y= input3$X18.20_ride, x=input3_x, mtry = , ntree = 100, importance = T)

input2_x <- input4 %>% select(-X18.20_ride)
rf.fit_456 = randomForest(y= input4$X18.20_ride, x=input4_x, mtry = , ntree = 100, importance = T)

#RF TEST
pred1 <- predict(rf.fit_123,test_x1)
pred2 <- predict(rf.fit_234,test_x2)
pred3 <- predict(rf.fit_345,test_x3)
pred4 <- predict(rf.fit_456,test_x4)

#Geometric_mean
final_pred <- (pred1*pred2*pred3*pred4)**(1/4) 




