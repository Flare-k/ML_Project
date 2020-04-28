rm(list=ls())
#데이터 불러오기
train<-read.csv("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/data.csv", header = T, quote="", fileEncoding = "utf-8")
test<-read.csv("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/test.csv", header = T, quote="", fileEncoding = "utf-8")
bts<-read.csv("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/bus_bts.csv", header = T, quote="", fileEncoding = "utf-8")
head(train)
head(test)
head(bts)
str(train) ; str(bts)
#date(날짜)변수의 datatype을 Date로 바꿔주기 as.Date()함수 사용
#요일 변수 추가해주기 format() 함수 사용
train$date<-as.Date(train$date)
train$weekday<-format(train$date, format="%a")
test$date<-as.Date(test$date)
test$weekday<-format(test$date, format="%a")
str(train$date); str(train$weekday)
str(test$date); str(test$weekday)
#route_station 변수 생성: bus_route_id + station_code
#paste0() 함수로 chr type 변수 합치기(구분: ",")
train$bus_route_id<-as.character(train$bus_route_id)
train$station_code<-as.character(train$station_code)
train$route_station<-paste0(train$bus_route_id,seq=",",train$station_code)
test$bus_route_id<-as.character(test$bus_route_id)
test$station_code<-as.character(test$station_code)
test$route_station<-paste0(test$bus_route_id,seq=",",test$station_code)
str(train)
str(test)
#bus_route_id_weekday 변수 생성: bus_route_id + weekday
train$bus_route_id_weekday<-paste0(train$bus_route_id,sep=",",train$weekday)
test$bus_route_id_weekday<-paste0(test$bus_route_id,sep=",",test$weekday)
str(train)
str(test)
#station_code_weekday 변수 생성: station_code + weekday
train$station_code_weekday<-paste0(train$station_code,sep=",",train$weekday)
test$station_code_weekday<-paste0(test$station_code,sep=",",test$weekday)
str(train)
str(test)
#route_station_weekday 변수생성 : route_station + weekday
train$route_station_weekday<-paste0(train$route_station,sep=",",train$weekday)
test$route_station_weekday<-paste0(test$route_station,sep=",",test$weekday)
str(train)
str(test)
#on_time 변수 생성: time변수에서 '시'부분만 인덱싱이 필요
library(stringr) #문자열 변수 인덱싱을 위한 패키지
bts$on_time<-str_extract(bts$geton_time, "[0-9]{2}") #숫자 중 처음 2자리 숫자를 추출
bts[c(1:5),c("geton_time","on_time")] #잘 됐는지 확인
#승하차 시간대 통합변수 (t~t+2)
#변수명으로 숫자가 먼저 올 수 없어서 68a 대신 a68 형식으로 만들었습니다.
train$a68<-apply(train[c("X6.7_ride","X7.8_ride")],1,sum)
train$a810<-apply(train[c("X8.9_ride","X9.10_ride")],1,sum)
train$a1012<-apply(train[c("X10.11_ride","X11.12_ride")],1,sum)
train$b68<-apply(train[c("X6.7_takeoff","X7.8_takeoff")],1,sum)
train$b810<-apply(train[c("X8.9_takeoff","X9.10_takeoff")],1,sum)
train$b1012<-apply(train[c("X10.11_takeoff","X11.12_takeoff")],1,sum)
str(train)
test$a68<-apply(test[c("X6.7_ride","X7.8_ride")],1,sum)
test$a810<-apply(test[c("X8.9_ride","X9.10_ride")],1,sum)
test$a1012<-apply(test[c("X10.11_ride","X11.12_ride")],1,sum)
test$b68<-apply(test[c("X6.7_takeoff","X7.8_takeoff")],1,sum)
test$b810<-apply(test[c("X8.9_takeoff","X9.10_takeoff")],1,sum)
test$b1012<-apply(test[c("X10.11_takeoff","X11.12_takeoff")],1,sum)
str(test)


library(dplyr)

id_statistic <- function(id,col1,col2){
  id = enquo(id) # passing variable name into function 
  # mean, sum
  rs_mean = train %>% group_by(!!id) %>% summarise(col1 = mean(X18.20_ride, na.rm = T))
  rs_sum = train %>% group_by(!!id) %>% summarise(col2 = sum(X18.20_ride, na.rm = T))
  rs_mean_sum = merge(rs_mean, rs_sum)
  names(rs_mean_sum)[2] = c(col1)
  names(rs_mean_sum)[3] = c(col2)
  # merge
  tr = left_join(train, rs_mean_sum)
  te = left_join(test, rs_mean_sum)
  # NA를 mean값으로 대체
  te[[col1]][is.na(te[[col1]])] <- mean(as.numeric(rs_mean[[col1]]), na.rm = T)
  te[[col2]][is.na(te[[col2]])] <- mean(as.numeric(rs_sum[[col2]]), na.rm = T)
  return(list(tr,te))
}
train = id_statistic(route_station, '1820_rs_mean','1820_rs_sum')[[1]]
test = id_statistic(route_station, '1820_rs_mean','1820_rs_sum')[[2]]
train = id_statistic(bus_route_id, '1820_r_mean', '1820_r_sum')[[1]]
test = id_statistic(bus_route_id, '1820_r_mean', '1820_r_sum')[[2]]
train = id_statistic(station_code, '1820_s_mean', '1820_s_sum')[[1]]
test = id_statistic(station_code, '1820_s_mean', '1820_s_sum')[[2]]
train = id_statistic(weekday, '1820_w_mean', '1820_w_sum')[[1]]
test = id_statistic(weekday, '1820_w_mean', '1820_w_sum')[[2]]

View(train)

mean_statistics <- function(){
  f = train %>% group_by(bus_route_id_weekday) %>% summarise(mean_bus_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(train, f, by = 'bus_route_id_weekday')
  te = left_join(test, f, by='bus_route_id_weekday') %>% mutate(mean_bus_weekday_ride=ifelse(is.na(mean_bus_weekday_ride),mean(mean_bus_weekday_ride,na.rm = T),mean_bus_weekday_ride))
  f = train %>% group_by(station_code_weekday) %>% summarise(mean_station_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(tr, f, by = 'station_code_weekday')
  te = left_join(te, f, by='station_code_weekday') %>% mutate(mean_station_weekday_ride=ifelse(is.na(mean_station_weekday_ride),mean(mean_station_weekday_ride,na.rm = T),mean_station_weekday_ride))
  f = train %>% group_by(route_station_weekday) %>% summarise(mean_route_station_weekday_ride = mean(X18.20_ride,na.rm=T))
  tr = left_join(tr, f, by = 'route_station_weekday')
  te = left_join(te, f, by='route_station_weekday') %>% mutate(mean_route_station_weekday_ride=ifelse(is.na(mean_route_station_weekday_ride),mean(mean_route_station_weekday_ride,na.rm = T),mean_route_station_weekday_ride))
  return(list(tr,te))  
}
train = mean_statistics()[[1]]
test = mean_statistics()[[2]]


#congestion 함수 만들기(bus_route_id를 기준으로 18~20의 혼잡도 계산)
congestion <- function(){
  df = train %>% group_by(bus_route_id) %>% summarise(passenger=sum(X18.20_ride, na.rm = T))
  test_congestion<-function(x){
    if (x>10000){
      return(7)
    } else if(x>5000){
      return(6)
    } else if(x>2000){
      return(5)
    } else if(x>700){
      return(4)
    } else if(x>200){
      return(3)
    } else if(x>50){
      return(2)
    } else {
      return(1)
    }
  }
  df$congestion<-c()
  for (i in 1:nrow(df)){
    df$congestion[i]<-test_congestion(df$passenger[i])
  }
  df=df[,c("bus_route_id","congestion")]
  tr = left_join(train, df)
  te = left_join(test, df)
  te$congestion[is.na(te$congestion)]<-4
  return(list(tr,te))
}
train = congestion()[[1]]
test = congestion()[[2]]
View(train)
#location=latitude+longitude
train$latitude<-as.character(train$latitude)
train$longitude<-as.character(train$longitude)
train$location<-paste0(train$latitude,seq=",",train$longitude)
test$latitude<-as.character(test$latitude)
test$longitude<-as.character(test$longitude)
test$location<-paste0(test$latitude,seq=",",test$longitude)
str(train)
str(test)
#merge key
train$cue <- 0
test$cue <- 1
str(train)
str(test)
#--------------------------------------------------------------------------
View(bts)

#--------------------------------------------------------------------------

test$X18.20_ride <- c(rep(NA,nrow(test))) #data로 합치기 위해서 target 변수인 X18.20_ride를 추가해서 NA로 채웠습니다

#오전 시간의 여러 데이터 활용한 변수
#Data set을 합쳐주기 이전에 test set에는 타겟 값인 X18.20_ride 변수가 없기 때문에, 데이터 불러오기 부분에서 X18.20_ride을 추가하고 NA로 채움.
data <- rbind(train,test)
View(data)
##a
data$a1012_sum <- ave(data$a1012, data$route_station, FUN = function(x) sum(x))
data$a1012_mean <- ave(data$a1012, data$route_station, FUN = function(x) mean(x))
##b
data$b1012_sum <- ave(data$b1012, data$route_station, FUN = function(x) sum(x))
data$b1012_mean <- ave(data$b1012, data$route_station, FUN = function(x) mean(x))
##c
data$`10.11_ride_sum` <- ave(data$X10.11_ride, data$route_station, FUN = function(x) sum(x))
data$`10.11_ride_mean` <- ave(data$X10.11_ride, data$route_station, FUN = function(x) mean(x))
##d
data$`10.11_takeoff_sum` <- ave(data$X10.11_takeoff, data$route_station, FUN = function(x) sum(x))
data$`10.11_takeoff_mean` <- ave(data$X10.11_takeoff, data$route_station, FUN = function(x) mean(x))
##e
data$`11.12_ride_sum` <- ave(data$X11.12_ride, data$route_station, FUN = function(x) sum(x))
data$`11.12_ride_mean` <- ave(data$X11.12_ride, data$route_station, FUN = function(x) mean(x))
##f
data$`11.12_takeoff_sum` <- ave(data$X11.12_takeoff, data$route_station, FUN = function(x) sum(x))
data$`11.12_takeoff_mean` <- ave(data$X11.12_takeoff, data$route_station, FUN = function(x) mean(x))
##g
data$`1820_r_mean_sum` <- ave(data$`1820_r_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_r_mean_mean` <- ave(data$`1820_r_mean`, data$route_station, FUN = function(x) mean(x))
##h
data$`1820_r_sum_sum` <- ave(data$`1820_r_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_r_sum_mean` <- ave(data$`1820_r_sum`, data$route_station, FUN = function(x) mean(x))
##i
data$`1820_rs_mean_sum` <- ave(data$`1820_rs_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_rs_mean_mean` <- ave(data$`1820_rs_mean`, data$route_station, FUN = function(x) mean(x))
##j
data$`1820_rs_sum_sum` <- ave(data$`1820_rs_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_rs_sum_mean` <- ave(data$`1820_rs_sum`, data$route_station, FUN = function(x) mean(x))
##k
data$`1820_s_mean_sum` <- ave(data$`1820_s_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_s_mean_mean` <- ave(data$`1820_s_mean`, data$route_station, FUN = function(x) mean(x))
##l
data$`1820_s_sum_sum` <- ave(data$`1820_s_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_s_sum_mean` <- ave(data$`1820_s_sum`, data$route_station, FUN = function(x) mean(x))
##m
data$`1820_w_mean_sum` <- ave(data$`1820_w_mean`, data$route_station, FUN = function(x) sum(x))
data$`1820_w_mean_mean` <- ave(data$`1820_w_mean`, data$route_station, FUN = function(x) mean(x))
##n
data$`1820_w_sum_sum` <- ave(data$`1820_w_sum`, data$route_station, FUN = function(x) sum(x))
data$`1820_w_sum_mean` <- ave(data$`1820_w_sum`, data$route_station, FUN = function(x) mean(x))
##o
data$a68_sum <- ave(data$a68, data$route_station, FUN = function(x) sum(x))
data$a68_mean <- ave(data$a68, data$route_station, FUN = function(x) mean(x))
##p
data$b68_sum <- ave(data$b68, data$route_station, FUN = function(x) sum(x))
data$b68_mean <- ave(data$b68, data$route_station, FUN = function(x) mean(x))
##q
data$`6.7_ride_sum` <- ave(data$X6.7_ride, data$route_station, FUN = function(x) sum(x))
data$`6.7_ride_mean` <- ave(data$X6.7_ride, data$route_station, FUN = function(x) mean(x))
##r
data$`6.7_takeoff_sum` <- ave(data$X6.7_takeoff, data$route_station, FUN = function(x) sum(x))
data$`6.7_takeoff_mean` <- ave(data$X6.7_takeoff, data$route_station, FUN = function(x) mean(x))
##s
data$`7.8_ride_sum` <- ave(data$X7.8_ride, data$route_station, FUN = function(x) sum(x))
data$`7.8_ride_mean` <- ave(data$X7.8_ride, data$route_station, FUN = function(x) mean(x))
##t
data$`7.8_takeoff_sum` <- ave(data$X7.8_takeoff, data$route_station, FUN = function(x) sum(x))
data$`7.8_takeoff_mean` <- ave(data$X7.8_takeoff, data$route_station, FUN = function(x) mean(x))
##u
data$`a810_sum` <- ave(data$a810, data$route_station, FUN = function(x) sum(x))
data$`a810_mean` <- ave(data$a810, data$route_station, FUN = function(x) mean(x))
##v
data$b810_sum <- ave(data$b810, data$route_station, FUN = function(x) sum(x))
data$b810_mean <- ave(data$b810, data$route_station, FUN = function(x) mean(x))
##w
data$`8.9_ride_sum` <- ave(data$X8.9_ride, data$route_station, FUN = function(x) sum(x))
data$`8.9_ride_mean` <- ave(data$X8.9_ride, data$route_station, FUN = function(x) mean(x))
##x
data$`8.9_takeoff_sum` <- ave(data$X8.9_takeoff, data$route_station, FUN = function(x) sum(x))
data$`8.9_takeoff_mean` <- ave(data$X8.9_takeoff, data$route_station, FUN = function(x) mean(x))
##y
data$`9.10_ride_sum` <- ave(data$X9.10_ride, data$route_station, FUN = function(x) sum(x))
data$`9.10_ride_mean` <- ave(data$X9.10_ride, data$route_station, FUN = function(x) mean(x))
##z
data$`9.10_takeoff_sum` <- ave(data$X9.10_takeoff, data$route_station, FUN = function(x) sum(x))
data$`9.10_takeoff_mean` <- ave(data$X9.10_takeoff, data$route_station, FUN = function(x) mean(x))
str(data)




#--------------------------------배차간격----------------------------------
train$bus_route_id = as.numeric(train$bus_route_id) # bus_route_id에 대해 cha -> num 변환
test$bus_route_id = as.numeric(test$bus_route_id)

library(lubridate)
bts$geton_time2 <- bts$geton_time #bts$geton_time2 열 만들기
bts$user_count <- as.numeric(as.character(bts$user_count)) #user_count를 numeric으로 변환

# f 데이터 프레임 만들기. geton_date, geton_time2, geton_station_code, bus_route_id를 기준으로 
# user_count의 합 구하여 f에 저장
f = bts %>% 
  group_by(geton_date, geton_time2, geton_station_code, bus_route_id) %>% 
  filter(!is.na(user_count)) %>%
  summarise(passenger = sum(user_count)) %>% 
  arrange(geton_date, geton_station_code, bus_route_id, geton_time2)
f <- na.omit(f)
f$index <- 1:nrow(f)  #f에 index라는 열 만들어 주기.


# f에서 해당 조건을 만족하는 경우 시,분,초를 정리하여 "HH:MM:SS"로 time 변수에 할당.
time = vector('character', length = nrow(f))
for (i in 1:(nrow(f)-1)){
  if((f[i,]$geton_date == f[i+1,]$geton_date) & 
     (f[i,]$geton_station_code == f[i+1,]$geton_station_code) &
     (f[i,]$bus_route_id == f[i+1,]$bus_route_id)){
    t1 <- strptime(f[i+1,]$geton_time2, "%H:%M:%S")
    t2 <- strptime(f[i,]$geton_time2, "%H:%M:%S")
    temp <- t1 - t2
    sec <- as.numeric(temp, units='secs')
    h <- sec/3600 ; h <-floor(h)
    m <- (sec%%3600)/60 ; m <- floor(m)
    s <- sec%%60 ; s <- floor(s)
    time[i] <- paste(h,":",m,":",s, sep="")
  }else{
    time[i] <- '00:00:00'
  }
}


#반복문이 너무 오래걸려서 일단은 위의 내용을 python으로 돌려 파일로 가져옴
library(data.table)
f<-fread("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/f.csv", data.table = F)

# "HH:MM:SS"를 초단위로 바꿔주는 함수
get_sec <- function(time_str){
  func_time <- strptime(time_str, "%H:%M:%S")
  hour <- func_time$hour
  minute <- func_time$min
  seconds <- func_time$sec
  
  return(hour*3600 + minute*60 + seconds)  
}
#f를 파일로 가져왔을때 인덱스에 순서가 밀려서 다시 초기화 (크게 중요한 부분은 아님)
f$index <- 1:nrow(f)


#----------------------배차간격을 계산해주는 함수----------------------
#-------interval에 f를 복사해주고 time변수를 초단위로 바꾸어 time4에 저장해준다.
#-------time4는 초단위인데 분단위로 바꿔주기 위해 60을 나눠준다.
#-------그중 3분 이상 180분 이하의 데이터만 다시 처리하여 저장해준다.
#-------bus_route_id를 그룹화하여 해당 그룹마다 평균값을 구해주고 반올림해준다.
#-------처리가 끝나면 csv파일로 저장해준다.
library(dplyr)
bus_interval <- function(){
  interval <- f
  
  time4 = vector('numeric', length = nrow(interval))
  for(i in 1:nrow(interval)){
    time4[i] <- get_sec(interval$time[i])
  }
  
  interval$time4 <- time4/60
  interval <- subset(interval, time4 > 3 & time4 < 180)
  
  interval = interval %>% group_by(bus_route_id) %>% summarise(bus_interval = ceiling(mean(time4)))
  
  interval$bus_interval <- as.numeric(as.character(interval$bus_interval))
  write.csv(interval, file = '/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/bus_interval_final.csv', row.names = FALSE)
  print("success..!")
}

bus_itvl = bus_interval()

library(data.table)
bus_interval_var<-fread("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/bus_interval_final.csv", data.table = F)
View(bus_interval_var)

#--------- data와 bus_interval_var 합치기
data <- left_join(data, bus_interval_var, by="bus_route_id")
str(data)

data$bus_interval <- replace(data$bus_interval, is.na(data$bus_interval)==T,9999)
sum(is.na(data$bus_interval))


View(bts)
jeju_finantial<-fread("/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict/jeju_financial_life_data.csv", data.table = F)
View(jeju_finantial)
