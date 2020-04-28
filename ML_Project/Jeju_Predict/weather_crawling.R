library(rvest)
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = 'localhost', port=4445L,browserName="chrome")
remDr$open()
remDr$navigate("https://www.weather.go.kr/weather/observation/currentweather.jsp?auto_man=m&type=t99&reg=184&tm=2019.10.25.16%3A00&x=19&y=7")
#Chrome Driver 실행

crawl_weather <- function(){
  #기상정보를 저장하기 위한 DataFrame만들기
  weathers_data_10 <- data.frame('현재일기_10'=NULL,'현재기온_10'=NULL,'체감온도_10'=NULL,'일강수_10'=NULL)
  #반복문을 통해 rbind를 해주기위해 임의의 list생성
  weathers = list()
  
  for(i in 1:46){
    #검색창으로 cursor 옮기기 / css selector
    dateInput<- remDr$findElement(using="css selector",value='#observation_text')
    dateInput$sendKeysToElement(list(key='shift', key='home', key='delete'))
    #'날짜' 입력하기 / css selector         
    dateInput$sendKeysToElement(list(paste('2019.09.',i,'.10:00', sep="")))
    
    #검색버튼 클릭 / css selector
    searchButton<- remDr$findElement(using="css selector", value='#content_weather > div.distibution_search3 > form > fieldset:nth-child(2) > input:nth-child(11)')
    searchButton$clickElement() ; Sys.sleep(2)
    
    html <- remDr$getPageSource()[[1]] #클릭한 페이지 모두 읽기
    html <- read_html(html) #해당 페이지의 html모두 읽기
    weathers[[i]] <- html %>% html_nodes("td")%>% html_text() 
    weathers[[i]] <- data.frame('현재일기_10'=weathers[[i]][41], '현재기온_10'=weathers[[i]][45], '체감온도_10'=weathers[[i]][45], '일강수_10'=weathers[[i]][48])
    weathers_data_10 <- rbind(weathers_data_10, weathers[[i]]) #빈 데이터프레임에 rbind() 실행
  }
  print('success !')
  
  return(weathers_data_10)
}

weather_data <- crawl_weather()
setwd('/Users/noblyan/Desktop/R/ML_Project/Jeju_Predict')
write.csv(weather_data, file = 'weather.csv', row.names = FALSE)
print('save in directory !')
weather_data = read.csv('weather.csv')
View(weather_data)
