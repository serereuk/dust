rm(list= ls())
#install.packages("XML")
library(XML)
library(data.table)

crawl <- function(year, month){
  url <- "http://openapi.seoul.go.kr:8088/" ;api_key <- "416a707464696d7336366748687569"
  url2 <- "/xml/"; category <- "TimeAverageCityAir"; number <- "/1/"; number2 <- "20/" # 어디까지 뽑을 것인지
  day <- paste(c(paste(0, 1:9, sep = ""), as.character(10:31)), sep = "")
  time <- paste(c(paste(0, 0:9, sep = ""), as.character(10:23)), "00", sep = "")
  table <- data.frame()
  for(i in 1:length(day)){
    for(j in 1:length(time)){
      date_total <- paste(year, month, day[i], time[j], sep = "")
      total <- paste(url, api_key, url2, category, number, number2, date_total, sep = "")
      doc <- xmlToDataFrame(total)
      try(table <- rbindlist(list(table, doc[15, 4:15])), silent = T)
    }
  }  
  return(data.table(table))
}

a <- crawl("2014", "02")
