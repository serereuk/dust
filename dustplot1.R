rm(list=ls())
#서해 
#install.packages("circular")
library(data.table)
temp <- fread("t_v1.txt")
library(circular)
library(dplyr)
ds <- fread("total414-17.txt")

cirplot <- function(data, year, data2, lower, upper){
  temp <- data[substr(data$일시,1,4)==year, ]
  temp_400 <- temp[temp$`고도(m)`<=upper & temp$`고도(m)`>=lower,]
  temp_400 <- temp_400 %>% mutate(d =gsub("-|:| ", "", temp_400$일시, perl = T))
  temp2 <- unique(paste(substr(temp_400$d, 1,10), "00", sep = ""))
  dust <- data.frame()
  for(i in 1:length(temp2)){
    dust <- rbindlist(list(dust, data2[temp2[i] == data2$MSRDT, c(1,6:8)] ), fill = T)
  }
  dust2 <- data.frame()
  for(j in 1:length(temp2)){
    dust2 <- rbindlist(list(dust2, temp_400[(temp2[j] == temp_400$d), c(3,4,5,9)]))
  }
  colnames(dust) <- c("d", "PM10", "PM24", "PM25")
  dust$d <- as.character(dust$d)
  temp3 <- merge(dust, dust2, all = T)
  temp3 <- temp3[complete.cases(temp3),]
  temp4 <- temp3[temp3$PM10 > 100, ]
  return(temp4)
}

datam <- cirplot(ds, "2016", temp, 100, 500)
rm(ds, temp)
winddc <- circular(datam$`풍향(deg)`, type = "angles", units = "degree",template = "geographics")
plot(winddc, bin = 300, stack =T, cex = 0.7, main = "2017 munsan lowest altitude", shrink = 1.3)
rose.diag(winddc, bins = 16, add = T, prop = 1.3, cex = 0.7, shrink = 1.3)
lines(density.circular(winddc, bw = 100))
fwrite(datam, "ex20162.txt")
