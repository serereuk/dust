rm(list=ls())
library(data.table)
totalfine <- fread("t_v1.txt"); class(totalfine)

totalfine[is.na(totalfine)] <- 0
colSums(is.na(totalfine))

# 범주 만들기 #
nrow(totalfine)
fine <- length(nrow(totalfine))
library(dplyr)
totalfine <- totalfine %>% mutate(pm_10=cut(totalfine$PM10, breaks = c(-Inf,30, 80, 150, Inf), include.lowest=F))
totalfine <- totalfine %>% mutate(pm_25=cut(totalfine$PM25, breaks = c(-1,15,35,75,Inf), include.lowest=F))
levels(totalfine$pm_10) <- levels(totalfine$pm_25) <- c("good", "normal", "bad", "worst")

# 교통?
rm(list = ls())
data2 <- fread("traffic2017.txt")
data3 <- fread("t_v2.txt")
data_temp <- data2[,1:7]
data_temp2 <- data2[,8:31]
#install.packages("tidyr")
library(tidyr)
data_temp <- data_temp %>% uncount(24)
data_temp <- cbind(data_temp, as.vector(t(data_temp2)))
rm(data_temp2)
data4 <- data3[substr(data3$MSRDT,1,4)=="2017",]
road <- unique(data_temp$road)

cv <- function(ds, index, roads, out){
  temp <- ds[ds$road == roads[index] & ds$구분 == "유입",8]
  temp2 <- ds[ds$road == roads[index] & ds$구분 == "유출",8]
  colnames(temp) <- paste(roads[index], "유입", sep = "")
  colnames(temp2) <- paste(roads[index], "유출", sep = "")
  return(cbind(out, temp, temp2))
}
a <- data_temp %>% group_by(road) %>% summarise(n())
s <- a[a$`n()` == 17520,]$road

for(i in 1:length(s)){data4 <- cv(data_temp, i, s, data4)}

