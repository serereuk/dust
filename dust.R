rm(list = ls())
#install.packages("data.table")
library(data.table)
total <- fread("total.txt")
seohae <- fread("연직바람관측.txt")
jong <- fread("종관기상관측.txt")

setkey(total, 측정소코드)
temp <- total[.(111161)]

temp <- temp[1:8736]
temp2 <- cbind(temp[,4:10], jong[,c(3:17, 19:20 ,22:27)])

temp2[is.na(temp2)] <- 0




model <- lm(PM10~. -PM25, data = temp2)
summary(model)
cor(temp2)
str(temp2)

#install.packages("corrplot")
library(corrplot)
corrplot(cor(temp2), method = "color")
