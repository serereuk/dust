rm(list = ls())
library(purrr)
library(xlsx)
#install.packages("rJava")

#대기오염
library(rJava)
library(data.table)
setwd("./airpollute")
files <- list.files(pattern = "v2")
dat_total <- map(files, fread, fill = T)
dat_total <- rbindlist(dat_total, fill = T)
fwrite(dat_total, "total14-17.txt")

#종합
rm(list=ls())
setwd("/Users/Wook-Young/Desktop/2017/dust/20181119215447")
files <- list.files(pattern = "txt")
dat_total <- map(files, fread)
dat_total <- rbindlist(dat_total, fill = T)
fwrite(dat_total, "total214-17.txt")

#바람방향
rm(list=ls())
setwd("/Users/Wook-Young/Desktop/2017/dust/서해")
files <- list.files(pattern = "txt")
dat_total <- map(files, fread)
dat_total <- rbindlist(dat_total, fill = T)
fwrite(dat_total, "total314-17.txt")

#바람방향2
rm(list=ls())
setwd("/Users/Wook-Young/Desktop/2017/dust/문산")
files <- list.files(pattern = "txt")
dat_total <- map(files, fread)
dat_total <- rbindlist(dat_total, fill = T)
fwrite(dat_total, "total414-17.txt")

#데이터 통합용
rm(list=ls())
setwd("/Users/Wook-Young/Desktop/2017/dust")
data1 <- fread("total14-17.txt")
data2 <- fread("total214-17.txt")
total <- cbind(data1,data2)
fwrite(total, "t_v1.txt")

#환경오염물질 배출
rm(list=ls())
data <- fread("report.txt-2.csv")
data <- data[data$자치구 == "성북구", ]
fwrite(data ,"envpollute.txt")

#전입전출
rm(list)