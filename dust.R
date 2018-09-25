rm(list = ls())
library(data.table)
total <- fread("total.txt", data.table = F)
seohae <- fread("연직바람관측.txt", data.table = F)
jong <- fread("종관기상관측.txt", data.table = F)


