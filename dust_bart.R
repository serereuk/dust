#install.packages("BART")
rm(list = ls())
library(BART)
index <- 1:26304
library(data.table)
library(caret)
dat <- fread("total1417_scaled.csv")

train <- data.matrix(dat[index,c(-1,-28)])
train_y <- dat[index, 28]
train_y <- factor(train_y$y1, levels = c("good", "normal", "bad", "worst"), ordered = T)
test <- data.matrix(dat[-index,c(-1,-28)])
test_y <- dat[-index, 28]
test_y <- factor(test_y$y1, levels = c("good", "normal", "bad", "worst"), ordered = T)

model <- mbart(train, train_y, x.test=test)
#model <- mc.mbart(train, train_y, x.test= test)

tree_mod <- C5.0(x = train, y= train_y, trials = 20)
tree_mod
summary(tree_mod)
confusionMatrix(table(predict(tree_mod, test), test_y))
