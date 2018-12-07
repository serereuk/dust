#install.packages("penalized")
#install.packages("C50")
rm(list=ls())
library(C50)
index <- 1:26304
library(data.table)
library(caret)
dat <- fread("total1417_scaled.csv")

set.seed(12)
V = 2
n =  nrow(dat)
id = sample(1:V, n, prob = c(0.75,0.25), replace = T) # Partitioning 7:3
ii = which(id==1)
train = dat[ii,c(-1, -28)]
train_y = dat[ii,28]
test = dat[-ii,c(-1, -28)]
test_y <- dat[-ii, 28]



library(DMwR)
training_2 <- cbind(train, train_y)
smote1 <- SMOTE(y1 ~ ., data = training_2, perc.over = 200, k = 5, perc.under = 800)


train <- dat[index,c(-1,-28)]
train_y <- dat[index, 28]
train_y <- factor(train_y$y1, levels = c("good", "normal", "bad", "worst"), ordered = T)
sav <- colnames(train)
colnames(train) <- paste("V", 1:30, sep = "")
training_2 <- data.frame(cbind(train, y1=train_y))
training_2 <- data.frame(rbind(training_2, smote1))
train <- training_2[,1:30]
train_y <- factor(training_2[,31], levels = c("good", "normal", "bad", "worst"), ordered = T)

test <- dat[-index,c(-1,-28)]
test_y <- dat[-index, 28]
test_y <- factor(test_y$y1, levels = c("good", "normal", "bad", "worst"), ordered = T)
colnames(test) <- paste("V", 1:30, sep = "")
tree_mod <- C5.0(x = train, y= train_y, trials = 20, control = C5.0Control(winnow = T))
tree_mod
summary(tree_mod)
#s <- C5imp(tree_mod)
#new <- train[,c(1,3,21,9,2,4,28,12,10)]
#tree2 <- C5.0(x = new, y = train_y, trials = 20)
#summary(tree2)
#sav[c(1,3,21,9,2,4,28,12,10)]
confusionMatrix(table(predict(tree_mod, test), test_y))
summary(test_y)
train_y <- factor(ifelse(as.numeric(train_y)==1, "good", "bad"), levels = c("good", "bad"), ordered = T)
test_y <- factor(ifelse(as.numeric(test_y)==1, "good", "bad"), levels = c("good", "bad"), ordered = T)
