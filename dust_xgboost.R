rm(list = ls())
library(data.table)
library(caret)
library(xgboost)
library(ggplot2)
library(dplyr)
dat_mat <- data.matrix(dat[,c(-1, -28)])
dat_label <- as.integer(factor(dat$y1)) -1
model <- xgboost(data = dat_mat, label = dat_label, nrounds = 10, objective = "multi:softprob",
                 params = list("num_class" = 4))
temp <- predict(model, newdata = dat_mat)
temp_predict <- matrix(temp, 4, 140256/4)
temp_predict <- temp_predict %>% t() %>% data.frame() %>% mutate(max_prob = max.col(.,"last"))
confusionMatrix(factor(temp_predict$max_prob), factor(dat_label+1), mode = "everything")
table(factor(dat$y1))

s <- xgb.importance(feature_names = colnames(dat_mat), model = model)
xgb.ggplot.importance(s)

#########################
rm(list =ls())
index <- 1:26304
library(data.table)
dat <- fread("total1417_scaled.csv")

train <- dat[index, ]
test <- dat[-index, ]
require(Matrix)
train_dat <- data.matrix(train[, c(-1,-28)])
train_y <- as.integer(factor(train$y1, levels = c("good", "normal", "bad", "worst"))) - 1
model <- xgboost(data = train_dat, label = train_y, nrounds = 30, objective = "multi:softprob",
                 params = list("num_class" = 4))
test_dat <- data.matrix(test[, c(-1,-28)])
temp <- predict(model, newdata=test_dat)
temp_predict <- matrix(temp, 4, length(temp)/4)
temp_predict <- temp_predict %>% t() %>% data.frame() %>% mutate(max_prob = max.col(.,"last"))
confusionMatrix(factor(temp_predict$max_prob), factor(as.integer(factor(test$y1, levels = c("good", "normal", "bad", "worst")))), mode = "everything")
s <- xgb.importance(feature_names = colnames(train_dat), model = model)
xgb.ggplot.importance(s)

##################################

# good or bad
rm(list = ls())
index <- 1:26304
library(data.table)
dat <- fread("total1417_scaled.csv")
library(dplyr)
dat <- dat %>% mutate(y_new = as.factor(ifelse(dat[,28] == "good", "good", "bad")))
train <- dat[index,c(-1,-28)]
test <- dat[-index,c(-1,-28)]
train_dat <- data.matrix(train[,-31])
train_y <- as.integer(factor(train$y_new)) - 1
model <- xgboost(data = train_dat, label = train_y, nrounds = 30, objective = "multi:softprob",
                 params = list("num_class" = 4))

test_dat <- data.matrix(test[,c(-31)])
temp <- predict(model, newdata=test_dat)
temp_predict <- matrix(temp, 4, length(temp)/4)
temp_predict <- temp_predict %>% t() %>% data.frame() %>% mutate(max_prob = max.col(.,"last"))
confusionMatrix(factor(temp_predict$max_prob), factor(as.integer(factor(test$y_new))), mode = "everything")
s <- xgb.importance(feature_names = colnames(train_dat), model = model)
xgb.ggplot.importance(s)

