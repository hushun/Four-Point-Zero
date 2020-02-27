

# Shun Hu, Yuhan Liu, Yanfei Du, Hongji Li

rm(list=ls())
library(kernlab)
library(caret)
library(e1071)
library(rpart)

ameslist <- read.table("ames.csv", header = TRUE, sep = ",")

num_obs <- nrow(ameslist)
train_index <- sample(num_obs, size = trunc(0.5*num_obs))


train <- ameslist[train_index,]
test <- ameslist[-train_index,]

tree_fit <- rpart(train$Fireplaces ~ train$GrLivArea + train$LotArea + train$YearBuilt, data = train)
plot(tree_fit)

predict_unseen <-predict(tree_fit, test)
trn_tab = table(predicted = predict_unseen, actual = test$Fireplaces)












