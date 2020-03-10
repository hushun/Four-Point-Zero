
rm(list=ls())
library(ISLR)
library(tibble)
library(caret)
library(ellipse)
library(tibble)
library(magrittr)
library(dplyr)
library(knitr)
library(kableExtra)
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tidyverse)

set.seed(876345)

ameslist <- read.table("ames2.csv", header = TRUE, sep = ",")

num_obs <- nrow(ameslist)
train_index <- sample(num_obs, size = trunc(0.5*num_obs))


train <- ameslist[train_index,]
test <- ameslist[-train_index,]

tree_fit <- rpart(train$Fireplaces ~ train$GrLivArea + train$LotArea + train$YearBuilt, data = train)
plot(tree_fit)


predict_unseen <- predict(tree_fit, test)
predict_unseen <- round(predict_unseen, 0)
trn_tab = table(predicted = predict_unseen, actual = test$Fireplaces)

trn_con_mat <- confusionMatrix(trn_tab, positive = "Yes")

df <- data.frame(
  predicted = c(0,1,2), 
  Fireplaces_Number = c(147,172,23))

p <- ggplot(data = df,
            mapping = aes(x = predicted, y = Fireplaces_Number))
p + geom_bar(stat = "identity")




