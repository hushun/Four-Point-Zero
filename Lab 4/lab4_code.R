
bank=read.csv("bank.csv",header=TRUE)

##################Exercise 1.1

#don't print out warning massages
options(warn=-1)
library(kernlab)
data("spam")

is.factor(spam$type)
levels(spam$type)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)


predict(fit_additive)


# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)


library(boot)

set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
set.seed(1)
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
set.seed(1)
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
set.seed(1)
cv.glm(spam_trn, fit_over, K = 5)$delta[1]


set.seed(7250)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
set.seed(7250)
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
set.seed(7250)
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
set.seed(7250)
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

#################################Exercise 1.2

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

spam_caps_pred = ifelse(predict(fit_caps, spam_tst) > 0,
                            "spam",
                            "nonspam")

spam_selected_pred = ifelse(predict(fit_selected, spam_tst) > 0,
                        "spam",
                        "nonspam")

spam_additive_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")

spam_over_pred = ifelse(predict(fit_over, spam_tst) > 0,
                            "spam",
                            "nonspam")


conf_mat_caps = make_conf_mat(predicted = spam_caps_pred, actual = spam_tst$type)

conf_mat_selected = make_conf_mat(predicted = spam_selected_pred, actual = spam_tst$type)

conf_mat_additive = make_conf_mat(predicted = spam_additive_pred, actual = spam_tst$type)

conf_mat_over = make_conf_mat(predicted = spam_over_pred, actual = spam_tst$type)


#####fit_caps: four confusion
#accuracy
mean(spam_caps_pred==spam_tst$type)
#Prev
(conf_mat_caps[1,2]+conf_mat_caps[2,2])/nrow(spam_tst)
#Sensitivity 
conf_mat_caps[2,2]/(conf_mat_caps[2,2]+conf_mat_caps[1,2])
#Specificity
conf_mat_caps[1,1]/(conf_mat_caps[1,1]+conf_mat_caps[2,1])


#####fit_selected: four confusion
#accuracy
mean(spam_selected_pred==spam_tst$type)
#Prev
(conf_mat_selected[1,2]+conf_mat_selected[2,2])/nrow(spam_tst)
#Sensitivity 
conf_mat_selected[2,2]/(conf_mat_selected[2,2]+conf_mat_selected[1,2])
#Specificity
conf_mat_selected[1,1]/(conf_mat_selected[1,1]+conf_mat_selected[2,1])

#####fit_additive: four confusion
#accuracy
mean(spam_additive_pred==spam_tst$type)
#Prev
(conf_mat_additive[1,2]+conf_mat_additive[2,2])/nrow(spam_tst)
#Sensitivity 
conf_mat_additive[2,2]/(conf_mat_additive[2,2]+conf_mat_additive[1,2])
#Specificity
conf_mat_additive[1,1]/(conf_mat_additive[1,1]+conf_mat_additive[2,1])


#####fit_over: four confusion
#accuracy
mean(spam_over_pred==spam_tst$type)
#Prev
(conf_mat_over[1,2]+conf_mat_over[2,2])/nrow(spam_tst)
#Sensitivity 
conf_mat_over[2,2]/(conf_mat_over[2,2]+conf_mat_over[1,2])
#Specificity
conf_mat_over[1,1]/(conf_mat_over[1,1]+conf_mat_over[2,1])



#################################Exercise 2
#question 1
set.seed(42)
bank_idx = sample(nrow(bank), round(nrow(bank) / 2))
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]

#question 2 

fit_additive_bank = glm(y ~ job + contact + month + duration + campaign,
                   data = bank_trn, family = binomial)

set.seed(7250)
cv.glm(bank_trn, fit_additive_bank, K = 10)$delta[1]

summary(fit_additive_bank)


#question 3

coef(fit_additive_bank)


#qeestion 4

bank_additive_pred = ifelse(predict(fit_additive_bank, bank_tst) > 0,
                            "yes",
                            "no")

bank_mat_additive = make_conf_mat(predicted = bank_additive_pred, actual = bank_tst$y)
bank_mat_additive

#####fit_additive_bank: four confusion
#accuracy
mean(bank_additive_pred==bank_tst$y)
#Prev
(bank_mat_additive[1,2]+bank_mat_additive[2,2])/nrow(bank_tst)
#Sensitivity 
bank_mat_additive[2,2]/(bank_mat_additive[2,2]+bank_mat_additive[1,2])
#Specificity
bank_mat_additive[1,1]/(bank_mat_additive[1,1]+bank_mat_additive[2,1])






