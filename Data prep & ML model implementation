#create balanced data: death
death_bal_train_fin <- data_death_logmodel
library(dplyr)
death_bal_train_fin <- death_bal_train_fin %>% arrange(desc(death))
set.seed(305)
rand_death <- sample(nrow(death_bal_train_fin[352:20345,]))
library(tidyverse)
e_death <- death_bal_train_fin[352:20345,]
e_death <- e_death[rand_death,]
death_bal_train_fin[352:20345,] <- e_death
rownames(death_bal_train_fin[352:20345,]) <- rownames(e_death)
death_bal_train_final <- death_bal_train_fin[1:702,]

#create balanced data: inpatient
inpat_bal_train_fin <- data_inpat_logmodel
library(dplyr)
inpat_bal_train_fin <- inpat_bal_train_fin %>% arrange(desc(Inpatient))
set.seed(4546)
rand_inpat <- sample(nrow(inpat_bal_train_fin[352:20345,]))
library(tidyverse)
A <- inpat_bal_train_fin[352:20345,]
A <- A[rand_inpat,]
inpat_bal_train_fin[352:20345,] <- A
rownames(inpat_bal_train_fin[352:20345,]) <- rownames(A)
inpat_bal_train_fin <- inpat_bal_train_fin[1:702,]

#create balanced data: icu
icu_bal_train_fin <- data_icu_logmodel
library(dplyr)
icu_bal_train_fin <- icu_bal_train_fin %>% arrange(desc(ICU))
set.seed(12323)
rand_icu <- sample(nrow(icu_bal_train_fin[352:20345,]))
library(tidyverse)
B_icu <- icu_bal_train_fin[352:20345,]
B_icu <- B_icu[rand_icu,]
icu_bal_train_fin[352:20345,] <- B_icu
rownames(icu_bal_train_fin[352:20345,]) <- rownames(B_icu)
icu_bal_train_final <- icu_bal_train_fin[1:702,]

#create balanced data: critical
crit_bal_train_fin <- data_crit_logmodel
library(dplyr)
crit_bal_train_fin <- crit_bal_train_fin %>% arrange(desc(critical))
set.seed(645)
rand_crit <- sample(nrow(crit_bal_train_fin[352:20345,]))
library(tidyverse)
c_crit <- crit_bal_train_fin[352:20345,]
c_crit <- c_crit[rand_crit,]
crit_bal_train_fin[352:20345,] <- c_crit
rownames(crit_bal_train_fin[352:20345,]) <- rownames(c_crit)
crit_bal_train_final <- crit_bal_train_fin[1:702,]

#create balanced data: ventilation
vent_bal_train_fin <- data_vent_logmodel
library(dplyr)
vent_bal_train_fin <- vent_bal_train_fin %>% arrange(desc(ventilation))
set.seed(4985)
rand_vent <- sample(nrow(vent_bal_train_fin[352:20345,]))
library(tidyverse)
d_vent <- vent_bal_train_fin[352:20345,]
d_vent <- d_vent[rand_vent,]
vent_bal_train_fin[352:20345,] <- d_vent
rownames(vent_bal_train_fin[352:20345,]) <- rownames(d_vent)
vent_bal_train_final <- vent_bal_train_fin[1:702,]

#simple log model: inpatient
sample_inpat_b <- sample(c(TRUE,FALSE),nrow(inpat_bal_train_fin),replace=TRUE,prob = c(0.8,0.2))
train_inpat_b <- inpat_bal_train_fin[sample_inpat_b,]
test_inpat_b <- inpat_bal_train_fin[!sample_inpat_b,]
inpat_logmodel_b <- glm(Inpatient ~., data=train_inpat_b,family=binomial(link="logit"))
summary(inpat_logmodel_b)
pred_inpat_b <- ifelse(predict(inpat_logmodel_b,test_inpat_b,type="response") > 0.5,"1","0")
confusionMatrix(as.factor(pred_inpat_b),test_inpat_b$Inpatient)

#simple log model: icu
sample_icu_b <- sample(c(TRUE,FALSE),nrow(icu_bal_train_final),replace=TRUE,prob = c(0.8,0.2))
train_icu_b <- icu_bal_train_final[sample_icu_b,]
test_icu_b <- icu_bal_train_final[!sample_icu_b,]
icu_logmodel_b <- glm(ICU ~., data=train_icu_b,family=binomial(link="logit"))
summary(icu_logmodel_b)
pred_icu_b <- ifelse(predict(icu_logmodel_b,test_icu_b,type="response") > 0.5,"1","0")
confusionMatrix(as.factor(pred_icu_b),test_icu_b$Inpatient)

#simple log model: critical
sample_crit_b <- sample(c(TRUE,FALSE),nrow(crit_bal_train_final),replace=TRUE,prob = c(0.8,0.2))
train_crit_b <- crit_bal_train_final[sample_crit_b,]
test_crit_b <- crit_bal_train_final[!sample_crit_b,]
crit_logmodel_b <- glm(critical ~., data=train_crit_b,family=binomial(link="logit"))
summary(crit_logmodel_b)
pred_crit_b <- ifelse(predict(crit_logmodel_b,test_crit_b,type="response") > 0.5,"1","0")
confusionMatrix(as.factor(pred_crit_b),test_crit_b$critical)

#simple log model: ventilation
sample_vent_b <- sample(c(TRUE,FALSE),nrow(vent_bal_train_final),replace=TRUE,prob = c(0.8,0.2))
train_vent_b <- vent_bal_train_final[sample_vent_b,]
test_vent_b <- vent_bal_train_final[!sample_vent_b,]
vent_logmodel_b <- glm(ventilation ~., data=train_vent_b,family=binomial(link="logit"))
summary(vent_logmodel_b)
pred_vent_b <- ifelse(predict(vent_logmodel_b,test_vent_b,type="response") > 0.5,"1","0")
confusionMatrix(as.factor(pred_vent_b),test_vent_b$ventilation)

#simple log model: death
sample_death_b <- sample(c(TRUE,FALSE),nrow(death_bal_train_final),replace=TRUE,prob = c(0.8,0.2))
train_death_b <- death_bal_train_final[sample_death_b,]
test_death_b <- death_bal_train_final[!sample_death_b,]
death_logmodel_b <- glm(death ~., data=train_death_b,family=binomial(link="logit"))
summary(death_logmodel_b)
pred_death_b <- ifelse(predict(death_logmodel_b,test_death_b,type="response") > 0.5,"1","0")
confusionMatrix(as.factor(pred_death_b),test_death_b$death)

#lasso regression: death
library(caret)
library(tidyverse)
library(glmnet)
set.seed(424)
x_l_death_b <- model.matrix(death ~., train_death_b)[,-16]
y_l_death_b <- ifelse(train_death_b$death == "1",1,0)
cv_death_b_lasso <- cv.glmnet(x_l_death_b,y_l_death_b,alpha=1,family="binomial") #alpha = 1 for lasso
cv_death_b_lasso$lambda.min
death_b_lasso <- glmnet(x_l_death_b,y_l_death_b,alpha=1,family="binomial",lambda=cv_death_b_lasso$lambda.min) #alpha = 1 for lasso
coef(death_b_lasso)
x_l_death_b.test <- model.matrix(death ~.,test_death_b)[,-16]
prob_death_b_lasso <- death_b_lasso %>% predict(newx = x_l_death_b.test)
pred_death_b_lasso <- ifelse(prob_death_b_lasso > 0.5,"1","0")
mean(pred_death_b_lasso == test_death_b$death)
#confusion matrix
confusion.glmnet(death_b_lasso,newx = x_l_death_b.test,newy = test_death_b$death,family = c("binomial"))

#lasso regression: inpatient
library(caret)
library(tidyverse)
library(glmnet)
set.seed(4624)
x_l_inpat_b <- model.matrix(Inpatient ~., train_inpat_b)[,-16]
y_l_inpat_b <- ifelse(train_inpat_b$Inpatient == "1",1,0)
cv_inpat_b_lasso <- cv.glmnet(x_l_inpat_b,y_l_inpat_b,alpha=1,family="binomial") #alpha = 1 for lasso
cv_inpat_b_lasso$lambda.min
inpat_b_lasso <- glmnet(x_l_inpat_b,y_l_inpat_b,alpha=1,family="binomial",lambda=cv_inpat_b_lasso$lambda.min) #alpha = 1 for lasso
coef(inpat_b_lasso)
x_l_inpat_b.test <- model.matrix(Inpatient ~.,test_inpat_b)[,-16]
prob_inpat_b_lasso <- inpat_b_lasso %>% predict(newx = x_l_inpat_b.test)
pred_inpat_b_lasso <- ifelse(prob_inpat_b_lasso > 0.5,"1","0")
mean(pred_inpat_b_lasso == test_inpat_b$Inpatient)
#confusion matrix
confusion.glmnet(inpat_b_lasso,newx = x_l_inpat_b.test,newy = test_inpat_b$Inpatient,family = c("binomial"))

#lasso regression: icu
library(caret)
library(tidyverse)
library(glmnet)
set.seed(983)
x_l_icu_b <- model.matrix(ICU ~., train_icu_b)[,-16]
y_l_icu_b <- ifelse(train_icu_b$ICU == "1",1,0)
cv_icu_b_lasso <- cv.glmnet(x_l_icu_b,y_l_icu_b,alpha=1,family="binomial") #alpha = 1 for lasso
cv_icu_b_lasso$lambda.min
icu_b_lasso <- glmnet(x_l_icu_b,y_l_icu_b,alpha=1,family="binomial",lambda=cv_icu_b_lasso$lambda.min) #alpha = 1 for lasso
coef(icu_b_lasso)
x_l_icu_b.test <- model.matrix(ICU ~.,test_icu_b)[,-16]
prob_icu_b_lasso <- icu_b_lasso %>% predict(newx = x_l_icu_b.test)
pred_icu_b_lasso <- ifelse(prob_icu_b_lasso > 0.5,"1","0")
mean(pred_icu_b_lasso == test_icu_b$ICU)
#confusion matrix
confusion.glmnet(icu_b_lasso,newx = x_l_icu_b.test,newy = test_icu_b$ICU,family = c("binomial"))

#lasso regression: critical
library(caret)
library(tidyverse)
library(glmnet)
set.seed(3539)
x_l_crit_b <- model.matrix(critical ~., train_crit_b)[,-16]
y_l_crit_b <- ifelse(train_crit_b$critical == "1",1,0)
cv_crit_b_lasso <- cv.glmnet(x_l_crit_b,y_l_crit_b,alpha=1,family="binomial") #alpha = 1 for lasso
cv_crit_b_lasso$lambda.min
crit_b_lasso <- glmnet(x_l_crit_b,y_l_crit_b,alpha=1,family="binomial",lambda=cv_crit_b_lasso$lambda.min) #alpha = 1 for lasso
coef(crit_b_lasso)
x_l_crit_b.test <- model.matrix(critical ~.,test_crit_b)[,-16]
prob_crit_b_lasso <- crit_b_lasso %>% predict(newx = x_l_crit_b.test)
pred_crit_b_lasso <- ifelse(prob_crit_b_lasso > 0.5,"1","0")
mean(pred_crit_b_lasso == test_crit_b$critical)
#confusion matrix
confusion.glmnet(crit_b_lasso,newx = x_l_crit_b.test,newy = test_crit_b$critical,family = c("binomial"))

#lasso regression: ventilation
library(caret)
library(tidyverse)
library(glmnet)
set.seed(5524)
x_l_vent_b <- model.matrix(ventilation ~., train_vent_b)[,-16]
y_l_vent_b <- ifelse(train_vent_b$ventilation == "1",1,0)
cv_vent_b_lasso <- cv.glmnet(x_l_vent_b,y_l_vent_b,alpha=1,family="binomial") #alpha = 1 for lasso
cv_vent_b_lasso$lambda.min
vent_b_lasso <- glmnet(x_l_vent_b,y_l_vent_b,alpha=1,family="binomial",lambda=cv_vent_b_lasso$lambda.min) #alpha = 1 for lasso
coef(vent_b_lasso)
x_l_vent_b.test <- model.matrix(ventilation ~.,test_vent_b)[,-16]
prob_vent_b_lasso <- vent_b_lasso %>% predict(newx = x_l_vent_b.test)
pred_vent_b_lasso <- ifelse(prob_vent_b_lasso > 0.5,"1","0")
mean(pred_vent_b_lasso == test_vent_b$ventilation)
#confusion matrix
confusion.glmnet(vent_b_lasso,newx = x_l_vent_b.test,newy = test_vent_b$ventilation,family = c("binomial"))

#ridge regression: death
library(caret)
library(tidyverse)
library(glmnet)
set.seed(61278)
x_r_death_b <- model.matrix(death ~., train_death_b)[,-16]
y_r_death_b <- ifelse(train_death_b$death == "1",1,0)
cv_death_b_ridge <- cv.glmnet(x_r_death_b,y_r_death_b,alpha=0,family="binomial") #alpha = 0 for ridge
cv_death_b_ridge$lambda.min
death_b_ridge <- glmnet(x_r_death_b,y_r_death_b,alpha=0,family="binomial",lambda=cv_death_b_ridge$lambda.min) #alpha = 0 for ridge
coef(death_b_ridge)
x_r_death_b.test <- model.matrix(death ~.,test_death_b)[,-16]
prob_death_b_ridge <- death_b_ridge %>% predict(newx = x_r_death_b.test)
pred_death_b_ridge <- ifelse(prob_death_b_ridge > 0.5,"1","0")
mean(pred_death_b_ridge == test_death_b$death)
#confusion matrix
confusion.glmnet(death_b_ridge,newx = x_r_death_b.test,newy = test_death_b$death,family = c("binomial"))

#ridge regression: inpatient
library(caret)
library(tidyverse)
library(glmnet)
set.seed(6125)
x_r_inpat_b <- model.matrix(Inpatient ~., train_inpat_b)[,-16]
y_r_inpat_b <- ifelse(train_inpat_b$Inpatient == "1",1,0)
cv_inpat_b_ridge <- cv.glmnet(x_r_inpat_b,y_r_inpat_b,alpha=0,family="binomial") #alpha = 0 for ridge
cv_inpat_b_ridge$lambda.min
inpat_b_ridge <- glmnet(x_r_inpat_b,y_r_inpat_b,alpha=0,family="binomial",lambda=cv_inpat_b_ridge$lambda.min) #alpha = 0 for ridge
coef(inpat_b_ridge)
x_r_inpat_b.test <- model.matrix(Inpatient ~.,test_inpat_b)[,-16]
prob_inpat_b_ridge <- inpat_b_ridge %>% predict(newx = x_r_inpat_b.test)
pred_inpat_b_ridge <- ifelse(prob_inpat_b_ridge > 0.5,"1","0")
mean(pred_inpat_b_ridge == test_inpat_b$Inpatient)
#confusion matrix
confusion.glmnet(inpat_b_ridge,newx = x_r_inpat_b.test,newy = test_inpat_b$Inpatient,family = c("binomial"))

#ridge regression: icu
library(caret)
library(tidyverse)
library(glmnet)
set.seed(9785)
x_r_icu_b <- model.matrix(ICU ~., train_icu_b)[,-16]
y_r_icu_b <- ifelse(train_icu_b$ICU == "1",1,0)
cv_icu_b_ridge <- cv.glmnet(x_r_icu_b,y_r_icu_b,alpha=0,family="binomial") #alpha = 0 for ridge
cv_icu_b_ridge$lambda.min
icu_b_ridge <- glmnet(x_r_icu_b,y_r_icu_b,alpha=0,family="binomial",lambda=cv_icu_b_ridge$lambda.min) #alpha = 0 for ridge
coef(icu_b_ridge)
x_r_icu_b.test <- model.matrix(ICU ~.,test_icu_b)[,-16]
prob_icu_b_ridge <- icu_b_ridge %>% predict(newx = x_r_icu_b.test)
pred_icu_b_ridge <- ifelse(prob_icu_b_ridge > 0.5,"1","0")
mean(pred_icu_b_ridge == test_icu_b$ICU)
#confusion matrix
confusion.glmnet(icu_b_ridge,newx = x_r_icu_b.test,newy = test_icu_b$ICU,family = c("binomial"))

#ridge regression: critical
library(caret)
library(tidyverse)
library(glmnet)
set.seed(98711)
x_r_crit_b <- model.matrix(critical ~., train_crit_b)[,-16]
y_r_crit_b <- ifelse(train_crit_b$critical == "1",1,0)
cv_crit_b_ridge <- cv.glmnet(x_r_crit_b,y_r_crit_b,alpha=0,family="binomial") #alpha = 0 for ridge
cv_crit_b_ridge$lambda.min
crit_b_ridge <- glmnet(x_r_crit_b,y_r_crit_b,alpha=0,family="binomial",lambda=cv_crit_b_ridge$lambda.min) #alpha = 0 for ridge
coef(crit_b_ridge)
x_r_crit_b.test <- model.matrix(critical ~.,test_crit_b)[,-16]
prob_crit_b_ridge <- crit_b_ridge %>% predict(newx = x_r_crit_b.test)
pred_crit_b_ridge <- ifelse(prob_crit_b_ridge > 0.5,"1","0")
mean(pred_crit_b_ridge == test_crit_b$critical)
#confusion matrix
confusion.glmnet(crit_b_ridge,newx = x_r_crit_b.test,newy = test_crit_b$critical,family = c("binomial"))

#ridge regression: ventilation
library(caret)
library(tidyverse)
library(glmnet)
set.seed(5551)
x_r_vent_b <- model.matrix(ventilation ~., train_vent_b)[,-16]
y_r_vent_b <- ifelse(train_vent_b$ventilation == "1",1,0)
cv_vent_b_ridge <- cv.glmnet(x_r_vent_b,y_r_vent_b,alpha=0,family="binomial") #alpha = 0 for ridge
cv_vent_b_ridge$lambda.min
vent_b_ridge <- glmnet(x_r_vent_b,y_r_vent_b,alpha=0,family="binomial",lambda=cv_vent_b_ridge$lambda.min) #alpha = 0 for ridge
coef(vent_b_ridge)
x_r_vent_b.test <- model.matrix(ventilation ~.,test_vent_b)[,-16]
prob_vent_b_ridge <- vent_b_ridge %>% predict(newx = x_r_vent_b.test)
pred_vent_b_ridge <- ifelse(prob_vent_b_ridge > 0.5,"1","0")
mean(pred_vent_b_ridge == test_vent_b$ventilation)
#confusion matrix
confusion.glmnet(vent_b_ridge,newx = x_r_vent_b.test,newy = test_vent_b$ventilation,family = c("binomial"))

#random forest: inpatient
library(randomForest)
library(caret)
train_inpat_b <- rename(train_inpat_b, ckd = 'cmrbd_CKD/ESRD')
rf_inpat_b <- randomForest(Inpatient~.,data=train_inpat_b,ntree=500,proximity=T)
print(rf_inpat_b)
test_inpat_b <- rename(test_inpat_b, ckd = 'cmrbd_CKD/ESRD')
pred_inpat_b_rf <- predict(rf_inpat_b,test_inpat_b)
confusionMatrix(pred_inpat_b_rf,test_inpat_b$Inpatient)
importance(rf_inpat_b)
varImpPlot(rf_inpat_b)

#random forest: icu
library(randomForest)
library(caret)
train_icu_b <- rename(train_icu_b, ckd = 'cmrbd_CKD/ESRD')
rf_icu_b <- randomForest(ICU~.,data=train_icu_b,ntree=500,proximity=T)
print(rf_icu_b)
test_icu_b <- rename(test_icu_b, ckd = 'cmrbd_CKD/ESRD')
pred_icu_b_rf <- predict(rf_icu_b,test_icu_b)
confusionMatrix(pred_icu_b_rf,test_icu_b$ICU)
importance(rf_icu_b)
varImpPlot(rf_icu_b)

#random forest: critical
library(randomForest)
library(caret)
train_crit_b <- rename(train_crit_b, ckd = 'cmrbd_CKD/ESRD')
rf_crit_b <- randomForest(critical~.,data=train_crit_b,ntree=500,proximity=T)
print(rf_crit_b)
test_crit_b <- rename(test_crit_b, ckd = 'cmrbd_CKD/ESRD')
pred_crit_b_rf <- predict(rf_crit_b,test_crit_b)
confusionMatrix(pred_crit_b_rf,test_crit_b$critical)
importance(rf_crit_b)
varImpPlot(rf_crit_b)

#random forest: ventilation
library(randomForest)
library(caret)
train_vent_b <- rename(train_vent_b, ckd = 'cmrbd_CKD/ESRD')
rf_vent_b <- randomForest(ventilation~.,data=train_vent_b,ntree=500,proximity=T)
print(rf_vent_b)
test_vent_b <- rename(test_vent_b, ckd = 'cmrbd_CKD/ESRD')
pred_vent_b_rf <- predict(rf_vent_b,test_vent_b)
confusionMatrix(pred_vent_b_rf,test_vent_b$ventilation)
importance(rf_vent_b)
varImpPlot(rf_vent_b)

#random forest: death
library(randomForest)
library(caret)
train_death_b <- rename(train_death_b, ckd = 'cmrbd_CKD/ESRD')
rf_death_b <- randomForest(death~.,data=train_death_b,ntree=500,proximity=T)
print(rf_death_b)
test_death_b <- rename(test_death_b, ckd = 'cmrbd_CKD/ESRD')
pred_death_b_rf <- predict(rf_death_b,test_death_b)
confusionMatrix(pred_death_b_rf,test_death_b$death)
importance(rf_death_b)
varImpPlot(rf_death_b)




