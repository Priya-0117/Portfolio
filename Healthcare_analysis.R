#Load Data

clinic_data <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv", header = T)
clinic_data

dim(clinic_data)
colnames(clinic_data)

str(clinic_data)
summary(clinic_data)


hist(clinic_data$age)

colnames(clinic_data)

#logistic regression 
index <- sample(nrow(clinic_data),nrow(clinic_data)*0.80) #split the data 80/20
clinic_train = clinic_data[index,] 
clinic_test = clinic_data[-index,]

clinic_glm0 <- glm(DEATH_EVENT ~., family=binomial, data=clinic_train) 
summary(clinic_glm0)

AIC(clinic_glm0)
BIC(clinic_glm0)


clinic_glm_back <- step(clinic_glm0) # backward selection (if you don't specify anything) 
summary(clinic_glm_back) 
clinic_glm_back$deviance 
AIC(clinic_glm_back) 
BIC(clinic_glm_back)

---
model_1 <- glm(DEATH_EVENT~ age + anaemia + creatinine_phosphokinase + diabetes + 
              ejection_fraction + high_blood_pressure + platelets + serum_creatinine + 
              serum_sodium + sex + smoking + time, family = binomial, data = clinic_train)
AIC(model_1)


model_2 <- glm(DEATH_EVENT~ age + anaemia + creatinine_phosphokinase + diabetes + 
                 ejection_fraction + high_blood_pressure + serum_creatinine + 
                 serum_sodium + sex + smoking + time, family = binomial, data = clinic_train)
AIC(model_2)


model_3 <- glm(DEATH_EVENT~ age + creatinine_phosphokinase + diabetes + ejection_fraction + 
                 high_blood_pressure + serum_creatinine + serum_sodium + sex + 
                 smoking + time, family = binomial, data = clinic_train)
AIC(model_3)


model_4 <- glm(DEATH_EVENT~ age + creatinine_phosphokinase + diabetes + ejection_fraction + 
                serum_creatinine + serum_sodium + sex + smoking + time, family = binomial, data = clinic_train)
AIC(model_4)


model_5 <- glm(DEATH_EVENT~ age + creatinine_phosphokinase + diabetes + ejection_fraction + 
                 serum_creatinine + serum_sodium + sex + time, family = binomial, data = clinic_train)
AIC(model_5)


model_6<- glm(DEATH_EVENT~ age + creatinine_phosphokinase + ejection_fraction + 
                serum_creatinine + serum_sodium + sex + time, family = binomial, data = clinic_train)
AIC(model_6)

#Summary of models     
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)

#out-of-sample
#ROC CURVE
library(ROCR) 
pred_glm0_test<- predict(model_6, newdata = clinic_test, type="response")
pred <- prediction(pred_glm0_test, clinic_test$DEATH_EVENT) 
perf <- performance(pred, "tpr", "fpr") 
plot(perf, colorize=TRUE)

#AUC VALUE
unlist(slot(performance(pred, "auc"), "y.values")) # gives you the value of AUC

pred_resp <- predict(model_6,type="response") 
hist(pred_resp)

#MISCLASSIFICATION MATRIX
table(clinic_train$DEATH_EVENT, (pred_resp > 0.5)*1, dnn=c("Truth", "Predicted"))
table(clinic_train$DEATH_EVENT, (pred_resp > 0.2)*1, dnn=c("Truth", "Predicted"))
table(clinic_train$DEATH_EVENT, (pred_resp > 0.0001)*1, dnn=c("Truth", "Predicted"))


#classification 
clinic_rpart0 <- rpart(formula = DEATH_EVENT ~ ., data = clinic_train, method = "class") #create model
clinic_rpart0
prp(clinic_rpart0) #tree visualization


#rf
library (randomForest)
set.seed (1)
bag.clinic_data <- randomForest(DEATH_EVENT ~ ., data = clinic_data , subset = clinic_train , mtry = 12, importance = TRUE)
bag.clinic_data
