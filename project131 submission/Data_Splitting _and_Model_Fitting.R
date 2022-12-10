## Data Splitting  and Model Fitting

EmployeeNew_split <- EmployeeNew %>% 
  initial_split(prop = 0.7, strata = "Attrition")

EmployeeNew_train <- training(EmployeeNew_split)
EmployeeNew_test <- testing(EmployeeNew_split)


#### Random Forest 
set.seed(7321)

tc_10<-trainControl(method="cv", 
                    number=10)

tg <- expand.grid(mtry=10:20)

mod.rf <- train(as.factor(Attrition) ~ ., 
                metric = "Accuracy", 
                method = "rf",
                trControl = tc_10,
                tuneGrid = tg,
                data = EmployeeNew_train)


result.rf<-data.frame(mtry=c(10:20),
                      Accuracy=mod.rf$results['Accuracy'],
                      AccuracySD=mod.rf$results['AccuracySD'],
                      cv=10)

result.rf

#predict
set.seed(7321)

tg <- expand.grid(mtry=10)

mod.rf.pre <- train(as.factor(Attrition) ~ ., 
                    metric = "Accuracy", 
                    method = "rf",
                    tuneGrid = tg,
                    data = EmployeeNew_train)

mod.rf.pre$finalModel

importance.df<-data.frame(mod.rf.pre$finalModel$importance)
importance.df$variables<-rownames(importance.df)
importance.df[order(importance.df$MeanDecreaseGini, decreasing = TRUE),]

rf.pre.result<-predict(mod.rf.pre, newdata = EmployeeNew_test) 
mean(EmployeeNew_test$Attrition==rf.pre.result)



#### Gradient Boosting model 

library(gbm)
library(plyr)
set.seed(7321)
tc_10<-trainControl(method="cv", 
                    number=10)

tg <- expand.grid(n.trees = c(100,200,300,400,500), 
                  interaction.depth = 5,
                  shrinkage = 0.2,
                  n.minobsinnode = 10)

mod.gb1 <- train(as.factor(Attrition) ~ ., 
                 metric = "Accuracy", 
                 method = "gbm",
                 trControl = tc_10,
                 tuneGrid = tg,
                 data = EmployeeNew_train)

result.gb1<-data.frame(n.trees = c(100,200,300,400,500),
                       Accuracy=mod.gb1$results['Accuracy'],
                       AccuracySD=mod.gb1$results['AccuracySD'],
                       cv=10)

result.gb1

set.seed(7321)

tg <- expand.grid(n.trees = 400, 
                  interaction.depth = 5,
                  shrinkage = 0.2,
                  n.minobsinnode = 10)


mod.gbl.pre <- train(as.factor(Attrition) ~ ., 
                     metric = "Accuracy", 
                     method = "gbm",
                     tuneGrid = tg,
                     data = EmployeeNew_train)

gbl.pre.result<-predict(mod.gbl.pre, newdata = EmployeeNew_test) 
mean(EmployeeNew_test$Attrition==gbl.pre.result)



#### Least Squares Support Vector Machine 
library(kernlab)
set.seed(7321)
tc_10<-trainControl(method="cv", 
                    number=10)

tg <- expand.grid(tau = c(0, 0.5, 1, 5, 10),
                  sigma=1)

mod.svm <- train(as.factor(Attrition) ~ ., 
                 metric = "Accuracy", 
                 method = "lssvmRadial",
                 trControl = tc_10,
                 tuneGrid = tg,
                 data = EmployeeNew_train)

result.svm<-data.frame(tau = c(0, 0.5, 1, 5, 10),
                       Accuracy=mod.svm$results['Accuracy'],
                       AccuracySD=mod.svm$results['AccuracySD'],
                       cv=10)

result.svm

set.seed(7321)

tg <- expand.grid(tau = 1, sigma=1)

mod.svm.pre <- train(as.factor(Attrition) ~ ., 
                     metric = "Accuracy", 
                     method = "lssvmRadial",
                     tuneGrid = tg,
                     data = EmployeeNew_train)

svm.pre.result<-predict(mod.svm.pre, newdata = EmployeeNew_test) 
mean(EmployeeNew_test$Attrition==svm.pre.result)




#### Model Averaged Neural Network  

library(nnet)
#train

#Model Averaged Neural Network

set.seed(7321)
tc_10<-trainControl(method="cv", 
                    number=10)

tg <- expand.grid(size = c(3, 4, 5, 6, 7),
                  decay=0,
                  bag=FALSE)

mod.nn <- train(as.factor(Attrition) ~ ., 
                metric = "Accuracy", 
                method = "avNNet",
                trControl = tc_10,
                tuneGrid = tg,
                data = EmployeeNew_train)

result.nn<-data.frame(size = c(3, 4, 5, 6, 7),
                      Accuracy=mod.nn$results['Accuracy'],
                      AccuracySD=mod.nn$results['AccuracySD'],
                      cv=10)

result.nn

set.seed(7321)

tg <- expand.grid(size = 3,
                  decay=0,
                  bag=FALSE)


mod.nn.pre <- train(as.factor(Attrition) ~ ., 
                    metric = "Accuracy", 
                    method = "avNNet",
                    tuneGrid = tg,
                    data = EmployeeNew_train)

nn.pre.result<-predict(mod.nn.pre, newdata = EmployeeNew_test) 
mean(EmployeeNew_test$Attrition==nn.pre.result)


## Result & conclusion 

result_df<-data.frame(method=c("Random Forest",
                               "Gradient Boosting",
                               "Least Squares SVM",
                               "Model Averaged Neural Network"),
                      accuracy=c(mean(EmployeeNew_test$Attrition==rf.pre.result), 
                                 mean(EmployeeNew_test$Attrition==gbl.pre.result),
                                 mean(EmployeeNew_test$Attrition==svm.pre.result), 
                                 mean(EmployeeNew_test$Attrition==nn.pre.result)))

result_df


































































