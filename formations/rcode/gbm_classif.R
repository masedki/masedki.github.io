rm(list=ls())
require(MLDataR)
require(rpart)
require(xgboost)
require(rpart.plot)
require(caret)
require(doParallel)
data("heartdisease")
hd = heartdisease

head(hd)
summary(hd)
hd$HeartDisease = as.factor(hd$HeartDisease)
hd$RestingECG = as.factor(hd$RestingECG)
hd$Angina = as.factor(hd$Angina)

set.seed(123)
split <- rsample::initial_split(hd, prop=3/4)
hd_train <- rsample::training(split)
hd_test  <- rsample::testing(split)

control <- trainControl(method="repeatedcv", number=5, repeats=5)
gbmGrid <-  expand.grid(interaction.depth = 1:6,
                        n.trees = 20*(1:15), 
                        shrinkage = c(0.005,0.01,0.1, 0.5, 1), 
                        n.minobsinnode=c(2, 5, 10, 15))
cl <- makePSOCKcluster(100)
registerDoParallel(cl)
gbm_logit <- train(HeartDisease~., 
                   data=hd_train, 
                   method="gbm",
                   distribution="bernoulli",
                   trControl=control, 
                   tuneGrid=gbmGrid,
                   verbose=FALSE)
stopCluster(cl)
plot(gbm_logit)
gbm_logit$bestTune
gbm_logit_pred <- predict(gbm_logit, newdata=hd_test)
mean(gbm_logit_pred!=hd_test$HeartDisease)
vip::vip(gbm_logit)

cl <- makePSOCKcluster(100)
registerDoParallel(cl)
gbm_adaboost <- train(HeartDisease~., 
                   data=hd_train, 
                   method="gbm",
                   distribution="adaboost",
                   trControl=control, 
                   tuneGrid=gbmGrid,
                   verbose=FALSE)
stopCluster(cl)
plot(gbm_adaboost)
gbm_adaboost$bestTune
gbm_adaboost_pred <- predict(gbm_adaboost, newdata=hd_test)
mean(gbm_adaboost_pred!=hd_test$HeartDisease)
vip::vip(gbm_adaboost)

## avec la librairie xgboost 
y_train = hd_train$HeartDisease
x_train = data.matrix(hd_train[,-10])
y_test = hd_test$HeartDisease
x_test = data.matrix(hd_test[,-10])
xgboost_train = xgb.DMatrix(data=data.matrix(hd_train[,-10]), 
                            label=as.integer(hd_train$HeartDisease)-1)
xgboost_test = xgb.DMatrix(data=data.matrix(hd_test[,-10]), 
                           label= as.integer(hd_test$HeartDisease)-1)

xgbmodel = xgboost(data = xgboost_train,
                   objective = "binary:logistic",
                   booster = "gbtree", 
                   max.depth = 2, 
                   eta = 1, 
                   nrounds = 5)


control <- trainControl(method="repeatedcv", number=5, repeats=5)
xgbgrid = expand.grid(eta =c(0.01,0.05,0.1),
                      nrounds = 10*(10:20),
                      max_depth = c(1, 2, 3, 4),
                      subsample = 1,
                      min_child_weight = 1.,
                      colsample_bytree = 1,
                      gamma = 0.)


## sur serveur uniquement !!!  
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
xgbtune <- train(x_train, 
                 y_train, 
                 method = "xgbTree",
                 trControl = control,
                 nthread = 1, 
                 tuneGrid = xgbgrid)
stopCluster(cl)
plot(xgbtune)
xgbtune$bestTune
xgbtune_pred <- predict(xgbtune, newdata=x_test)
mean(xgbtune_pred!=y_test)
vip::vip(xgbtune$finalModel)