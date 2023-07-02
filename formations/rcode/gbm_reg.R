rm(list=ls())
require(caret)
require(doParallel)
load("insurance.rda")

head(insurance)
summary(insurance)

set.seed(123)
split <- rsample::initial_split(insurance, prop=3/4)
insurance_train <- rsample::training(split)
insurance_test  <- rsample::testing(split)

control <- trainControl(method="repeatedcv", number=5, repeats=5)
gbmGrid <-  expand.grid(interaction.depth = 1:6,
                        n.trees = 20*(1:15), 
                        shrinkage = c(0.005,0.01,0.1, 0.5, 1), 
                        n.minobsinnode=c(10, 15, 20, 25))

cl <- makePSOCKcluster(11)
registerDoParallel(cl)
gbm_reg <- train(charges~., 
                 data=insurance_train, 
                 method="gbm",
                 distribution="gaussian",
                 trControl=control, 
                 tuneGrid=gbmGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(gbm_reg)
gbm_reg$bestTune
gbm_reg_pred <- predict(gbm_reg, newdata=insurance_test)
sqrt(mean((insurance_test$charges - gbm_reg_pred)**2))
vip::vip(gbm_reg$finalModel)

## avec la librairie xgboost 
y_train = insurance_train$charges
x_train = data.matrix(insurance_train[,-7])
y_test = insurance_test$charges
x_test = data.matrix(insurance_test[,-7])


control <- trainControl(method="repeatedcv", number=5, repeats=5)
xgbgrid = expand.grid(eta =c(0.01,0.05,0.1),
                      nrounds = 10*(5:15),
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
sqrt(mean((insurance_test$charges - xgbtune_pred)**2))
vip::vip(xgbtune$finalModel)
