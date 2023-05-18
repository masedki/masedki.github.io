rm(list=ls())
require(MASS)
require(caret)
require(doParallel)
require(parallel)
data("Pima.te")
data("Pima.tr")
summary(Pima.te)
summary(Pima.tr)

Pima.all = rbind.data.frame(Pima.te, Pima.tr)
set.seed(11)
train = sample(1:nrow(Pima.all), round(0.75*nrow(Pima.all)))
Pima_tr = Pima.all[train,]
Pima_te = Pima.all[-train,]

## CART sans élagage
cart.0 <- rpart(type~.,
                data=Pima_tr, 
                control=rpart.control(minsplit=2,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, Pima_te, type="class")
mean(Pima_te$type!=pred.0)

plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cpOptim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cpOptim)
rpart.plot(cart.pruned)
pred_pruned <- predict(cart.pruned, Pima_te, type="class")
mean(Pima_te$type!=pred_pruned)

## Random forest 
control <- trainControl(method="repeatedcv", number=5, repeats=10)
rf_grid <-  data.frame(mtry = 1:7)
cl <- makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)
rf_model <- train(type~., 
                  data=Pima_tr, 
                  method="rf", 
                  trControl=control,
                  ntree=500, 
                  tuneGrid = rf_grid,
                  verbose=FALSE)
stopCluster(cl)
plot(rf_model)
print(rf_model)
pred_rf <- predict(rf_model, Pima_te, type="raw")
mean(Pima_te$type !=pred_rf)



