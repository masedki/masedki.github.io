rm(list=ls())
load("~/codes/TP_R/insurance.rda")
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
require(parallel)
summary(insurance)
set.seed(11)
train = sample(1:nrow(insurance), round(0.75*nrow(insurance)))
insurance.tr = insurance[train,]
insurance.te = insurance[-train,]


## CART sans élagage
cart.0 <- rpart(charges~.,
                data=insurance.tr, 
                control=rpart.control(minsplit=5,cp=0, xval=5))






rpart.plot(cart.0)
pred.0 <- predict(cart.0, insurance.te)
sqrt(mean((insurance.te$charges - pred.0)**2))





plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable


## CART avec élagage
cpOptim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cpOptim)
rpart.plot(cart.pruned)
pred_pruned <- predict(cart.pruned, insurance.te)
sqrt(mean((insurance.te$charges - pred_pruned)**2))

cart.pruned$variable.importance/sum(cart.pruned$variable.importance)

## Random forest 
control <- trainControl(method="repeatedcv", number=5, repeats=10)
rf_grid <-  data.frame(mtry = 1:6)
cl <- makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)
rf_model <- train(charges~., 
                  data=insurance.tr, 
                  method="rf", 
                  trControl=control,
                  ntree=500, 
                  tuneGrid = rf_grid,
                  verbose=FALSE)
stopCluster(cl)
plot(rf_model)
print(rf_model)
pred_rf <- predict(rf_model, insurance.te)
sqrt(mean((insurance.te$charges - pred_rf)**2))

