rm(list=ls())
require(MASS)
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
require(randomForest)
data("Pima.tr")
data("Pima.te")

set.seed(11)
## CART sans élagage
cart.0 <- rpart(type~.,
                data=Pima.tr, 
                control=rpart.control(minsplit=1,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, Pima.te, type="class")
mean(Pima.te$type!=pred.0)


plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cp_optim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cp = cp_optim)
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, Pima.te, type="class")
mean(Pima.te$type!=pred.pruned)

cart.0$variable.importance/sum(cart.0$variable.importance)



## Random forest 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

control <- trainControl(method="repeatedcv", number=5, repeats=50)
rfGrid <-  expand.grid(mtry = 1:7)

RFmodel <- train(type~., data=Pima.tr, 
                 method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)
print(RFmodel)
pred.rf.caret <- predict(RFmodel, Pima.te)
mean(Pima.te$type !=pred.rf.caret)








