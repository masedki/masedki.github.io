rm(list=ls())
require(MASS)
require(rpart)
require(rpart.plot)
require(glmnet)
data("Pima.tr")
data("Pima.te")

set.seed(11)
## CART sans élagage
cart.0 <- rpart(type~.,
                data=Pima.tr, 
                control=rpart.control(minsplit=7,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, Pima.te, type="class")
mean(Pima.te$type!=pred.0)
table(Pima.te$type, pred.0)

plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cart.pruned <- prune(cart.0, cp = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"])
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, Pima.te, type="class")
mean(Pima.te$type!=pred.pruned)
table(Pima.te$type, pred.pruned)
