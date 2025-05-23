setwd("~/Dropbox/enseignement/M1/supports/Rscripts")
rm(list=ls())
load("insurance.rda")
require(rpart)
require(rpart.plot)
require(caret)
require(randomForest)
require(doParallel)
# lecture du jeu de données

summary(insurance)
set.seed(11)
train = sample(1:nrow(insurance), round(0.75*nrow(insurance)))
insurance.tr = insurance[train,]
insurance.te = insurance[-train,]

## CART sans élagage
cart.0 <- rpart(charges~.,
                data=insurance.tr, 
                control=rpart.control(minsplit=7,cp=0, xval=5))

rpart.plot(cart.0)
pred.0 <- predict(cart.0, insurance.te)
sqrt(mean((insurance.te$charges - pred.0)**2))


plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable


## CART avec élagage
cart.pruned <- prune(cart.0, cp = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"])
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, insurance.te)
sqrt(mean((insurance.te$charges - pred.pruned)**2))


## Random forest 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
rfGrid <-  expand.grid(mtry = 1:7)
RFmodel <- train(charges~., data=insurance.tr, method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)
pred.rf.caret <- predict(RFmodel, insurance.te)
sqrt(mean((insurance.te$charges - pred.rf.caret)**2))
