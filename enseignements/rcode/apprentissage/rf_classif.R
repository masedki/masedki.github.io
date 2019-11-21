rm(list=ls())
## packages  pour rpart 
require(rpart)
require(rpart.plot)
## pacakge pour lire les données
require(MASS)
## package pour les forêts aléatoire et choix de modèle
require(caret)
require(randomForest)
require(e1071)
## Pour faire du calcul parallèle 
require(doParallel)

## préparation des données 
data("Pima.tr")
data("Pima.te")

set.seed(11)
## CART sans élagage
cart.0 <- rpart(type~.,
                data=Pima.tr, 
                control=rpart.control(minsplit=5,cp=0))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, Pima.te, type="class")
mean(Pima.te$type!=pred.0)
table(Pima.te$type, pred.0)

plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cp_optim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cp = cp_optim)
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, Pima.te, type="class")
mean(Pima.te$type!=pred.pruned)
table(Pima.te$type, pred.pruned)
## importance des variables 
round(cart.pruned$variable.importance/sum(cart.pruned$variable.importance), 2)

## choix du nombre de processeurs
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
model.rf <- randomForest(type~.,data=Pima.tr,ntree=500)
plot(model.rf)
varImpPlot(model.rf)
pred.rf <- predict(model.rf, Pima.te,type="class")
mean(Pima.te$type!=pred.rf)
stopCluster(cl)

## avec caret
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
rfGrid <-  expand.grid(mtry = 1:7)
RFmodel <- train(type~., data=Pima.tr, method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)
pred.rf.caret  <- predict(RFmodel, Pima.te[,-8],type="prob")
max.prob = as.factor(names(pred.rf.caret)[apply(pred.rf.caret, 1,which.max)])
mean(Pima.te$type!=max.prob)

