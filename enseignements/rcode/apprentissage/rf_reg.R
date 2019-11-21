## packages  pour rpart 
require(rpart)
require(rpart.plot)
## pacakge pour lire les données
require(ElemStatLearn)
## package pour les forêts aléatoire et choix de modèle
require(caret)
require(randomForest)
require(e1071)
## Pour faire du calcul parallèle 
require(doParallel)

## préparation des données 
data(prostate)
prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)
prostate.tr = subset(prostate, train==TRUE)[,1:9]
prostate.te = subset(prostate, train==FALSE)[,1:9]

## CART sans élagage
cart.0 <- rpart(lpsa~., data=prostate.tr, control=rpart.control(minsplit=5,cp=0))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, prostate.te)
sqrt(mean((prostate.te$lpsa - pred.0)**2))


## table de complexité
plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cp_optim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cp = cp_optim)
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, prostate.te)
sqrt(mean((prostate.te$lpsa - pred.pruned)**2))

## importance des variables 
round(cart.pruned$variable.importance/sum(cart.pruned$variable.importance), 2)

## choix du nombre de processeurs
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
model.rf <- randomForest(lpsa~.,data=prostate.tr,ntree=500)
plot(model.rf)
varImpPlot(model.rf)
pred.rf <- predict(model.rf, prostate.te)
sqrt(mean((prostate.te$lpsa - pred.rf)**2))
stopCluster(cl)

## avec caret
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
rfGrid <-  expand.grid(mtry = 1:7)
RFmodel <- train(lpsa~., data=prostate.tr, method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)
pred.rf.caret <- predict(RFmodel, prostate.te)
sqrt(mean((prostate.te$lpsa - pred.rf.caret)**2))
