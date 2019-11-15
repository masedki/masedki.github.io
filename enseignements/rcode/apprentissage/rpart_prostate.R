## packages 
require(rpart)
require(rpart.plot)
require(ElemStatLearn)
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
cart.pruned <- prune(cart.0, cp = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"])
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, prostate.te)
sqrt(mean((prostate.te$lpsa - pred.pruned)**2))


## importance des variables 
round(cart.pruned$variable.importance/sum(cart.pruned$variable.importance), 2)
