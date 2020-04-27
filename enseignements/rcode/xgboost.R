setwd("~/Dropbox/enseignement/M1/supports/Rscripts")
rm(list=ls())
load("insurance.rda")
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
require(xgboost)
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



##xboost
?xgboost
y.tr = insurance.tr[,7]
x.tr = data.matrix(insurance.tr[,-7])
y.te = insurance.te[,7]
x.te = data.matrix(insurance.te[,-7])


boosted = xgboost(data = x.tr, label = y.tr,
                  objective = "reg:linear" , # la fonction de perte quadratique
                  booster = "gbtree", # règle faible : arbre 
                  max.depth = 2, # nombre de feuilles de la règle faible 
                  eta = 1, # le paramètre de régularisation appelé lambda en cours
                  nrounds = 5) # le nombre d'itération du GB


## réglage avec caret 

fitControl <- trainControl(method = "repeatedcv", number = 3,
                           repeats = 2,  search = "random")
# boost.grid = expand.grid(eta =c(0.1, 0.2, 0.3),
#                          nrounds = 10*(5:20), 
#                          max_depth = c(2, 3, 4, 5),
#                          subsample = 1, 
#                          min_child_weight = 1.,
#                          colsample_bytree = 1,
#                          gamma = 0.)
#cl <- makePSOCKcluster(7)
#registerDoParallel(cl) 
boosted.cv <- train(x.tr, 
                    y.tr, 
                    method = "xgbTree",
                    trControl = fitControl)
                    #tuneGrid = boost.grid)
#stopCluster(cl) 
plot(boosted.cv)
boosted.cv$bestTune

pred.boost = predict(boosted.cv, x.te)
sqrt(mean((y.te - pred.boost)^2))


## Paramètres optimaux d'après Etienne
boost.opt = xgboost(data = x.tr, label = y.tr,
                    objective = "reg:linear",
                    booster = "gbtree",
                    max.depth = 9,
                    eta = 0.1111882,
                    nrounds = 913)

pred.boost = predict(boost.opt,x.te)
sqrt(mean((y.te - pred.boost)^2)) 

