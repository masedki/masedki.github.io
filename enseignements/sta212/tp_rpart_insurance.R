setwd("~/codes/tp_R/")
rm(list=ls())
load("insurance.rda")
require(rpart)
require(rpart.plot)
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
cart.0$cptable[1:20,]


## CART avec élagage
cp_optim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cp = cp_optim)
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, insurance.te)
sqrt(mean((insurance.te$charges - pred.pruned)**2))


