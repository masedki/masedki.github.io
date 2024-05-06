setwd("~/tp_intro_ds")
require(rpart)
require(rpart.plot)
load("insurance.rda")
summary(insurance)

set.seed(11)
n = nrow(insurance)
idx = sample(n, size=0.75*n, replace = F)
insurance.tr = insurance[idx,]
insurance.te = insurance[-idx,]

cart.0 = rpart(charges~., 
               data = insurance.tr, 
               control = rpart.control(maxdepth = 1))
rpart.plot(cart.0)


cart.vide = rpart(charges~., 
                  data = insurance.tr, 
                  control = rpart.control(cp = 1000))
rpart.plot(cart.vide)


pred.vide = predict(cart.vide, insurance.te)
err.vide = sqrt(mean((insurance.te$charges - pred.vide)**2))
print(err.vide)

pred.0 = predict(cart.0, insurance.te)
err.0 = sqrt(mean((insurance.te$charges - pred.0)**2))
print(err.0)


cart.complet = rpart(charges~., data = insurance.tr,
                     control = rpart.control(cp = 0, 
                                             xval = 5,
                                             minsplit = 5))
rpart.plot(cart.complet)

pred.complet = predict(cart.complet, insurance.te)
err.complet = sqrt(mean((insurance.te$charges - pred.complet)**2))
print(err.complet)

plotcp(cart.complet)

cpoptim = cart.complet$cptable[which.min(cart.complet$cptable[,"xerror"]),"CP"]
cart.complet$cptable[1:15,]
cart.pruned = prune(cart.complet, cp=cpoptim)
rpart.plot(cart.pruned)

pred.pruned = predict(cart.pruned, insurance.te)
err.pruned = sqrt(mean((insurance.te$charges - pred.pruned)**2))
print(err.pruned)
cart.pruned$variable.importance
cart.pruned$variable.importance/sum(cart.pruned$variable.importance)
