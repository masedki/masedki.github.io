rm(list=ls())
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
load("insurance.rda")

head(insurance)
summary(insurance)

set.seed(123)
split <- rsample::initial_split(insurance, prop=3/4)
insurance_train <- rsample::training(split)
insurance_test  <- rsample::testing(split)


## CART sans élagage
cart.0 <- rpart(charges~.,
                data=insurance_train, 
                control=rpart.control(minsplit=2,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, insurance_test)
sqrt(mean((insurance_test$charges - pred.0)**2))


plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cpOptim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cpOptim)
rpart.plot(cart.pruned)
pred_pruned <- predict(cart.pruned, insurance_test)
sqrt(mean((insurance_test$charges - pred_pruned)**2))



## random forest : package historique 
ctrl = trainControl(method="repeatedcv", number = 5, repeats=5)
grille = data.frame(mtry=1:6)
cl = makeCluster(11)
registerDoParallel(cl)
t0 = proc.time()
rftune = train(charges~., 
               insurance_train,
               method="rf",
               ntrees = 1000,
               trControl=ctrl,
               tuneGrid = grille)
t1 = proc.time()
t1 - t0
stopCluster(cl)
plot(rftune)
print(rftune)

pred_tf <- predict(rftune, insurance_test)
sqrt(mean((insurance_test$charges - pred_tf)**2))


## random forest : ranger va plus vite 
ctrl = trainControl(method="repeatedcv", number = 5, repeats=5)
tgrid <- expand.grid(.mtry = 1:6, 
                     .splitrule = c("variance", "extratrees", "maxstat"), 
                     .min.node.size = 10) #on peut tester c(5, 10, 20)
cl = makeCluster(11)
registerDoParallel(cl)
t0 = proc.time()
rftune = train(charges~., 
               insurance_train,
               method="ranger",
               num.trees = 1000,
               importance = "impurity",
               trControl=ctrl,
               tuneGrid = tgrid)
t1 = proc.time()
t1 - t0
stopCluster(cl)
plot(rftune)
print(rftune)

pred_tf <- predict(rftune, insurance_test)
sqrt(mean((insurance_test$charges - pred_tf)**2))
sort(rftune$finalModel$variable.importance/sum(rftune$finalModel$variable.importance), dec=T)
vip::vip(rftune$finalModel)
