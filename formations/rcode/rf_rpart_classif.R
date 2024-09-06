rm(list=ls())
require(MLDataR)
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
data("heartdisease")
hd = heartdisease

head(hd)
summary(hd)
hd$HeartDisease = as.factor(hd$HeartDisease)
hd$RestingECG = as.factor(hd$RestingECG)
hd$Angina = as.factor(hd$Angina)

set.seed(123)
split <- rsample::initial_split(hd, prop=3/4)
hd_train <- rsample::training(split)
hd_test  <- rsample::testing(split)


## CART sans élagage
cart_0 <- rpart(HeartDisease~.,
                data=hd_train, 
                control=rpart.control(minsplit=2,cp=0, xval=5))
rpart.plot(cart_0)
pred_0 <- predict(cart_0, hd_test, type="class")
mean(hd_test$HeartDisease!=pred_0)

plotcp(cart_0)
which.min(cart_0$cptable[,"xerror"])
cart_0$cptable

## CART avec élagage
cpOptim = cart_0$cptable[which.min(cart_0$cptable[,"xerror"]),"CP"]
cart_pruned <- prune(cart_0, cpOptim)
rpart.plot(cart_pruned)
pred_pruned <- predict(cart_pruned, hd_test, type="class")
mean(hd_test$HeartDisease!=pred_pruned)
vip::vip(cart_pruned)


## random forest avec ranger 
ctrl = trainControl(method="repeatedcv", number = 3, repeats=10)
tgrid <- expand.grid(.mtry = 1:9, 
                     .splitrule = "gini", 
                     .min.node.size = 20) #on peut tester c(5, 10, 20)
cl = makeCluster(11)
registerDoParallel(cl)
t0 = proc.time()
rftune = train(HeartDisease~., 
               hd_train,
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

pred_tf <- predict(rftune, hd_test, type="raw")
mean(hd_test$HeartDisease!=pred_tf)
vip::vip(rftune$finalModel)
