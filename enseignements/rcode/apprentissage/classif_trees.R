rm(list=ls())
setwd("~/codes/TP_R")
require(caret)
require(doParallel)
require(parallel)
require(rpart)
require(rpart.plot)
load("fetal_health.rda")
summary(fetal_df)
set.seed(11)
train = sample(1:nrow(fetal_df), round(0.75*nrow(fetal_df)))
fetal_tr = fetal_df[train,]
fetal_te = fetal_df[-train,]

## CART sans élagage
cart.0 <- rpart(fetal_health~.,
                data=fetal_tr, 
                control=rpart.control(minsplit=2,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, fetal_te, type="class")
mean(fetal_te$fetal_health!=pred.0)

plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cpOptim = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"]
cart.pruned <- prune(cart.0, cpOptim)
rpart.plot(cart.pruned)
pred_pruned <- predict(cart.pruned, fetal_te, type="class")
mean(fetal_te$fetal_health!=pred_pruned)
vip::vip(cart.pruned)



## Random forest 
control <- trainControl(method="repeatedcv", number=5, repeats=10)
rf_grid <-  data.frame(mtry = 1:21)
cl <- makePSOCKcluster(detectCores()-2)
registerDoParallel(cl)
rf_model <- train(fetal_health~., 
                  data=fetal_tr, 
                  method="rf", 
                  trControl=control,
                  ntree=500, 
                  tuneGrid = rf_grid,
                  verbose=FALSE)
stopCluster(cl)
plot(rf_model)
print(rf_model)
pred_rf <- predict(rf_model, fetal_te, type="raw")
mean(fetal_te$fetal_health !=pred_rf)
vip::vip(rf_model)


