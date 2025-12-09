rm(list=ls())
setwd("~/codes/tp_arbres_reg/")
require(caret)
require(doParallel)
require(parallel)
require(vip)
load("fetal_health.rda")
summary(fetal_df)
fetal_df$fetal_health = as.numeric(fetal_df$fetal_health)
fetal_df$fetal_health[which(fetal_df$fetal_health==1)] = 0
fetal_df$fetal_health[which(fetal_df$fetal_health==2 | fetal_df$fetal_health==3)] = 1
fetal_df$fetal_health = as.factor(fetal_df$fetal_health)
summary(fetal_df)
prop.table(table(fetal_df$fetal_health))

set.seed(11)
train = sample(1:nrow(fetal_df), round(0.75*nrow(fetal_df)))
fetal_tr = fetal_df[train,]
fetal_te = fetal_df[-train,]
control <- trainControl(method="repeatedcv", number=5, repeats = 10)
adaboost_grid <- expand.grid(n.trees =  25*4:20,
                             interaction.depth = 1:10,
                             shrinkage = c(0.01, 0.05, 0.1, 0.2, 0.5),
                             n.minobsinnode = c(2, 5, 10, 15, 20)) 


cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)
adaboost_model <- train(fetal_health~., 
                        data=fetal_tr, 
                        method ="gbm",
                        distribution = "adaboost",
                        trControl=control, 
                        tuneGrid = adaboost_grid,
                        verbose=FALSE)
stopCluster(cl)
plot(adaboost_model)
print(adaboost_model)

pred_adaboost <- predict(adaboost_model, fetal_te, type="raw")
mean(fetal_te$fetal_health !=pred_adaboost)
