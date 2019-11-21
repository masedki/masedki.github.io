rm(list=ls())
## packages pour boosting
require(gbm)
require(caret)
require(e1071)
## package pour le jeu de données 
require(MASS)
## package pour calcul parallèle
require(doParallel)
## préparation des données 
data("Pima.tr")
data("Pima.te")
set.seed(123)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
gbmGrid <-  expand.grid(interaction.depth = 1:4,
                        n.trees = 10*(1:15), 
                        shrinkage = c(0.01,0.1, 0.5, 1), 
                        n.minobsinnode=c(5, 10, 15))
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
modelGbm <- train(type~., 
                  data=Pima.tr, 
                  method="gbm", 
                  trControl=control, 
                  tuneGrid=gbmGrid,
                  verbose=FALSE)
stopCluster(cl)

plot(modelGbm)
print(modelGbm)
modelGbm.pred <- predict(modelGbm, newdata=Pima.te)
mean(modelGbm.pred!=Pima.te$type)
