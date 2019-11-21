rm(list=ls())
## packages pour boosting
require(gbm)
require(caret)
## package pour le jeu de données 
require(ElemStatLearn)
## package pour calcul parallèle
require(doParallel)
## préparation des données 
data(prostate)
prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)
prostate.tr = subset(prostate, train==TRUE)[,1:9]
prostate.te = subset(prostate, train==FALSE)[,1:9]

control <- trainControl(method="repeatedcv", number=5, repeats=5)
gbmGrid <-  expand.grid(interaction.depth = 1:4,
                       n.trees = 5*(1:15), 
                       shrinkage = c(0.01,0.1, 0.5), 
                       n.minobsinnode=1:4)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)
modelGbm <- train(lpsa~., 
                  data=prostate.tr, 
                  method = 'gbm', 
                  trControl=control, 
                  tuneGrid=gbmGrid,
                  verbose=FALSE)
stopCluster(cl)

plot(modelGbm)
print(modelGbm)
modelGbm.pred <- predict(modelGbm, newdata=prostate.te)
sqrt(mean((modelGbm.pred - prostate.te$lpsa)**2))
