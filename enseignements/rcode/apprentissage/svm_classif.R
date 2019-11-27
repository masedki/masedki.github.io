rm(list=ls())
## packages pour SVR
require(kernlab)
require(caret)
## package pour le jeu de données 
require(MASS)
## package pour calcul parallèle
require(doParallel)
## préparation des données 
data("Pima.tr")
data("Pima.te")
## Linear
control <- trainControl(method="repeatedcv", number=5, repeats=5)
svmLGrid <-  expand.grid(#epsilon = seq(0,1,0.1), 
  C = c(0.01, 0.05, 0.1, 0.5, 1, 5,10))
svmRGrid <-  expand.grid(sigma = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1), 
                         C = c(0.01, 0.05, 0.1, 0.5, 1, 5,10))

cl <- makePSOCKcluster(8)
registerDoParallel(cl)
tunedsvmL <- train(type~., 
                   data=Pima.tr, 
                   method = 'svmLinear', 
                   trControl=control, 
                   tuneGrid=svmLGrid,
                   verbose=FALSE)
tunedsvmR <- train(type~., 
                   data=Pima.tr, 
                   method = 'svmRadial', 
                   trControl=control, 
                   tuneGrid=svmRGrid,
                   verbose=FALSE)
stopCluster(cl)

plot(tunedsvmL)
print(tunedsvmL)
tunedsvmL.pred <- predict(tunedsvmL, newdata=Pima.te)
mean(tunedsvmL.pred!=Pima.te$type)


plot(tunedsvmR)
print(tunedsvmR)
tunedsvmR.pred <- predict(tunedsvmR, newdata=Pima.te)
mean(tunedsvmR.pred!=Pima.te$type)
