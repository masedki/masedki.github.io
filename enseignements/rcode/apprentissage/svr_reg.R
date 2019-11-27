rm(list=ls())
## packages pour SVR
require(kernlab)
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

## Linear
control <- trainControl(method="repeatedcv", number=5, repeats=5)
svrLGrid <-  expand.grid(#epsilon = seq(0,1,0.1), 
                        C = c(0.01, 0.05, 0.1, 0.5, 1, 5,10))
svrRGrid <-  expand.grid(sigma = c(0.01, 0.05, 0.1, 0.5, 1), 
                         C = c(0.01, 0.05, 0.1, 0.5, 1, 5,10))

cl <- makePSOCKcluster(8)
registerDoParallel(cl)
tunedsvrL <- train(lpsa~., 
                   data=prostate.tr, 
                   method = 'svmLinear', 
                   trControl=control, 
                   tuneGrid=svrLGrid,
                   verbose=FALSE)
tunedsvrR <- train(lpsa~., 
                   data=prostate.tr, 
                   method = 'svmRadial', 
                   trControl=control, 
                   tuneGrid=svrRGrid,
                   verbose=FALSE)
stopCluster(cl)

plot(tunedsvrL)
print(tunedsvrL)
tunedsvrL.pred <- predict(tunedsvrL, newdata=prostate.te)
sqrt(mean((tunedsvrL.pred - prostate.te$lpsa)**2))


plot(tunedsvrR)
print(tunedsvrR)
tunedsvrR.pred <- predict(tunedsvrR, newdata=prostate.te)
sqrt(mean((tunedsvrR.pred - prostate.te$lpsa)**2))

