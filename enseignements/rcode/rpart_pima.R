rm(list=ls())
require(MASS)
require(rpart)
require(rpart.plot)
require(caret)
require(doParallel)
require(randomForest)
data("Pima.tr")
data("Pima.te")

set.seed(11)
## CART sans élagage
cart.0 <- rpart(type~.,
                data=Pima.tr, 
                control=rpart.control(minsplit=1,cp=0, xval=5))
rpart.plot(cart.0)
pred.0 <- predict(cart.0, Pima.te, type="class")
mean(Pima.te$type!=pred.0)


plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable

## CART avec élagage
cart.pruned <- prune(cart.0, cp = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"])
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, Pima.te, type="class")
mean(Pima.te$type!=pred.pruned)




## Random forest 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

control <- trainControl(method="repeatedcv", number=5, repeats=10)
rfGrid <-  expand.grid(mtry = 1:7)
RFmodel <- train(type~., data=Pima.tr, method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)

pred.rf.caret <- predict(RFmodel, Pima.te)
mean(Pima.te$type !=pred.rf.caret)











## comparaison avec glmnet
y.train = Pima.tr[,8] # la variable réponse y est mise dans le vecteur y.train
y.test = Pima.te[,8] 
x.train = as.matrix(Pima.tr[, -8]) # on garde seulement les variables explicatives dans x.train 
x.test  = as.matrix(Pima.te[, -8])

errcv = rep(NA, 11)
for(i in 0:10)
{ 
  # validation croisée pour choisir lambda à alpha fixé à i/10 pour régression logistique pénalisée
  cvfit=cv.glmnet(x.train, y.train, family="binomial", type.measure = "class",nfolds=5, alpha = i/10) 
  assign(paste("fit", i, sep="."), cvfit)  
  errcv[i+1] = min(cvfit$cvm) 
}

## affichage du meilleur alpha
print(paste("best alpha : ", (which.min(errcv) - 1)/10, sep="")) 
#meilleur modèle qui correspond au meilleur alpha et et au meilleur lambda
cvfitbest  = get(paste("fit", (which.min(errcv) - 1), sep="."))
## affichage de la meilleur valeur de lambda
print(paste(" best lambda = ", round(cvfitbest$lambda.min, 3), sep=""))
## les coefficients de régression qui correspondent aux meilleures valeurs de lambda et alpha
bestbetaenet = cvfitbest$glmnet.fit$beta[,which.min(cvfitbest$cvm)]
print(bestbetaenet)

pred.enet = predict(cvfitbest, x.test, type="class")
# erreur de test du meilleur modèle obtenu 
mean(y.test!=pred.enet)
