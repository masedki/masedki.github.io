setwd("~/Dropbox/enseignement/M1/supports/Rscripts")
rm(list=ls())
load("insurance.rda")
require(rpart)
require(rpart.plot)
require(caret)
require(randomForest)
require(doParallel)
# lecture du jeu de données

summary(insurance)
set.seed(11)
train = sample(1:nrow(insurance), round(0.75*nrow(insurance)))
insurance.tr = insurance[train,]
insurance.te = insurance[-train,]

## CART sans élagage
cart.0 <- rpart(charges~.,
                data=insurance.tr, 
                control=rpart.control(minsplit=7,cp=0, xval=5))

rpart.plot(cart.0)
pred.0 <- predict(cart.0, insurance.te)
sqrt(mean((insurance.te$charges - pred.0)**2))


plotcp(cart.0)
which.min(cart.0$cptable[,"xerror"])
cart.0$cptable


## CART avec élagage
cart.pruned <- prune(cart.0, cp = cart.0$cptable[which.min(cart.0$cptable[,"xerror"]),"CP"])
rpart.plot(cart.pruned)
pred.pruned <- predict(cart.pruned, insurance.te)
sqrt(mean((insurance.te$charges - pred.pruned)**2))


## Random forest 
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
rfGrid <-  expand.grid(mtry = 1:7)
RFmodel <- train(charges~., data=insurance.tr, method="rf", 
                 trControl=control,
                 ntree=500, 
                 tuneGrid = rfGrid,
                 verbose=FALSE)
stopCluster(cl)
plot(RFmodel)
pred.rf.caret <- predict(RFmodel, insurance.te)
sqrt(mean((insurance.te$charges - pred.rf.caret)**2))












## comparaison avec glmnet
require(glmnet)
y.train = insurance.tr[,7]
y.test = insurance.te[,7]
x.train = model.matrix( ~ .-1, insurance.tr[, -7]) ## permet de binariser les données catégorielles
x.test  = model.matrix( ~ .-1, insurance.te[, -7]) ## permet de binariser les données catégorielles
x.train = x.train[, -3] # suppression d'une variale inutile
x.test = x.test[, -3] # suppression d'une variable inutile
errcv = rep(NA, 11)
for(i in 0:10)
{ 
  #set.seed(583)
  cvfit=cv.glmnet(x.train, y.train, family="gaussian", nfolds=5, alpha = i/10) 
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

pred.enet = predict(cvfitbest, x.test)
sqrt(mean((y.test - pred.enet)**2))
