setwd("~/codes/tp_R/")
rm(list=ls())
load("insurance.rda")
#require(rpart)
#require(rpart.plot)
require(caret)
#require(randomForest)
require(doParallel)
require(gbm)
# lecture du jeu de données

summary(insurance)
set.seed(11)
train = sample(1:nrow(insurance), round(0.75*nrow(insurance)))
insurance.tr = insurance[train,]
insurance.te = insurance[-train,]

# gradient boosting 
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=5, repeats=5)
gbmGrid <-  expand.grid(n.trees = 25*(1:40), 
                        shrinkage = c(0.001, 0.01, 0.1),
                        interaction.depth = c(1,2,3,4,5,6), 
                        n.minobsinnode=2)
GbmModel <- train(charges~., 
                  data=insurance.tr, 
                  method="gbm", 
                  trControl=control,
                  distribution="gaussian", 
                  tuneGrid = gbmGrid,
                  verbose=FALSE)
stopCluster(cl)
plot(GbmModel)
print(GbmModel)
pred.gbm <- predict(GbmModel, insurance.te)
sqrt(mean((insurance.te$charges - pred.gbm)**2))
