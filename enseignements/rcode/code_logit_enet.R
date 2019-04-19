rm(list=ls())
require(glmnet)
# install.packages("mlbench") # à utiliser si le package n'est pas installé
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
str(bc)
# remove id column
bc <- bc[,-1]
# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
mod = glm(Class~., data = bc, family="binomial")
summary(mod)


## régression logistique elasticnet 
#un vecteur pour stocker l'erreur de validation croisée correspondant aux meilleur lambda à alpha fixé 
y = bc[, 10] # la dernière colonne Class contient la variable y 
x = as.matrix(bc[, -10]) # x correspondant à toute la base privée de la dernière colonne : as.matrix pour que ça puisse fonctionner avec glmnet
errcv = rep(NA, 11)
for(i in 0:10)
{
  set.seed(587)
  cvfit=cv.glmnet(x, y, family="binomial", type.measure="class", nfolds=5,  alpha = i/10) 
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



