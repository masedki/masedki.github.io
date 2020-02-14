rm(list=ls())
require(lasso2)
require(MASS)
retinol = read.csv2("~/codes/masedki/enseignements/datasets/presentationTPretinol.csv")
summary(retinol)
# lecture du jeu de données
data("Prostate")
lam = seq(0.1,200,len=99)
fit = lm.ridge(lpsa~., Prostate, lambda=lam)
# chaque courbe (une coleur) correspond à l'évolution de la variable du coefficient de régression d'une variable en fonction de lambda 
matplot(fit$lambda, coef(fit)[,-1], col=1:8, type="l", lty=1, lwd=2, 
        ylab=~beta, xlab=~lambda, log="x")
legend("topleft",lty=1, lwd=2, col = 1:8, legend = colnames(Prostate[,-9]))

## la fonction lm.ridge effectue une validation croisée 
## chaque individus du jeu de données est un fold, on estime le modèle sans l'individu et on calcul l'erreur de test sur l'individu laissé
## de côté. On dit qu'on fait une Leave One Out Cross-Validation -LOOCV- 
## on récupère les valeur des coefficients qui correspondent à la meilleur valeur de lambda
betabestridge = coef(fit)[which.min(fit$GCV),]
## cette meilleure valeur de lambda
lambdabest = lam[which.min(fit$GCV)]
## on trace l'erreur CV en fonction de lambda
plot(lam, fit$GCV, lty=1, type="l", ylab="CV error", xlab=~lambda)




## Lasso regression 
require(glmnet)
lam = exp(seq(log(1.5),log(0.001),len=100))
y = Prostate[,9]
x = as.matrix(Prostate[,-9])
## la fonction de cv.glmnet du package glmnet 
## cette fonction permet de contrôler la CV, et ne pas faire qu'une LOOCV
## par défaut on fait un lasso
cvfit = cv.glmnet(x, y, family="gaussian", lambda=lam, nfolds=5)
## on trace l'évolution du coefficient de régression de chaque variable en fonction de l'évolution de lambda
plot(cvfit$glmnet.fit, "lambda", col=1:8, lwd=2,ylab=~beta)
legend("topright",lty=1, lwd=2, col = 1:8, legend = colnames(Prostate[,-9]))


## Elasticnet regression 
lam = exp(seq(log(1.5),log(0.001),len=100))
#un vecteur pour stocker l'erreur de validation croisée correspondant aux meilleur lambda à alpha fixé 
errcv = rep(NA, 11)
for(i in 0:10)
{
 cvfit=cv.glmnet(x, y, family="gaussian", lambda=lam, nfolds=5, alpha = i/10) 
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

