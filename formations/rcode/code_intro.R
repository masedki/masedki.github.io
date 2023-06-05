
load("mixture_example.rda")
require(class)
# 200 données de dimension 2 sous forme de matrice 200 x 2 
x <- mixture.example$x
dim(x)
head(x)
# les labels des données 0 ou 1 
y <- mixture.example$y
table(y)

# on trace le nuage de points avec couleur rouge si le label du point vaut 1 et vert s'il vaut 0. 
plot(x, col=ifelse(y==1,"red", "green"), xlab="x1", ylab="x2")

# une grille 
px1 = mixture.example$px1 
px2 = mixture.example$px2
xnew = mixture.example$xnew
dim(xnew)
# un maillage 
plot(xnew, pch=".",xlab="x1", ylab="x2")

# aide de la fonction 
?knn 

# k=15
mod15 = knn(train=x, test=xnew, cl=y, k=15, prob=TRUE)
summary(mod15)
prob = attr(mod15, "prob")
prob = ifelse(mod15=="1", prob, 1-prob) 
prob15 <- matrix(prob, length(px1), length(px2))
prob_bayes = matrix(mixture.example$prob, length(px1), length(px2))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="x1", ylab="x2", main="decision boundary")
contour(px1, px2, prob_bayes, add = TRUE, 
        levels=0.5,
        lwd=1.5,
        col="darkblue")
points(x, col=ifelse(y==1, "red", "green"))

# k=1
mod1 <- knn(x, xnew, k=1, cl=y, prob=TRUE)
prob <- attr(mod1, "prob")
prob <- ifelse( mod1=="1", prob, 1-prob) 
prob1 <- matrix(prob, length(px1), length(px2) )
contour(px1, px2, prob1, level=0.5, labels="", xlab="x1", ylab="x2", main="decision boundary")
contour(px1, px2, prob_bayes, add = TRUE, 
        levels=0.5,
        lwd=1.5,
        col="darkblue")
points(x, col=ifelse(y==1, "red", "green"))






K = 1:120
nK = length(K)
ErrTrain = rep(NA, length=nK)
ErrTest  = rep(NA, length=nK)
for (i in 1:nK) 
  {
  k = K[i] 
  modtrain = knn(x, x, k=k, cl=y)
  ErrTrain[i] = mean(modtrain!=y)
  modtest = knn(x, xtest, k=k, cl=y)
  ErrTest[i] = mean(modtest!=ytest)
}

# Figure pour superposer les deux erreurs
# erreur test 
plot(K, ErrTest, type="b", col="blue", xlab="nombre de voisins",ylab=" erreurs train et test", pch=20, 
     ylim=range(c(ErrTest, ErrTrain)))
# erreur d'apprentissage
lines(K, ErrTrain,type="b",col="red",pch=20)
# une petite légende
legend("bottomright",lty=1,col=c("red","blue"),legend = c("train ", "test "))

