# Exemple de données d'apprentissage
# 150 gaussiennes standard
ntr <- 150
x = rnorm(ntr)
# Quadratic Y's
y = 2.5*x^2 - 0.5*x + rnorm(ntr)
# Tracer le nuage de point avec la vraie courbe
plot(x,y, pch=16, col=8)
curve(2.5*x^2-0.5*x,add=TRUE)
# Ajustons deux modèles basiques
# La constante égale à la moyenne des yi
plot(x,y, pch=16, col=8)
curve(2.5*x^2-0.5*x,add=TRUE)
y.0 = lm(y ~ 1)
abline(h=y.0$coefficients[1])
# La droite de régression linéaire
d = seq(min(x),max(x),length.out=200) # nous avons besoin d'une grille de x
degree = 1
fm = lm(y ~ poly(x,degree))
assign(paste("y",degree,sep="."), fm)
lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))

#ajout des polynômes de 2 à 9
for(degree in 2:9)
{ 
  # la commande poly(x, degree) permet de calculer les puissances du vecteur x allant de 1 à degree
  fm = lm(y ~ poly(x,degree))
  # la commande assign permet de stocker fm dans un objet y.degree au fur et à mesure
  assign(paste("y",degree,sep="."), fm) ## cette commande permet de stocker les 
  # la lines permet d'ajouter la courbe à chaque fois.
  lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}


# Ajout de 150 points test aux figures précédentes
nnew <- 150
x.new = rnorm(nnew)
y.new = 2.5*x.new^2 - 0.5*x.new + rnorm(nnew)
#le nuage de points test en bleu  
plot(x.new,y.new,xlab="x",ylab="y",pch=24,cex=0.1,col="blue")
curve(2.5*x^2-0.5*x,col="grey",add=TRUE)
abline(h=y.0$coefficients[1])
d = seq(from=min(x.new),to=max(x.new),length.out=200)
for (degree in 1:9) {
  # la fonction get fait l'inverse de la fonction assign on met dans l'objet fm ce que contient l'objet y.degree
  fm = get(paste("y",degree,sep="."))
  lines(d, predict(fm,data.frame(x=d)),lty=(degree+1))
}

# Calcul des deux erreurs test et apprentissage
err.train = rep(NA, 10) # vecteur vide de taille 10
err.test =  rep(NA, 10)
for (degree in 0:9) {
  fm = get(paste("y",degree,sep="."))
  predictions = predict(fm,data.frame(x=x.new))
  resids = y.new - predictions
  err.train[degree+1] = mean(residuals(fm)^2)
  err.test[degree+1] = mean(resids^2)
}

# erreur d'apprentissage
plot(0:9,err.train,type="b",xlab="polynomial degree",
     ylab="train and test error",log="y",ylim=range(c(err.train, err.test)))
# erreur de test
lines(0:9,err.test,lty=2,col="blue")

# validation croisée à 5 folds
K <- 5 # nombre de folds
nnew <- 500 # le jeu de données en entier
indices.cv <- matrix(1:nnew, ncol = K, byrow=F) # chaque colonne de cette matrice contient les indices d'un folds  
err.cv <- matrix(NA, 5,10)
x.new = rnorm(nnew)
y.new = 2.5*x.new^2 - 0.5*x.new + rnorm(nnew)
# boucle sur les valeurs de degré
for (degree in 0:9) {
  # boucle sur l'indice du fold
  for(k in 1:K)
  {
    y <- y.new[c(indices.cv[,-k])]
    x <- x.new[c(indices.cv[,-k])]
    if(degree==0) 
      fm = lm(y~1)
    
    else
      fm = lm(y~poly(x,degree))
    
    predictions = predict(fm, data.frame(x=x.new[indices.cv[,k]]))
    err.cv[k, degree+1] = mean((y.new[indices.cv[,k]] - predictions)**2)
  }# fin de la boucle sur l'indice du fold
}# fin de la boucle sur le degré

plot(0:9, colMeans(err.cv),type="b",xlab="polynomial degree",
     ylab="cv error",log="y")
# Meilleur degré au sens de l'erreur de validation croisée
which.min(colMeans(err.cv))-1
