# lecture de la librairie qui permet de reproduire une grande partie des exemples du livre : Element of Statistical Learning
# pour l'installer : install.packages("ElemStatLearn", dep=TRUE)
require(ElemStatLearn)
# lecture de la librairie  qui permet d'ajuster des modèles d'apprentissage par les knn
require(class)
# on charge le jeu de données mixture.example de la librairie ElemStatLearn
data("mixture.example")
# 200 données de dimension 2 sous forme de matrice 200 x 2 
x <- mixture.example$x
dim(x)
head(x)
# les labels des données 0 ou 1 
y <- mixture.example$y
table(y)
# on trace le nuage de points avec couleur rouge si le label du point vaut 1 et vert s'il vaut 0. 
plot(x, col=ifelse(y==1,"red", "green"), xlab="x1", ylab="x2")
# un autre nuage de points sous forme de matrice 6831 x 2 sous forme de grille régulière 
px1 = mixture.example$px1 # 
px2 = mixture.example$px2
xnew = mixture.example$xnew
dim(xnew)
# cette grille permet d'obtenir un maillage de l'espace comme suit
plot(xnew, pch=".",xlab="x1", ylab="x2")
# cette grille a été obtenue en composant tous les couples qu'on peut former comme suit 
# première coordonnée de px1 et seconde coordonnée de px2.

# modèle à 15 plus proche voisins basé sur les 200 données observées x et y
# aide de la fonction knn identifier les différents paramètres d'entrées. 
?knn 
########################################################################################
######################### modèle à 15 plus proches voisins #############################
########################################################################################
# les paramètres de la fonction 
# train : les variables explicatives des données d'entrainement  :  x 
# test : le jeu de données pour lequel on a envie de prédire la variable réponse à partir des données d'entrainement
# cl : les classes des individus du jeu de données d'entrainement:  y
# k : le nombre de voisins du modèle
# prob = TRUE permet de renvoyer les votes pour prédire la classe 1 de chaque point du jeu de données test
mod15 = knn(train=x, test=xnew, cl=y, k=15, prob=TRUE)
# résumé des sorties du modèle mod15
summary(mod15)
# les effectifs des prédictions des deux classes 0 et 1 pour le jeu de données test
# On récupère les proportions de votes de la classe 1 pour chaque point du jeu de données test 
prob = attr(mod15, "prob")
# Les probas de votes de la classe majoritaire 
prob = ifelse(mod15=="1", prob, 1-prob) 
# On construit la la matrice qui donne la proprtion de votre de la classe majoritaire pour chaque point de la grille qui est le jeu de données test. 
prob15 <- matrix(prob, length(px1), length(px2))
# On trace la frontière de classement où ces proportions vallent 0.5 (là où on bascule d'une classe à une autre)
contour(px1, px2, prob15, levels=0.5, labels="", xlab="x1", ylab="x2", main="frontière de classement du modèle 15-nn")
# ajout du points du jeu de données d'apprentissage au modèle
points(x, col=ifelse(y==1, "red", "green"))


######################################################################################
######################## modèle à 1 plus proches voisins #############################
######################################################################################
# On refait exactement les mêmes étapes précédentes 
mod1 <- knn(x, xnew, k=1, cl=y, prob=TRUE)
prob <- attr(mod1, "prob")
prob <- ifelse( mod1=="1", prob, 1-prob) 
prob1 <- matrix(prob, length(px1), length(px2) )
contour(px1, px2, prob1, level=0.5, labels="", xlab="x1", ylab="x2", main="frontière de classement du modèle 1-nn")
points(x, col=ifelse(y==1, "red", "green"))



# Nous remarquons que la frontière de classement du modèle 1-nn est plus fine que celle du modèle 15-nn 
# La complexité du modèle knn est inversement proportionnelle aux nombre de voisins k 
# on simule un jeu de données test de taille 10000 avec le même modèle qui a permet d'obtenir les 200 point d'apprentissage x, y
# le bloc de code suivant permet de simuler le jeu de données test : xtest et ytest
require(MASS)
set.seed(123)
centers = c(sample(1:10, 5000, replace=TRUE), sample(11:20, 5000, replace=TRUE))
means = mixture.example$means
means = means[centers, ]
xtest = mvrnorm(10000, c(0,0), 0.2*diag(2))
xtest = xtest + means
ytest = c(rep(0, 5000), rep(1, 5000))


# Nous allons comparer deux type d'erreurs  comme suit : 
# Dans les deux cas, nous allons construire un modèle sur le jeu de données d'apprentissage x, y et nous comparons :
# Erreur de classement du jeu de données d'apprentissage x, y en fonction du nombre de voisins
# Erreur de classement du jeu de données test xtest, ytest en fonction du nombre de voisins

# le vecteur suivant contient les valeurs de K à tester : on peut évidemment tester d'autres valeurs
K = c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151)
# le nombre de valeurs à tester 
nK = length(K)
# deux vecteurs vides pour stocker les deux types d'erreurs 
ErrTrain = rep(NA, length=nK)
ErrTest  = rep(NA, length=nK)
# une boucle qui permet de parcourir les valeurs de k 
for (i in 1:nK) 
  {
  # valeur courante de k 
  k = K[i] 
  # modèle pour classer le jeu de données d'apprentissage
  modtrain = knn(x, x, k=k, cl=y)
  # proportion de mal classés du jeu de données d'apprentissage
  ErrTrain[i] = mean(modtrain!=y)
  # modèle pour classer le jeu de données de test (appelé aussi validation)
  modtest = knn(x, xtest, k=k, cl=y)
  # proportion de mal classés du jeu de données de test
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

# Conclusion : 
# le modèle le plus complèxe est désigné comme le plus performant par l'erreur d'apprentissage
# c'est le phénomène de sur-apprentissage ou sur-ajustement.
# l'erreur de test permet de contourner ce phénomène. 
