setwd("~/Dropbox/enseignement/M1/supports/notes/Rscripts/")
rm(list=ls())
# le jeu de données d'origine 
require (rlist) # le package qui permet la lecture du jeu de données à installer 
mnist <-list.load('mnist.rdata')
x_train <- mnist$train$x
x_test  <- mnist$test$x
y_train <- mnist$train$y
y_test  <- mnist$test$y
dim(x_train) # pour découvrir la dimension du jeu de données : un objet de dimension 3 une sorte de pile de matrices  

# visualisation du 9 première image du jeu de données
par(mfcol=c(3,3))
for (idx in 1:9) { 
  im <- x_train[idx,,] # on récupère l'image qui se trouve à la position idx de pile
  im <- t(apply(im, 2, rev)) # les images sont retournées donc on les remet 
  image(1:28, 1:28, im, xlab = "", ylab="", xaxt='n', main=paste(y_train[idx])) # on trace l'image avec le titre qui correspond à la réalité écrite dessus
}


# Comme les images sont des matrices de pixels, on doit les aplatir pour récupérer un vecteur de dimension nombre de lignes x nombre de colonnes
# lecture de la version aplatie du jeu de données
rm(list=ls())# on libérer la mémoire de ce qui a été déjà chargé
load("mnist_flatten.rda")
dim(x_train_f)
# on va supprimer les pixels qui n'apporte aucune information c'est à dire constant sur toutes les images
x_all_f = rbind(x_train_f, x_test_f) # attention : on doit supprimer les pixels constant sur toute la base de données 

# un pixel ou une colonne (variable) constant de x correspond aux colonnes de variances nulles
nvz = which(apply(x_all_f, 2, var) == 0)
x_train_f = x_train_f[ ,-nvz]
x_test_f = x_test_f[ ,-nvz]

# régression logistique elasticnet
require(glmnet)
require(doParallel) # calcul parallèle car temps lent 
cl <- makePSOCKcluster(8)
registerDoParallel(cl)
errcv = rep(NA, 11)
for(i in 0:10)
{
  cvfit=cv.glmnet(x_train_f, y_train, family="multinomial", # ici on y = 0, 1, ..., 9 : 10 classes
                  type.measure="class", 
                  nfolds=5,  alpha = i/10,
                  parallel=TRUE) # pour calculer en parallèle 
  assign(paste("fit", i, sep="."), cvfit)  
  errcv[i+1] = min(cvfit$cvm) 
}
stopCluster(cl)
