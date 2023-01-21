rm(list=ls())
setwd("~/Dropbox/enseignement/ENSAI/EvaluationDecisionsPubliques/TP")
require(FactoMineR)
# Importation des donnees
temperature <- read.table("data/temperatures.csv",header = TRUE, sep = ";", dec = ".", row.names = 1)


##Stat descriptives (on verifie l'importation des donnees)
summary(temperature)
# Graph bivariee avec la region en couleur
plot(temperature[,1:12], col=temperature[,17])
# Liste des villes 
rownames(temperature)
# On regarde la matrice de correlation pour savoir si l'ACP est pertinente
correlations <- round(cor(temperature[,-17]), 2)
# Attention pour visualiser, il vaut mieux inverser les colonnes (plus c'est rouge, plus c'est corrélé)
image(correlations[,16:1])
# On met en individu actif les capitales et en variables actives les mesures au cours des 12 mois
res <- PCA(temperature, ind.sup = 24:35, quanti.sup = 13:16, quali.sup = 17, graph = FALSE)
# Pour retenir le nombre d'axes
barplot(res$eig[,1])
# Nuage des individus dans le premier plan factoriel avec la region en couleur
plot(res, choix = "ind", habillage = 17)
# Resume des sorties de l'ACP
summary(res)
# Resume pratique des axes en fonction des variables de départ
dimdesc(res)
# Cercle des correlations (quel bel effet taille!)
plot(res, choix = "var")
# On regarde ce qui se passe dans le plan 2/3 (mais en faite l'axe 3 n'explique rien car pas d'inertie)
plot(res, choix = "ind", axes = c(2,3), habillage = 17)
plot(res, choix = "var", axes = c(2,3), habillage = 17)
# Ellipses de confiance en supposant que les variables suivent une Gaussienne multivariée conditionnelle à la variable quali
plotellipses(res, cex=.8, level = .95)
