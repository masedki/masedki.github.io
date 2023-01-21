rm(list=ls())
library(FactoMineR)


####################################################
#### Exemple d'ACP: Données dépenses des ménages

donnees <- read.table("~/Dropbox/enseignement/ENSAI/EvaluationDecisionsPubliques/exemples/data/base_conso.csv", 
                      header = TRUE,
                      sep=";",
                      row.names = 1
)
# choix des individus actifs
X <- donnees[1:7,]

# quelques stat descriptives
summary(X)
summary(cor(X))

# choix de la métrique
apply(X, 2, sd)/apply(X, 2, mean)

# mise en oeuvre de l'ACP
res.pca <- PCA(donnees, ind.sup = 8:18, quanti.sup = 27:30, scale.unit = TRUE, graph = FALSE)

# resumé des résultats
summary(res.pca, nb.dec = 2, nbelements = Inf)

# choix du nombre de dimension
barplot(res.pca$eig[,1], main="Valeurs propres", names.arg = paste0("dim", 1:nrow(res.pca$eig)))

### Plan 1-2
# Plan 1-2: nuage des individus actifs
plot.PCA(res.pca, choix = "ind", invisible = "ind.sup")

# Plan 1-2: nuage des variables actives
plot.PCA(res.pca, choix = "var", invisible = "quanti.sup", cex=0.75)
round(cor(X[,c(1,4,17)]),2)

plot.PCA(res.pca, choix = "var", invisible = "var")

# Description automatique des dimensions
dimdesc(res.pca)

# Plan 1-2: nuage des individus supplémentaires
plot.PCA(res.pca, choix = "ind")

### Plan 2-3
plot(res.pca, choix = "ind", axes = 2:3)
plot(res.pca, choix = "var", axes = 2:3, cex=0.75)

# Retour aux données
res.pca$call$centre
res.pca$call$ecart.type

round(scale(X[,c(5,6,15,17,19,21)]) * sqrt(6/7), 2)
par(las=2)
plot(X[,21], type = "b", axes = F, ylab = "Communication (en Euros)", xlab = "", bty="o")
axis(2)
axis(1, 1:7, rownames(X)[1:7])
par(las=0)

pairs(X[,1:4])

##############################################################################################################
# Example ACP: methode de reduction de dimension
rm(list=ls())
library(pixmap)
te <- read.pnm("~/Dropbox/enseignement/ENSAI/EvaluationDecisionsPubliques/exemples/data/Lena_soderberg.ppm", cellres=1)

# affiche l' image
plot(te)

# affiche chaque couleur de base
par(mfrow=c(2,2), mar=c(1,1,1,1))
cp=te
cp@red=cp@red*1
cp@green=cp@green*0
cp@blue=cp@blue*0
plot(cp)
cp=te
cp@red=cp@red*0
cp@green=cp@green*1
cp@blue=cp@blue*0
plot(cp)
cp=te
cp@red=cp@red*0
cp@green=cp@green*0
cp@blue=cp@blue*1
plot(cp)

# creation de la matrice ACP
# en ligne les pixels (individus)
# en colonne les couleurs RGB (variables)
data <- cbind(as.vector(te@red),as.vector(te@green),as.vector(te@blue))

# ACP
res.pca=PCA(data,ncp=3,graph=FALSE,scale.unit=FALSE)
summary(res.pca)

rec = reconst(res.pca,ncp=1)
im <- te
im@red <- matrix(rec[,1],nrow(te@red),ncol(te@red))
im@green <- matrix(rec[,2],nrow(te@green),ncol(te@green))
im@blue <- matrix(rec[,3],nrow(te@blue),ncol(te@blue))


im@red[im@red<0]=0
im@green[im@green<0]=0
im@blue[im@blue<0]=0
im@red[im@red>1]=1
im@green[im@green>1]=1
im@blue[im@blue>1]=1
plot(im)


####################################################
#### Exemple d'AFC: Données sur les causes de décés
rm(list=ls())
library(FactoMineR)

donnees <- read.table("data/deces.csv", 
                      header = TRUE,
                      sep=";",
                      row.names = 1, 
                      check.names = FALSE)
# Statistiques descriptives
dim(donnees)
rowSums(donnees)
colSums(donnees)
sum(donnees)

# Test d'indépendance sur tout le monde
chisq.test(donnees)
# Test d'indépendance sur les donnees actives
chisq.test(donnees[1:66,])

# AFC
res.ca <- CA(donnees, row.sup = 66:nrow(donnees), graph = FALSE)
round(res.ca$call$marge.col, 3)
round(res.ca$call$marge.row[order(res.ca$call$marge.row)], 3)
par(las = 1, mar=c(4,20,4,1))
barplot(res.ca$call$marge.col, horiz = TRUE, cex.names = 0.7)
barplot(res.ca$call$marge.row[order(res.ca$call$marge.row)], horiz = TRUE, cex.names = 0.7)

summary(res.ca, nb.dec = 4)
par(las = 0, mar=c(4,4,4,1))
barplot(res.ca$eig[,1], main = "Valeurs propres", names.arg = 1:nrow(res.ca$eig))

inertia.col <- round(res.ca$col$inertia / sum(res.ca$col$inertia), 3)
names(inertia.col) <- colnames(donnees)
inertia.col

inertia.row <- res.ca$row$inertia / sum(res.ca$row$inertia)
names(inertia.row) <- rownames(donnees)[1:65]
head(inertia.row[order(inertia.row, decreasing = T)])

tab <- data.frame(poids = res.ca$call$marge.col, 
                  distance = sqrt(res.ca$col$inertia/res.ca$call$marge.col),
                  inertie = res.ca$col$inertia,
                  percent.inertie = res.ca$col$inertia/sum(res.ca$col$inertia))
round(tab, 4)


plot.CA(res.ca, axes = 1:2, invisible = c("row", "row.sup"))

plot.CA(res.ca, axes = 1:2, invisible = c("row.sup"), selectRow = order(inertia.row, decreasing = T)[1:10])

plot.CA(res.ca, axes = 2:3, invisible = c("row", "row.sup"))

plot.CA(res.ca, axes = 2:3, invisible = c("row.sup"), selectRow = order(inertia.row, decreasing = T)[1:10])

res.ca <- CA(donnees, row.sup = 66:nrow(donnees), ncp=Inf, graph = FALSE)
round(dist(res.ca$col$coord), 3)

summary(res.ca, nb.dec = 3)

head(cbind(res.ca$row$contrib[,2], 
           res.ca$row$cos2[,2],
           res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,2])),])

head(cbind(res.ca$row$contrib[,3], 
           res.ca$row$cos2[,3],
           res.ca$call$marge.row)[rev(order(res.ca$row$contrib[,3])),])

plot.CA(res.ca, axes = 2:3)

plot.CA(res.ca, axes = 2:3, selectCol="cos2 0.9",selectRow="cos2 0.9", cex = 0.8)

# On supprime les élements supplémentaires qui ne correspondent pas aux totaux de
# décès par année entre 1979 et 2006
res.ca$row.sup$coord <- res.ca$row.sup$coord[130:157,]
plot.CA(res.ca, invisible = c("row", "col"), axes = 2:3)
points(res.ca$row.sup$coord[,2:3], type="l")


####################################################
#### Exemple d'AFCM: Preception des OGM
rm(list=ls())

ogm <- read.table("data/ogm.csv", 
                  header = TRUE,
                  sep=";"
                  )
summary(ogm)
dim(ogm)

# On regroupe des modalités très peu observées
levels(ogm$Position.Al.H)[4] <- levels(ogm$Position.Al.H)[1]
levels(ogm$Position.Culture) <- c("Favorable", "Pas Favorable du Tout", "Plutot Defavorable", "Favorable")

summary(ogm)


res <- MCA(ogm, ncp = 5, quali.sup = 17:21, graph = FALSE)

plot.MCA(res, invisible = c("var", "quali.sup"), label = "none")

plot.MCA(res, invisible = c("ind", "quali.sup"), label = "none")

plot.MCA(res, invisible = c("ind", "quali.sup"), cex=.8)

plot.MCA(res, col.quali.sup="green", invisible = c("quanti.sup", "ind", "var"))

res$ind$coord
