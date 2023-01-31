rm(list=ls())
require(jpeg)
img <- readJPEG("irm_small.jpeg")
image(img,col=grey((0:100)/100))

# Matrice de similarité
# On transforme d'abord l'image, stockée sous forme de matrice, en vecteur
img_v <- as.vector(img)
d <- as.matrix(dist(img_v))

# On choisit sigma=0.5 mais on peut tester d'autres valeurs
W <- exp(-d^2/(2*0.5^2))






# Graphe du epsilon-voisinage
seuil <- quantile(W,0.75)
Weps <- W
Weps[W<seuil] <- 0
Weps[W>=seuil] <- 1

Deps <- apply(Weps,1,sum) # matrice des degrés
mean(Deps) # degré moyen

# Matrice Laplacienne normalisée symétrique
Leps <- diag(length(img_v)) - diag(1/sqrt(Deps)) %*% Weps %*%  diag(1/sqrt(Deps))

# Spectre
speps <- eigen(Leps,symmetric=TRUE)

# Plot
n <- length(speps$values)
ggplot(data=data.frame(values=speps$values[(n-9):n],order=10:1),
       aes(x=order,y=values)) + geom_point() + ggtitle("Graphe du epsilon voisinage")

# Sur le graphe précédent, on remarque un saut entre la 3ème et la 4ème valeur propre, on retient donc 3 classes
# Matrice des 3 premiers vecteurs propres
Ueps <- speps$vectors[,(n-2):n]

# Normalisation par lignes
rownorm <- function(v){
  return(v/sqrt(sum(v^2)))
}

Veps <- t(apply(Ueps,1,rownorm))

# k-means sur les lignes 
cleps <- kmeans(Veps,3)

matcl <- matrix(cleps$cluster,nr=nrow(img),byrow = F)

# Plots
image(img,col=grey((0:100)/100)) # image originale
image(matcl) # clusters

# Pour superposer les clusters les uns après les autres
cl1 <- matcl
cl1[cl1!=1] <- NA
cl2 <- matcl
cl2[cl2!=2] <- NA
cl3 <- matcl
cl3[cl3!=3] <- NA

image(img,col=grey((0:100)/100))
image(cl1,col="blue",add=T)
image(cl2,col="orange",add=T)
image(cl3,col="yellow",add=T)

