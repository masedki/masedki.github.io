rm(list=ls())
install.packages("HGDP.CEPH", repos="https://genostats.github.io/R/")
require(gaston)
require(HGDP.CEPH)
require(VarSelLCM)
# lire données
filepath <-system.file("extdata", "hgdp_ceph.bed", package="HGDP.CEPH")
x <- read.bed.matrix(filepath)

# données SNP et individus
head( x@snps )
head( x@ped )

# comment les régions sont recodées en 7 grandes régions
table(x@ped$region, x@ped$region7)

# ajouter callrate, maf, etc
x <- set.stats(x)

# virer SNPs rares et non-autosomaux
x <- select.snps(x, maf > 0.05 & is.autosome(chr))

# calcul matrice de corrélation inter individus
K.x = GRM(x)

# eigen decomposition
eigen.K.x <- eigen(K.x)

# ébouli des valeurs propres
plot(eigen.K.x$values)

plot(eigen.K.x$vectors[,1:2], col=x@ped$region7)

legend("top",lty=1,col= 1:nlevels(x@ped$region7), legend = levels(x@ped$region7))


km.acp <- kmeans(eigen.K.x$vectors[,1:2], centers=7, nstart=500)
table(km.acp$cluster, x@ped$region7)
ARI(km.acp$cluster, x@ped$region7)


dist.x = K.x
#dist.x = eigen.K.x <- eigen(K.x)$vectors
# clustering spectral
W =  sqrt(pmax(length(x@snps$id)*(dist.x),0))
#W =  sqrt(pmax((dist.x),0))
D  =  diag(apply(W, 1, sum))
Dsqrt.inv = diag(1/sqrt(diag(D)))
I  = diag(rep(1, ncol(W)))
L = Dsqrt.inv%*%W%*%Dsqrt.inv
evL = eigen(L, symmetric=TRUE)
#delta = diff(evL$values)
#seuil  = - 0.00016 + (2.7/nrow(K.x)) + (2.3/length(x@snps$id))
#d  = sum(abs(delta) > seuil) 
d = 7
#lambda = pmax(1-evL$values[1:d],0)
phi  =  evL$vectors[,1:d]
#for(j in 1:d)
#  phi[,j] = lambda[j]*phi[,j]
km <- kmeans(phi, centers=d, nstart=500)
table(km$cluster, x@ped$region7)
ARI(km$cluster, x@ped$region7)

