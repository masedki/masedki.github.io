rm(list=ls())
require(FactoMineR)
require(cluster)
# Importation des donnees

temperature <- read.table("~/Dropbox/enseignement/ENSAI/EvaluationDecisionsPubliques/TP/data/temperatures.csv", 
                          header = TRUE, sep = ";", dec = ".", row.names = 1)
summary(temperature)

#plot(temperature[,1:12], col=temperature[,17])

rownames(temperature)


Kmax <- 8
temperature <- scale(temperature[,1:12])



dataTocluster <- temperature[1:23, ]

othercities <- temperature[24:35, ]
res <- PCA(rbind(dataTocluster, othercities), ind.sup = 24:35, graph = FALSE)


#### Partie K-means
res.kmeans <- lapply(1:Kmax, kmeans, x = dataTocluster)

partitions <- sapply(res.kmeans, function(u) u$cluster)


# Variance intra
inertie.intra <- sapply(res.kmeans, function(u) u$tot.withinss)


# graphique des inertie
plot(inertie.intra, type = "b", xlab = "nb clusters")
kselected <- 3



# Evaluation de la qualite de la partition
si.kmeans <- silhouette(res.kmeans[[kselected]]$cluster, 
                        daisy(dataTocluster, metric = "euclidean"))
plot(si.kmeans)
plot(res$ind$coord[,1:2], col=res.kmeans[[kselected]]$cluster)  


# distances entre les autres villes et les centres
mydist <- matrix(0, nrow(othercities), kselected)
for (k in 1:kselected){
  for (i in 1:nrow(othercities)){
    mydist[i,k] <- sqrt(sum((othercities[i,] - res.kmeans[[kselected]]$centers[k,])**2))
  }
}


# prediction des classes pour les nouvelles villes
pred.newcities <- apply(mydist, 1, which.min)
names(pred.newcities) <- rownames(othercities)
points(res$ind.sup$coord[,1:2], col=pred.newcities, pch=15)  



#### Partie PAM
# Clustering
res.pam <- lapply(1:Kmax, pam, x = dataTocluster)
# Matrice des partitions
partitions <- sapply(res.pam, function(u) u$clustering)
kselected <- 3
si.kmeans <- silhouette(res.pam[[kselected]]$clustering, 
                        daisy(dataTocluster, metric = "euclidean"))
plot(si.kmeans)


#### Partie CAH
# Clustering
res.cah <- agnes(x = dataTocluster, 
                 metric = "euclidean", method = "ward")

# dendrogramme
plot(res.cah)
kselected <- 3
partition.cah <- cutree(res.cah, kselected)
si.kmeans <- silhouette(partition.cah, 
                        daisy(dataTocluster, metric = "euclidean"))
plot(si.kmeans)

# On compare les partitions issues de K-mean, PAM et CAH
partitions <- cbind(res.kmeans[[3]]$cluster,res.pam[[3]]$clustering, partition.cah)
table(partitions[,1], partitions[,2])
table(partitions[,1], partitions[,3])
