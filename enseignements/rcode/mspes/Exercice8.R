rm(list=ls())
require(VarSelLCM)
require(FactoMineR)
require(missMDA)

# Fonction de generation des donnees
rmixture <- function(n=100, delta=2.5, d.disc=2, d.nondisc=0, na.rate=0){
  z <- sample(1:2, n, replace = TRUE)
  x <- matrix(rnorm(n*(d.disc+d.nondisc)), n)
  x[which(z==2), 1:d.disc] <- x[which(z==2), 1:d.disc]*2 + delta
  xnona <- x
  if (na.rate>0){
    for (j in 1:ncol(x)){
      x[which(runif(n)<na.rate),j] <- NA
    }
  }
  list(x=as.data.frame(x), z=z, xnona = as.data.frame(xnona))
}

## question 2 du TP ex.8
ech = rmixture(n=100, delta=2.5, d.disc=2, d.nondisc = 1, na.rate=0)
res.mixture.nosel <- VarSelCluster(ech$x, 2, vbleSelec = FALSE)
res.mixture.sel <- VarSelCluster(ech$x, 2, vbleSelec = TRUE, crit.varsel = "BIC")
ARI(ech$z, res.mixture.nosel@partitions@zMAP)
ARI(ech$z, res.mixture.sel@partitions@zMAP)


ech.all = replicate(50, rmixture(n=100, delta=2.5, d.disc=2, d.nondisc = 1, na.rate=0), simplify = F)

OneTest = function(ech)
{
  
  res.mixture.nosel <- VarSelCluster(ech$x, 2, vbleSelec = FALSE)
  res.mixture.sel <- VarSelCluster(ech$x, 2, vbleSelec = TRUE, crit.varsel = "BIC")
  
  c(ARI(ech$z, res.mixture.nosel@partitions@zMAP),
    ARI(ech$z, res.mixture.sel@partitions@zMAP))

}
Test = sapply(ech.all, OneTest)
rowMeans(Test)


# Importance de la gestion de valeurs manquantes
OneExpe <- function(ech){
  if (any(is.na(ech$x))){
    out <- imputePCA(ech$x, ncp = ncol(ech$x)-1)
    ech.nona <- out$completeObs
  }else{
    ech.nona <- ech$x
  }
  z.kmeans <- kmeans(ech.nona, 2)$cluster
  res.mixture.nosel <- VarSelCluster(ech$x, 2, vbleSelec = FALSE)
  z.mixture.nosel <- fitted(res.mixture.nosel)
  res.mixture.sel <- VarSelCluster(ech$x, 2, vbleSelec = TRUE, crit.varsel = "BIC")
  z.mixture.sel <- fitted(res.mixture.sel)
  c(ARI(ech$z, z.kmeans), ARI(ech$z, z.mixture.nosel), ARI(ech$z, z.mixture.sel))
}

ech <- rmixture(100, delta=2.5, d.disc = 3, na.rate=0, d.nondisc = 50)
OneExpe(ech)

par(mfrow=c(2,2))

for (d.nondisc in c(0, 3, 6, 9)){
  rate <- c(0, .1, .2, .3, .4)
  results <- matrix(0, length(rate), 3)
  cp <- 0
  for (r in rate){
    cp <- cp + 1
    all.ech <- replicate(20, rmixture(100, delta=4, d.disc = 3, na.rate=r, d.nondisc = d.nondisc), simplify = FALSE)
    out <- sapply(all.ech, OneExpe)
    results[cp,] <- rowMeans(out)
  }
  plot(NA, xlim=range(rate), ylim=range(results))
  for (k in 1:3){
    lines(rate, results[,k], col=k)
  }
}


ech <- rmixture(100, delta=2.5, d.disc = 4, na.rate=0.1, d.nondisc = 4)
OneExpe(ech)

colnames(ech$x) <- colnames(ech$xnona) <- paste0("X", 1:8)
res <- VarSelCluster(ech$x, 2)
mean((ech$xnona - VarSelImputation(res, newdata = ech$x, method = "sampling"))**2)
out <- imputePCA(ech$x, ncp = ncol(ech$x)-1)
mean(ech$xnona -out$completeObs)**2