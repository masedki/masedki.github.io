require(kernlab)
data(spirals)
alpha = 1
d = as.matrix(dist(spirals))
S = exp(-alpha*d**2)
n = nrow(spirals)
A <- matrix(rep(0,n^2), ncol=n)
n.voisins = 4
for(i in 1:n) { # pour chaque ligne
  best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.voisins]
  for (s in best.similarities) {
    j <- which(S[i,] == s)
    A[i,j] <- S[i,j]
    A[j,i] <- S[i,j] 
  }
}

D = diag(rowSums(A))  
L = D - A

specL =  eigen(L, symmetric = TRUE) 
u = specL$vectors[,299:300]
plot(u[,1], u[,2])
testkm =  kmeans(u, centers = 2, nstart = 100)
plot(u[,1], u[,2], col = testkm$cluster)
plot(spirals, col=testkm$cluster)

