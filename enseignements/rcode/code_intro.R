load("mixture_example.rda")
require(class)

x <- mixture.example$x
dim(x)
head(x)
y <- mixture.example$y
table(y)

plot(x, col=ifelse(y==1,"red", "green"), xlab="x1", ylab="x2")

px1 = mixture.example$px1 # 
px2 = mixture.example$px2
xnew = mixture.example$xnew
dim(xnew)

plot(xnew, pch=".",xlab="x1", ylab="x2")





?knn 


mod15 = knn(train=x, test=xnew, cl=y, k=15, prob=TRUE)

summary(mod15)


prob = attr(mod15, "prob")

prob = ifelse(mod15=="1", prob, 1-prob) 

prob15 <- matrix(prob, length(px1), length(px2))

contour(px1, px2, prob15, levels=0.5, labels="", xlab="x1", ylab="x2", main="frontière de classement du modèle 15-nn")

points(x, col=ifelse(y==1, "red", "green"))



mod1 <- knn(x, xnew, k=1, cl=y, prob=TRUE)
prob <- attr(mod1, "prob")
prob <- ifelse( mod1=="1", prob, 1-prob) 
prob1 <- matrix(prob, length(px1), length(px2) )
contour(px1, px2, prob1, level=0.5, labels="", xlab="x1", ylab="x2", main="frontière de classement du modèle 1-nn")
points(x, col=ifelse(y==1, "red", "green"))




require(MASS)
set.seed(123)
centers = c(sample(1:10, 5000, replace=TRUE), sample(11:20, 5000, replace=TRUE))
means = mixture.example$means
means = means[centers, ]
xtest = mvrnorm(10000, c(0,0), 0.2*diag(2))
xtest = xtest + means
ytest = c(rep(0, 5000), rep(1, 5000))



K = c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151)

nK = length(K)

ErrTrain = rep(NA, length=nK)
ErrTest  = rep(NA, length=nK)
for (i in 1:nK) 
  {
  k = K[i] 
  modtrain = knn(x, x, k=k, cl=y)
  ErrTrain[i] = mean(modtrain!=y)
  modtest = knn(x, xtest, k=k, cl=y)
  ErrTest[i] = mean(modtest!=ytest)
}



plot(K, ErrTest, type="b", col="blue", xlab="nombre de voisins",ylab=" erreurs train et test", pch=20, 
     ylim=range(c(ErrTest, ErrTrain)))
lines(K, ErrTrain,type="b",col="red",pch=20)
legend("bottomright",lty=1,col=c("red","blue"),legend = c("train ", "test "))


