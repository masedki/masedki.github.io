rm(list=ls())
setwd("~/Dropbox/enseignement/ensta/TP/R_codes_corrige/")
require(class)
load("mixture_example.rda")
x <- mixture.example$x
dim(x)
head(x)
y <- mixture.example$y
table(y)


plot(x, col=ifelse(y==1,"red", "green"), xlab="x1", ylab="x2")





px1 = mixture.example$px1 
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
contour(px1, px2, prob15, levels=0.5, 
        labels="0.5", xlab="x1", ylab="x2", 
        main="frontière de classement du modèle 15-nn")



mod2 = knn(train=x, test=xnew, cl=y, k=2, prob=TRUE)
summary(mod2)
prob = attr(mod2, "prob")
prob = ifelse(mod2=="1", prob, 1-prob) 
prob2 <- matrix(prob, length(px1), length(px2))
contour(px1, px2, prob2, levels=0.5, 
        labels="0.5", xlab="x1", ylab="x2", 
        main="frontière de classement du modèle 2-nn")


mod180 = knn(train=x, test=xnew, cl=y, k=180, prob=TRUE)
summary(mod180)
prob = attr(mod180, "prob")
prob = ifelse(mod180=="1", prob, 1-prob) 
prob180 <- matrix(prob, length(px1), length(px2))
contour(px1, px2, prob180, levels=0.5, 
        labels="0.5", xlab="x1", ylab="x2", 
        main="frontière de classement du modèle 180-nn")
points(x, col=ifelse(y==1, "red", "green"))

probbayes = mixture.example$prob

probbayes <- matrix(probbayes, length(px1), length(px2))

contour(px1, px2, probbayes, 
        levels=0.5, 
        labels="", xlab="", ylab="",
        main="",
        col="blue",
        lwd=2,
        add=TRUE)

#points(x, col=ifelse(y==1, "red", "green"))
mod1 <- knn(x, xnew, k=1, cl=y, prob=TRUE)
prob <- attr(mod1, "prob")
prob <- ifelse( mod1=="1", prob, 1-prob) 
prob1 <- matrix(prob, length(px1), length(px2) )
contour(px1, px2, prob1, level=0.5, labels="", xlab="x1", ylab="x2", 
        main="frontière de classement du modèle à 1-nn")
points(x, col=ifelse(y==1, "red", "green"))




require(MASS)
set.seed(123)
centers = c(sample(1:10, 5000, replace=TRUE), sample(11:20, 5000, replace=TRUE))
means = mixture.example$means
means = means[centers, ]
xtest = mvrnorm(10000, c(0,0), 0.2*diag(2))
xtest = xtest + means
ytest = c(rep(0, 5000), rep(1, 5000))




#K = c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151)
K = 1:150
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
 
plot(K, ErrTest, type="b", col="blue", 
     xlab="nombre de voisins",
     ylab=" erreurs train et test", pch=20, 
     ylim=range(c(ErrTest, ErrTrain)))

lines(K, ErrTrain,type="b",col="red",pch=20)

legend("bottomright",lty=1,col=c("red","blue"),
       legend = c("train ", "test "))
which.min(ErrTrain)
which.min(ErrTest)
