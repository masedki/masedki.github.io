require(leaps)
load("~/Dropbox/enseignement/ml/Lectures/rmds/prostate.rda")
#retinol <- read.csv2("~/recherche/codes/masedki/enseignements/datasets/presentationTPretinol.csv", header = T)
names(prostate)

regfit.full = regsubsets(lcavol~. , prostate)
summary(regfit.full)

regfit.full = regsubsets(lcavol~., data = prostate, nvmax=13)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)

par(mfrow=c(2 ,2))
plot(reg.summary$rss , xlab =" Number of Variables " , ylab =" RSS " ,type ="l")
plot(reg.summary$adjr2 , xlab =" Number of Variables " , ylab =" Adjusted RSq " , type ="l")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col =" red " , cex =2 , pch =20)

plot(reg.summary$cp , xlab =" Number of Variables " , ylab =" Cp " ,type = "l")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col =" red " , cex =2 , pch =20)

plot(reg.summary$bic , xlab =" Number of Variables " , ylab =" BIC " , type ="l")
which.min(reg.summary$bic)
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col =" red " , cex =2 , pch =20)

plot(regfit.full , scale ="r2")
plot(regfit.full , scale ="bic")
plot(regfit.full , scale ="adjr2")
plot(regfit.full , scale ="Cp")

coef(regfit.full , 2)

regfit.fwd = regsubsets(lcavol~. , data = prostate, method ="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="bic")
regfit.bwd = regsubsets(lcavol~. , data = prostate, method ="backward")
summary(regfit.bwd)
plot(regfit.bwd, scale="bic")

set.seed(1)
train = sample (c(TRUE,FALSE) , nrow(prostate), prob=c(2/3,1/3) , rep = TRUE)
test=(!train)
regfit.best = regsubsets(lcavol~. , data = prostate[train,])
test.mat = model.matrix(lcavol~. , data = prostate[test,])

val.errors = rep(NA ,8)
for( i in 1:8){
  coefi = coef(regfit.best,  id=i)
  pred = test.mat[ ,names(coefi)]%*%coefi
  val.errors[i]=mean((prostate$lcavol[test] - pred)^2)
}
val.errors
which.min(val.errors)
