rm(list=ls())
require(MASS)
require(glmnet)

y = scale(Boston$medv)
X = scale(Boston[, -which(colnames(Boston) %in% "medv")])
dim(Boston)
dim(X)
colnames(X)

reg_ridge = glmnet(X, y, alpha = 0) 
plot(reg_ridge)

reg_lasso = glmnet(X, y, alpha = 1) 
plot(reg_lasso)

cv_ridge = cv.glmnet(X, y, alpha = 0, nfolds = 5)
plot(cv_ridge)
dim(cv_ridge$glmnet.fit$beta)
length(cv_ridge$lambda)


cv_ridge$glmnet.fit$beta[, which(cv_ridge$lambda==cv_ridge$lambda.min)]
cv_ridge$glmnet.fit$beta[, which(cv_ridge$lambda==cv_ridge$lambda.1se)]


grille_lambda = exp(log(seq(0.00001, 0.1, length = 50)))

cv_ridge_v1 = cv.glmnet(X, y, alpha = 0, nfolds = 5, lambda = grille_lambda)
plot(cv_ridge_v1)
dim(cv_ridge_v1$glmnet.fit$beta)
length(cv_ridge_v1$lambda)


cv_ridge$glmnet.fit$beta[, which(cv_ridge$lambda==cv_ridge$lambda.min)]
cv_ridge$glmnet.fit$beta[, which(cv_ridge$lambda==cv_ridge$lambda.1se)]


beta = rep(0, ncol(X))
epsilon = 0.001
niter = 2500
Beta = matrix(NA, ncol(X), niter)
Beta[,1] = beta
for(iter in 2:niter)
{
  r = y - X%*%beta
  c = cor(cbind(r, X))
  r_c = c[1, -1]
  j = which.max(abs(r_c))
  s = ifelse(r_c[j]> 0, 1, -1)
  beta[j] = beta[j] + epsilon * s 
  Beta[,iter] = beta
}
plot(Beta[1,], type="l", ylim = range(Beta), ylab="beta", xlab="iter")
for(j in 2:nrow(Beta))
  lines(Beta[j,], type="l", col=j)
