rm(list=ls())
library(glmnet)
set.seed(21)
D = read.table("breast_cancer.txt", header = T) # add your path here
dim(D) # rows patients, cols outcome + gene expressions

anyNA(D)

y = as.numeric(D[,1]) # survival time
X = as.matrix(D[,2:ncol(D)]) # gene expression measurements
rm(D)
n = length(y)
p = ncol(X)
summary(y)

summary(apply(X, 2, mean)) # not mean centered

summary(apply(X, 2, sd))


X = scale(X) # now mean = 0 and var = 1 for each column
y = y - mean(y) # mean-center, but do not scale to keep the interpretation of time units.
# we can now ignore the intercept because everything is mean-centered



beta.uni = as.vector((t(X)%*%y)/(n-1))
sigma2.uni = colSums(( y - t( t(X)*beta.uni) )**2)/(n-2) #this is sigma2 formula above written in R
sigma2.uni_v1 = colSums((matrix(rep(y, ncol(X)), nrow = n, byrow = F) - t(t(X)*beta.uni))**2)/(n-2)
se = sqrt(sigma2.uni/(n-1))
# Now we have fit > 24,000 linear models.
# Check an arbitrary column against lm() output:
i = 10625
summary(lm(y ~ X[,i]))$coefficients[2,1:2]
print(c(beta.uni[i], se[i]))
# OK.
pval = pchisq( (beta.uni/se)**2, df = 1, lower = F)
qqplot(-log10(ppoints(p)), -log10(pval), pch = 4) # huge deviation from the null
abline(0,1)
summary(pval)

## LASSO
cv.lasso = cv.glmnet(X, y, alpha = 1, nfolds =  5) # takes < 5 seconds even with p > 24,000
plot(cv.lasso)



plot(cv.lasso$glmnet.fit, xvar = "lambda")
abline( v = log(cv.lasso$lambda.min), lty = 2 )
abline( v = log(cv.lasso$lambda.1se), lty = 2 )




lasso.var = as.matrix(coef(cv.lasso, s = "lambda.min"))
lasso.var_v1 = as.matrix(cv.lasso$glmnet.fit$beta[,which(cv.lasso$lambda==cv.lasso$lambda.min)])
all(lasso.var==lasso.var_v1)

genes = names(which(abs(lasso.var[,1]) > 1e-10)) # names of the chosen genes
lasso.ind = which(abs(lasso.var[,1]) > 1e-10) - 1 # indexes, removing intercept index by -1
data.frame(gene = genes, beta = beta.uni[lasso.ind], se = se[lasso.ind], pval = pval[lasso.ind])
sum(pval < 1e-7)



## repeated cross-validation (caret)
require(caret)

lambda = cv.lasso$glmnet.fit$lambda 
ctrl = trainControl(method="repeatedcv", number = 5, repeats = 10)
lasso_grid = data.frame(lambda=lambda, 
                        alpha = 1)
fit_lasso = train(x=X,
                  y=y,
                  method = "glmnet",
                  trControl = ctrl,
                  tuneGrid = lasso_grid)
plot(fit_lasso)
fit_lasso$bestTune
## ridge
cv.ridge = cv.glmnet(X, y, alpha = 0) # takes only ~ 20 seconds even with p > 24000
plot(cv.ridge)


plot(cv.ridge$glmnet.fit, xvar = "lambda")
abline( v = log(cv.ridge$lambda.min), lty = 2 )
abline( v = log(cv.ridge$lambda.1se), lty = 2 )


## elasticnet
cverror = rep(NA, 11)
for(j in 0:10)
{
  fit =  cv.glmnet(X, y, alpha = (j/10))
  cverror[j+1] =  min(cv.lasso$cvm)
  assign(paste("fit_", j, sep=""), fit) 
}

which.min(cverror)

#plot(cv.enet)


plot(cv.enet$glmnet.fit, xvar = "lambda")
abline( v = log(cv.enet$lambda.min), lty = 2 )
abline( v = log(cv.enet$lambda.1se), lty = 2 )

