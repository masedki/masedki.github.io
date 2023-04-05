setwd("~/codes/TP_R/")
require(caret)
#require(MASS)
#require(class)
require(doParallel)
require(parallel)
load("mixture_example.rda")
# 200 données de dimension 2 sous forme de matrice 200 x 2 
x <- data.frame(mixture.example$x)
dim(x)
head(x)
# les labels des données 0 ou 1 
y <- as.factor(mixture.example$y)
table(y)

knn_ctrl = trainControl(method="repeatedcv", number = 5, repeats = 100)

knn_grid = data.frame(k=1:70)

cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)
t0 = proc.time()
knn_model = train(x = x, 
                  y = y, 
                  method="knn",
                  trControl = knn_ctrl,
                  tuneGrid = knn_grid)
t1 = proc.time()
stopCluster(cl)
print(t1-t0)
print(knn_model)
plot(knn_model)
