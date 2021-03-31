setwd("~/codes/tp_R/")
require(caret)
require(MASS)
require(class)
require(doParallel)
load("mixture.example.rda")
# 200 données de dimension 2 sous forme de matrice 200 x 2 
x <- data.frame(mixture.example$x)
dim(x)
head(x)
# les labels des données 0 ou 1 
y <- as.factor(mixture.example$y)
table(y)

knn_ctrl = trainControl(method="repeatedcv", number = 5, repeats = 100)
knn_grid = expand.grid(k=c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151))


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

knn_model = train(x = x, 
                  y = y, 
                  method="knn",
                  trControl = knn_ctrl,
                  tuneGrid = knn_grid)
stopCluster(cl)
print(knn_model)
plot(knn_model)
