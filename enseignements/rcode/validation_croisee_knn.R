load("mixture_example.rda")
require(class)
require(caret)
require(doParallel)
require(parallel)

x <- as.data.frame(mixture.example$x)
dim(x)
head(x)

y <- as.factor(mixture.example$y)
table(y)

control = trainControl(method="repeatedcv", number=5, repeats = 100) 
knn_grid = data.frame(k=1:150)

print(detectCores())
cl = makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)
t0 = proc.time()
knnModel = train(x = x,
                 y = y,
                 method="knn",
                 trControl = control,
                 tuneGrid = knn_grid)
t1 = proc.time()
stopCluster(cl)

print(knnModel)
plot(knnModel)
print(t1-t0)