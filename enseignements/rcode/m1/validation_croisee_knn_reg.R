

require(class)
require(caret)
require(doParallel)
require(parallel)

load("insurance.rda")

summary(insurance)
insurance_bin = fastDummies::dummy_cols(insurance, 
                                        select_columns = c("sex", "smoker", "region"))
summary(insurance_bin)
control = trainControl(method="repeatedcv", number=5, repeats = 10) 
knn_grid = data.frame(k=1:150)
print(detectCores())
cl = makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)
t0 = proc.time()
knnModel = train(charges~.,
                 data = insurance_bin,
                 method="knn",
                 trControl = control,
                 tuneGrid = knn_grid)
t1 = proc.time()
stopCluster(cl)
print(knnModel)
plot(knnModel)
print(t1-t0)
