rm(list=ls())
require(keras)
require(doParallel)
require(xgboost)
require(caret)

mnist   <- dataset_mnist()
x_train <- mnist$train$x
x_test  <- mnist$test$x

y_train_d = mnist$train$y
y_test_d = mnist$test$y
y_train <- factor(mnist$train$y)
y_test  <- factor(mnist$test$y)

# visualize the digits
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs="i", yaxs="i")
for (idx in 1:36) { 
  im <- x_train[idx,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        xaxt="n", main=paste(y_train[idx]))
}




# aplatir les images 
x_train_f = array_reshape(x_train, c(nrow(x_train), 784))
x_test_f = array_reshape(x_test, c(nrow(x_test), 784))
xm = do.call(rbind,list(x_train_f, x_test_f))
dim(xm)
all_vars = apply(xm, 2, var)

# on retire les colonnes de variance nulle 
x_train_f = x_train_f[, -which(all_vars==0)]
x_test_f = x_test_f[, -which(all_vars==0)]
xm = do.call(rbind,list(x_train_f, x_test_f))
dim(xm)


# on réduit la dimension avec une PCA 
# 80 %
colnames(xm) <- paste("x", 1:ncol(xm), sep="")
preProcValues <- preProcess(xm, method=c("pca"),  thresh = 0.8)
xm_tr <- predict(preProcValues, xm)
dim(xm_tr)
xm_train_tr <- xm_tr[1:60000,]
xm_test_tr  <- xm_tr[60001:70000,]
dim(xm_train_tr)
dim(xm_test_tr)

# random forest
control <- trainControl(method="cv", number=2)
rf.grid <-  expand.grid(mtry = 10*(1:7))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
rf.model <- train(xm_train_tr,
                 y_train,
                 method="rf", 
                 trControl=control,
                 ntree=50,
                 sampsize = 500,
                 tuneGrid = rf.grid,
                 verbose=FALSE)
stopCluster(cl)
plot(rf.model)
rf.pred = predict(rf.model, xm_test_tr)
# erreur de test rf 
mean(rf.pred!=y_test)
table(rf.pred, y_test)


# gradient boosting 
control <- trainControl(method="cv", number=2)
boost.grid = expand.grid(eta = 1,
                         nrounds = c(700, 750, 800, 850), # best : 750
                         max_depth = 2,
                         subsample = 1,
                         min_child_weight = 1.,
                         colsample_bytree = 0.5,
                         gamma = 0.)
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
boost.model <- train(xm_train_tr, 
                     y_train, 
                     method = "xgbTree",
                     metric = "Accuracy",
                     booster = "gbtree", # règle faible : arbre
                     trControl = control,
                     tuneGrid = boost.grid)

stopCluster(cl)
plot(boost.model)
boost.pred = predict(boost.model, xm_test_tr)
# erreur de test boost
mean(boost.pred!=y_test)
table(boost.pred, y_test)

# réseau de neurones 
# keras avec deux couches cachées sans convolution 
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# rescale
x_train <- x_train / 255
x_test <- x_test / 255
y_train_c <- to_categorical(y_train, 10)
y_test_c <- to_categorical(y_test, 10)

model_1 <- keras_model_sequential() 
model_1 %>% 
  layer_dense(units = 784, activation = 'relu', input_shape = c(784)) %>% 
  layer_dense(units = 10, activation = 'softmax')
summary(model_1)

model_1 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)
model_1 %>% fit(
  x_train, y_train_c, 
  epochs = 10, batch_size = 200, 
  validation_data = list(x_test, y_test_c))

# erreur de test keras
keras.pred = model_1 %>% predict_classes(x_test)
mean(keras.pred!=y_test)
table(keras.pred, y_test)
