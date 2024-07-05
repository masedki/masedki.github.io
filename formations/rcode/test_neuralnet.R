rm(list=ls())
load("insurance.rda")
require(neuralnet)
insurance_bin = fastDummies::dummy_cols(insurance, 
                                        select_columns = c("sex","smoker","region"),
                                        remove_first_dummy = TRUE,
                                        remove_selected_columns = TRUE)
set.seed(500)
insurance_bin
apply(insurance_bin,2,function(x) sum(is.na(x)))
maxs <- apply(insurance_bin, 2, max) 
mins <- apply(insurance_bin, 2, min)
scaled <- as.data.frame(scale(insurance_bin, center = mins, scale = maxs - mins))
index <- sample(1:nrow(insurance_bin),round(0.75*nrow(insurance_bin)))
insurance_tr <- scaled[index,]
insurance_te <- scaled[-index,]

n <- names(insurance_tr)
f <- as.formula(paste("charges~", paste(n[!n %in% "charges"], collapse = " + ")))
nn <- neuralnet(f, data=insurance_tr, hidden=c(5, 3),linear.output=T)
plot(nn)
pr.nn <- compute(nn, insurance_te[,-4])
pr.nn_ <- pr.nn$net.result*(max(insurance_bin$charges)-min(insurance_bin$charges))+min(insurance_bin$charges)
test.r <- (insurance_te$charges)*(max(insurance$charges)-min(insurance$charges))+min(insurance$charges)
MSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(insurance_te))
print(MSE.nn)
