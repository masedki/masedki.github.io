rm(list=ls())
require(MLDataR)
data("heartdisease")
hd = as.data.frame(heartdisease)
summary(hd)
for(j in 1:ncol(hd))
  if(is.character(hd[,j]) | length(unique(hd[,j]))==2)
     hd[,j] = as.factor(hd[,j])
  
summary(hd)  
 
