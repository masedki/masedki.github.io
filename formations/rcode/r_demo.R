## dossier de travail   
## on va indiquer le dossier de travail 
setwd("~/R_demo")
#ls()  
#rm(list=ls())
#?rm  

## lecture    
database = read.csv2("heart_disease.csv")
## pour lire un fichier .xlsx 
#require(readxl)
#database = read_excel("heart_original.xlsx")


summary(database)
str(database)



##  
dim(database)
##
colnames(database)
##  
head(database)
tail(database)

## acces 
database[1,]
database[303,]
database[,1]
length(database[,1])
length(database$age)
database$thal


## magique !!!
attach(database)
database$thal
thal 
is.na(thal)

which(is.na(thal))

which(is.na(num_vessels_flour))

database[which(is.na(thal)),]





## binariser pour obtenir une vraie variable categorielle
database$gender = ifelse(database$gender=="male", 1, 0)
database$has_heart_disease = ifelse(database$has_heart_disease=="yes", 1, 0)
summary(database)

database$gender = as.factor(database$gender)
database$num_vessels_flour = as.factor(database$num_vessels_flour)
#database$has_heart_disease = as.factor(database$has_heart_disease)
#database$exer_angina = as.factor(database$exer_angina)
summary(database)





## tri et ordre 


## missing data 
complete.cases(database)
!complete.cases(database)

which(!complete.cases(database))
database_complete = database[which(complete.cases(database)),]
database_na = database[which(!complete.cases(database)),]

head(database_complete)
head(database_na)
## fusion en rbind 

database = rbind.data.frame(database_complete, database_na)

tail(database)
rownames(database)

## fusion en cbind 

database_1 = database[,c("age", "gender")]
database_2 = database[,-c(1,2)]
database = cbind.data.frame(database_1, database_2)



summary(database)

## calcul sur chaque colonne

apply(database, 2, sd)

apply(na.omit(database), 2, sd)

## format factor !!
#database$gender = as.factor(database$gender)
#database$has_heart_disease = as.factor(database$has_heart_disease)

summary(database)


## tri par sort et order 

## graphiques 
hist(database$age, main ="le", xlab="age", ylab="freq")


barplot(table(database$gender), col="darkred", main="gender", 
        names.arg = c("female", "male"))

boxplot(database$max_heart_rate~database$has_heart_disease, 
        ylab="max heart rate",
        xlab="heart disease")



## exer_angina
my_table = table(exer_angina, has_heart_disease)
my_table
save(my_table, database, file="Montpellier.rda")
load("Montpellier.rda")

barplot(prop.table(my_table, 1),
        main = "heart disease by exer_angina",
        xlab = "exer_angina", ylab = "Freq",
        col = c("darkblue", "red"),
        legend.text = rownames(my_table),
        beside = TRUE)# Grouped bars

barplot(prop.table(my_table, 2),
        main = "exer_angina by heart disease",
        xlab = "heart disease", ylab = "Freq",
        col = c("darkblue", "red"),
        legend.text = rownames(my_table),
        beside = TRUE)# Grouped bars

## tester la différence de proportions 
prop.table(my_table, 1)
my_test = chisq.test(exer_angina, has_heart_disease, correct = FALSE)
print(my_test)


## force d'association 
require(Epi)

twoby2(1-exer_angina, 1-has_heart_disease)


## regression logistique 
database_complete_1 = database_complete[,-which(colnames(database_complete)=="heart_disease_severity")]
m0 =  glm(has_heart_disease~age+gender+resting_blood_pressure+max_heart_rate+slope+fasting_blood_sugar+exer_angina, 
           data = database_complete, 
           family = "binomial")
summary(m0)


m1 =  glm(has_heart_disease~., 
          data = database_complete_1, 
          family = "binomial")
summary(m1)

## sour R on peut tout stocker 
save(m0, m1, file="my_models.rda")
load("my_models.rda")

