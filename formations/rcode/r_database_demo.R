## dossier de travail   
## on va indiquer le dossier de travail 
setwd("~/R_demo")
#ls()  
#rm(list=ls())
#?rm  

## lecture    
database = read.csv2("framingham_dataset.csv")
## pour lire un fichier .xlsx 
#require(readxl)
#database = read_excel("heart_original.xlsx")


## aide pour le jeu de donnees
?riskCommunicator::framingham


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
database[11627,]
database[c(1:3, 20:22), ]

database[,4]
database[,2:4]
length(database[,4])
length(database$AGE)
database$BMI


## magique !!!
attach(database)
database$BMI
BMI


## donnees manquantes 
is.na(BMI)
!is.na(BMI)  #la negation logique 
which(is.na(BMI))
which(is.na(TOTCHOL))

database[which(is.na(BMI)),] #une petite extraction 

## extraction des donnees completes 
# on en a combien
length(which(complete.cases(database)))

# on va les extraire 
database_complete = database[which(complete.cases(database)), ]
head(database_complete)
dim(database_complete)

database_na = database[which(!complete.cases(database)), ]
head(database_na)
dim(database_na)

## bonne nouvelle : il y a plus simple 
database_omit_na = na.omit(database)

## un petit summary de rappel 
summary(database)

## on va creer une variable categorielle  
database$SEX = as.factor(database$SEX)
summary(database)

# si on a besoin de revenir au format numeric 
database$SEX = as.numeric(database$SEX)
summary(database)





## tri et ordre 


## fusion de lignes : rbind 

database_r = rbind.data.frame(database_complete, database_na)
tail(database_r)


## fusion en colonnes : cbind 
colnames(database)
database_1 = database[,c("SEX", "TOTCHOL")]
database_2 = database[,-c(2,3)]
database_c = cbind.data.frame(database_1, database_2)
head(database_c)


## calcul sur chaque colonne
apply(database, 2, sd)
apply(na.omit(database), 2, sd)


summary(database)


## graphiques 
hist(database$AGE, main ="Histogramme de l'AGE", xlab="age", ylab="comptage")


# creation d'une variable et la fonction ifelse
database$GENDER = ifelse(SEX==1, "homme", "femme")

# verification des comptages 
table(database$GENDER)
prop.table(table(database$GENDER))

barplot(table(database$GENDER), col="darkred", main="gender", 
        names.arg = c("homme", "femme"))

barplot(prop.table(table(database$GENDER)), col="darkred", main="gender", 
        names.arg = c("homme", "femme"))

boxplot(database$BMI~database$HYPERTEN, 
        ylab="BMI",
        xlab="Hypertension")



## DIABETES 
my_table = table(HOSPMI, DIABETES)
my_table

# si on veut enregistrer 
#save(my_table, database, file="ma_table.rda")
#load("base_paris.rda")

barplot(prop.table(my_table, 2),
        main = "HOSPMI by DIABETES",
        xlab = "DIABETES", ylab = "Freq",
        col = c("darkblue", "red"),
        legend.text = rownames(my_table),
        beside = TRUE)# Grouped bars


## tester la différence de proportions 
prop.table(my_table, 2)
my_test = chisq.test(DIABETES, HOSPMI, correct = FALSE)
print(my_test)


## force d'association 
require(Epi)
twoby2(1-DIABETES, 1-HOSPMI)

## regression logistique 
m0 =  glm(HOSPMI~DIABETES, 
          data = database, 
          family = "binomial")
summary(m0)
exp(coefficients(m0))
exp(confint(m0))
m1 =  glm(HOSPMI~AGE+GENDER+BMI+CURSMOKE+CIGPDAY+TOTCHOL+DIABETES, 
          data = database, 
          family = "binomial")
summary(m1)
exp(coefficients(m1))
exp(confint(m1))

## sour R on peut tout stocker 
save(m0, m1, file="my_models.rda")
load("my_models.rda")

