rm(list = ls())
setwd("~/LICENCE 3/SEMESTRE 2/PROJET Obama")
data1 <- read.csv("Obama_data.csv", sep = ",", header = TRUE)

### projet data1 code final à rendre
library(doBy)
library(naniar)
library(car)
library(AICcmodavg)
############################################################ Choix des variables

#########################################variable dépendante

data1$RCa10 = recodeVar(data1$RCa10 , c("Yes", "No", "Don't know") , c('1', '0', '0')) # recodage en variable indicatrice 1(femme)
sum(is.na(data1$RCa10)) ## = 842 valeurs manquantes

#########################################variables indépendantes
#recodage des réponses manquantes ou non pertinentes de notre enquête en valeurs manquantes

data1 %>% replace_with_na(replace = list(ABo05_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(ABo12_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(CDb01_c = c("no answer", "don't know", "none of these"))) -> data1
data1 %>% replace_with_na(replace = list(RD01_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(MA04_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(MA01_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WA02_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(WA03_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WA04_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WC03_c = c("no answer", "don't know"))) -> data1

#variable par variable(recodage, factor, valeurs manquantes, regression lineaire simple)
########################Bloc socio-démographique
## Sex
sum(is.na(data1$WA01_c))##=0
data1$WA01_c <- as.factor(data1$WA01_c)
summary(reg_femme <- lm(data1$RCa10 ~ data1$WA01_c))
## Age
sum(is.na(data1$WA02_c))##=24
summary(reg_age <- lm(data1$RCa10 ~ data1$WA02_c))
## Education
sum(is.na(data1$WA03_c))##=4
data1$WA03_c = recodeVar(data1$WA03_c,c(unique(data1$WA03_c)),c("Some education","Lot of education","Some education","Lot of education","No education","No education","Some education","Lot of education","Some education",NA))
data1$WA03_c <- as.factor(data1$WA03_c)





data1$WA03_c <- relevel(data1$WA03_c, "No education")
reg_educ = lm(data1$RCa10 ~ data1$WA03_c)
summary(reg_educ)
## Salary
sum(is.na(data1$WA04_c)) ##=1662
data1$WA04_c = recodeVar(data1$WA04_c, c(unique(data1$WA04_c)),c("li","hi","mi","mi","hi","li","mi",NA,"hi","li"))
data1$WA04_c = as.factor(data1$WA04_c)
data1$WA04_c = relevel(data1$WA04_c,"li")
summary(lm(RCa10 ~ WA04_c,data1 ))
## Race
sum(is.na(data1$WC03_c)) ##=43
data1$WC03_c = recodeVar(data1$WC03_c,  c(unique(data1$WC03_c)),  c("black", "white", "other", "other", "other", "other", "other", NA))
data1$WC03_c = as.factor(data1$WC03_c)
data1$WC03_c <- relevel(data1$WC03_c, "black")
summary(reg_race <- lm(RCa10 ~ WC03_c, data1))

#############################Bloc Idélogique
## Last vote
sum(is.na(data1$RD01_c))##=74
data1$RD01_c = recodeVar(data1$RD01_c , c("nader") , c("other")) ## on transforme nader en other car il n'est ni républicain ni démocrate
data1$RD01_c = as.factor(data1$RD01_c)
data1$RD01_c = relevel(data1$RD01_c, "kerry")
summary(reg_lv<-lm(RCa10 ~ RD01_c , data1))
## Conservative-Liberal
sum(is.na(data1$MA04_c))##=86
data1$MA04_c = recodeVar(data1$MA04_c, c(unique(data1$MA04_c)), c("moderate","liberal","liberal","conservative","conservative",NA))
data1$MA04_c <- as.factor(data1$MA04_c)
data1$MA04_c <- relevel(data1$MA04_c, "liberal")
summary(reg_cl <- lm(RCa10 ~ MA04_c, data1))
## Id-Party
sum(is.na(data1$MA01_c))##=116
unique(data1$MA01_c)
summary(reg_id <- lm(RCa10 ~ MA01_c, data1))

################################Bloc Opinion
## data1 share values
sum(is.na(data1$ABo12_c)) ##=99
data1$ABo12_c = recodeVar(data1$ABo12_c , c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') , c('No', 'No', 'No', 'No', 'No', 'somewhat', 'somewhat', 'yes','yes', 'yes', 'yes'))
data1$ABo12_c <- as.factor(data1$ABo12_c)
data1$ABo12_c =relevel(data1$ABo12_c, "yes")
summary(reg <- lm(RCa10 ~ ABo12_c, data1))
## Withdraw Troops From Iraq
sum(is.na(data1$CDb01_c)) ##=132
data1$CDb01_c = as.factor(data1$CDb01_c)
data1$CDb01_c = relevel(data1$CDb01_c, "withdraw as soon as possible" )
summary(reg_withdraw<-lm(RCa10 ~ CDb01_c, data1))
## data1 leader
sum(is.na(data1$ABo05_c))##=86
#data1$ABo05_c <- as.factor(data1$ABo05_c)
#data1$ABo05_c<- relevel(data1$ABo05_c, "10")
summary(reg <- lm(RCa10 ~ ABo05_c, data1))

####################################################Regression Multiple Linéaire
######################################Modèle
summary(reg123<- lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,data1))

#####################Verification Hypothèses
### Vérifions que notre échantillon est aléatoire, i.e. repésentatif et qu'il est pas biaisé
data1_data <- read.csv("Obama_data.csv", sep = ",", header = TRUE)
data1_data$RCa10_na[is.na(data1_data$RCa10)] = 1 #on construit une variable indicatrice qui donne 1 si RCa10 est manquante est 0 sinon
data1_data$RCa10_na[!is.na(data1_data$RCa10)] = 0
regNA = lm(RCa10_na ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,data1_data )
summary(regNA)
### Histogramme de la distribution des residus
x= reg123$residuals
x = (x-mean(x))/sqrt(var(x))
hist(x,freq = FALSE,ylim=c(0,1) )
lines(density(x),col ='red')
curve(dnorm(x, mean=0, sd=1), from = -4, to = 4,add=TRUE)
### Test de normalite des residus
shapiro.test((reg123$residuals))
### Multicolinéarité des variables
vif(reg123)

######################Possible biais de sélection à cause de la variable salaire
## On compare le même modèle sur deux échantillons différents
## Regression avec individus dont salaire valeur manquante mais sans la variable Salary
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,data1)
summary(reg123)
confint(reg123) # intervalles de confiance pour chaque paramètre
test = subset(  data1,  !is.na(data1$WA04_c))
## Regression sans individus dont salaire valeur manquante mais sans variable Salary
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,test)
summary(reg123)

####################################################Regression Multiple Linéaire par Blocs
# On commence par enlever toutes les valeurs manquantes de toutes les variables utilisées pour que toutes nos régressions soient faites sur le mème échantillon et que l'on puisse les comparer grâce à anova
test = subset(
  data1,
  !is.na(data1$WA01_c) &
    !is.na(data1$WA02_c) &
    !is.na(data1$WA03_c) &
    !is.na(data1$WC03_c) & !is.na(data1$RD01_c)
  &
    !is.na(data1$MA04_c) &
    !is.na(data1$MA01_c) &
    !is.na(data1$CDb01_c) &
    #!is.na(data1$CBb01_c) & 
    !is.na(data1$ABo12_c)
  & !is.na(data1$WA04_c) & !is.na(data1$RCa10) &!is.na(data1$ABo05_c))

## Bloc 1 (Socio-démographiques) : WA01_c + WA02_c + WA03_c + WA04_c + WC03_c
## Bloc 2 (Fixe) : RD01_c + MA04_c + MA01_c
## Bloc 3 (Fluide) : CDb01_c +ABo5 c  ABo12_c

reg1 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c, test)
reg2 = lm(RCa10 ~ RD01_c + MA04_c + MA01_c, test)
reg3 = lm(RCa10 ~ CDb01_c + ABo12_c + ABo05_c , test)
reg12 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c    + RD01_c + MA04_c + MA01_c, test)
reg13 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c    + CDb01_c + ABo12_c + ABo05_c, test)
reg23 = lm(RCa10 ~ RD01_c + MA04_c + MA01_c    + CDb01_c + ABo12_c + ABo05_c, test)
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,test )
summary(reg123) 
anova(reg123, reg23, reg13, reg12, reg3, reg2, reg1)
aictab(list(reg1, reg2, reg3, reg12, reg13, reg23, reg123),c("1", "2", "3", "1&2", "1&3", "2&3", "1&2&3"))

