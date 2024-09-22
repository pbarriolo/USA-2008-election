##PROJET data1

rm(list = ls())
setwd("~/LICENCE 3/SEMESTRE 2/PROJET Obama")
data1 <- read.csv("Obama_data.csv", sep = ",", header = TRUE)

sum(is.na(data1$RCa10))# 3737
table(data1$RCa10)
data1$vote_fac <- as.factor(data1$RCa10)
summary(reg <- lm(RCa10 ~ WA02_c, data1))
data1$sexe <- as.factor(data1$WA01_c)
data1$Educ_fac <- as.factor(data1$WA03_c)
data1$Educ_fac <- relevel(data1$Educ_fac, "grade 8 or lower")
summary(reg <- lm(RCa10 ~ Educ_fac, data1))
summary(reg <- lm(RCa10 ~ sexe, data1))
reg[["coefficients"]]

table(data1$RCa10) ## valeurs variable
summary(lm(RCa10 ~ WA02_c, data1))


data1 <-
  data.frame(data1$Educ_fac, data1$sexe, data1$vote_fac) ## pour creer une table une selection de colonnes

################################################################################

rm(list = ls())
setwd("~/LICENCE 3/SEMESTRE 2/PROJET OBAMA")
data1 <- read.csv("Obama_data.csv", sep = ",", header = TRUE)


unique(data1$ABo04_c) ## regarde les valeurs de la variable

library(doBy)
library(naniar)

data1$RCa10 = recodeVar(data1$RCa10 , c("Yes", "No", "Don't know") , c('1', '0', '0'))

data1 %>% replace_with_na(replace = list(ABo04_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(ABo12_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(CA02_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(SB01_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(SB02_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(CDa05_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(CDb01_c = c("no answer", "don't know", "none of these"))) -> data1
data1 %>% replace_with_na(replace = list(RD01_c = c("no answer", "don't know"))) -> data1
data1$RD01_c = recodeVar(data1$RD01_c , c("nader") , c("other")) ## on transforme nader en other car il n'est ni républicain ni démocrate
data1 %>% replace_with_na(replace = list(CDd01_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(MA04_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(CBb01_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(MA01_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WA02_c = c(998, 999))) -> data1
data1 %>% replace_with_na(replace = list(WA03_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WA04_c = c("no answer", "don't know"))) -> data1
data1 %>% replace_with_na(replace = list(WC03_c = c("no answer", "don't know"))) -> data1


data1$ABo12_c = recodeVar(data1$ABo12_c , c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') , c('No', 'No', 'No', 'No', 'No', 'somewhat', 'somewhat', 'yes','yes', 'yes', 'yes'))
data1$SB02_c = recodeVar(data1$SB02_c , c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') , c('No', 'No', 'No', 'No', 'No', 'somewhat', 'somewhat', 'yes','yes', 'yes', 'yes'))
data1$SB01_c = recodeVar(data1$SB01_c , c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') , c('No', 'No', 'No', 'No', 'No', 'somewhat', 'somewhat', 'yes','yes', 'yes', 'yes'))
data1$ABo04_c = recodeVar(data1$ABo04_c , c('very conservative', 'very liberal'), c('somewhat conservative', 'somewhat liberal'))
data1$MA04_c = recodeVar(data1$MA04_c, c(unique(data1$MA04_c)), c("moderate","liberal","liberal","conservative","conservative",NA))

################################################################################
## regressions
##Regression Obama cons-liberal
table(data1$ABo04_c)
data1$ABo04_c <- as.factor(data1$ABo04_c)
summary(reg <- lm(RCa10 ~ ABo04_c, data1))

##Regression Obama share values
table(data1$ABo12_c)
sum(is.na(data1$ABo12_c))
data1$ABo12_c <- as.factor(data1$ABo12_c)
data1$ABo12_c =relevel(data1$ABo12_c, "No")
summary(reg <- lm(RCa10 ~ ABo12_c, data1))

##Regression Trust in blacks
table(data1$SB01_c)
sum(is.na(data1$SB01_c))
data1$SB01_c <- as.factor(data1$SB01_c)
data1$SB01_c =relevel(data1$SB01_c, "yes")
summary(reg <- lm(RCa10 ~ SB01_c, data1))
sum(is.na(data1$SB01_c))

##Regression Trust in whities
table(data1$SB02_c)
sum(is.na(data1$SB02_c))
data1$SB02_c <- as.factor(data1$SB02_c)
data1$SB02_c =relevel(data1$SB02_c, "yes")
summary(reg <- lm(RCa10 ~ SB02_c, data1))

## Regression country going in right direction
unique(data1$CA02_c)
data1$CA02_c <- as.factor(data1$CA02_c)
data1$CA02_c = relevel(data1$CA02_c, "wrong track" )
summary(reg <- lm(RCa10 ~ CA02_c, data1))
sum(is.na(data1$CA02_c))### trop de valeurs manquantes


##
##Régression de salaire
table(data1$WA04_c)
unique(data1$WA04_c)
sum(is.na(data1$WA04_c))
data1$WA04_c = recodeVar(data1$WA04_c, c(unique(data1$WA04_c)),c("li","hi","mi","mi","hi","li","mi",NA,"hi","li"))
data1$WA04_c = as.factor(data1$WA04_c)
table(data1$WA04_c)
data1$WA04_c = relevel(data1$WA04_c,"li")
summary(lm(RCa10 ~ WA04_c,data1 ))


reg <- lm(RCa10 ~ CA02_c, data1)
reg1 <- lm(RCa10 ~ CA02_c + WA04_c, data1)
summary(reg1)



## Regression taxes

table(data1$CBb01_c)
test = subset(data1, data1$CBb01_c != "none of these")
summary(lm(RCa10 ~ CBb01_c, test))
## Bizarre mais peut s'expliquer par le fait que Obama veut absolument sortir de la crise
sum(is.na(data1$CBb01_c))

##Régression de Favor negotiating
unique(data1$CDa05_c)
reg_negotiation = lm(RCa10 ~ CDa05_c, data1)
summary(reg_negotiation)
sum(is.na(data1$CDa05_c))
##Significatif Stat et Eco sauf pour neutre, mais logique car neutre => pas d'effet


##Régression de Withdraw Troops
table(data1$CDb01_c)
data1$CDb01_c = as.factor(data1$CDb01_c)
data1$CDb01_c = relevel(data1$CDb01_c, "withdraw as soon as possible" )
reg_withdraw = lm(RCa10 ~ CDb01_c, data1)
summary(reg_withdraw)
## Parfait !


## Regression bloc flex ideo
summary(reg <- lm(RCa10~ CDb01_c+ABo12_c , data1))



## Regression bloc fixe ideologie
table(data1$MA01_c)

data1$RD01_c = as.factor(data1$RD01_c)
data1$RD01_c <- relevel(data1$RD01_c, "kerry")
data1$MA04_c <- as.factor(data1$MA04_c)
data1$MA04_c <- relevel(data1$MA04_c, "liberal")

summary(reg1 <- lm(RCa10~ MA04_c + RD01_c, data1))


## Regression ideo + flex

summary(reg2 <- lm(RCa10~ CDb01_c+ABo12_c+RD01_c + MA04_c + MA01_c, data1))


## Anova
test = subset(data1,!is.na(data1$WA01_c) &!is.na(data1$WA02_c) &!is.na(data1$WA03_c) & !is.na(data1$WC03_c) & !is.na(data1$RD01_c)&!is.na(data1$MA04_c) &!is.na(data1$MA01_c) &!is.na(data1$CDb01_c) &!is.na(data1$CBb01_c) & !is.na(data1$ABo12_c)& !is.na(data1$WA04_c))
summary(reg1 <- lm(RCa10~RD01_c + MA04_c + MA01_c, test))
summary(reg2 <- lm(RCa10~ CDb01_c+ABo12_c+RD01_c + MA04_c + MA01_c, test))
anova(reg1,reg2)


############################### Recherche d'une nouvelle variable pour bloc flex

# ABo05 Obama est-il un bon leader
data1 %>% replace_with_na(replace = list(ABo05_c = c(998, 999))) -> data1
table(data1$ABo05_c)
data1$ABo05_c <- as.factor(data1$ABo05_c)
data1$ABo05_c<- relevel(data1$ABo05_c, "10")
summary(reg <- lm(RCa10 ~ ABo05_c, data1))
sum(is.na(data1$ABo05_c))##86
## ABo05 est une variable avec 86 NA , un bon R**2, des bonnes stat, NICE








# ABo06 Obama est-il trustworthy
data1 %>% replace_with_na(replace = list(ABo06_c = c(998, 999))) -> data1
table(data1$ABo06_c)
data1$ABo06_c <- as.factor(data1$ABo06_c)
data1$ABo06_c<- relevel(data1$ABo06_c, "10")
summary(reg <- lm(RCa10 ~ ABo06_c, data1))
sum(is.na(data1$ABo06_c))# 101
## Nice


# ABo07 Obama experience to be president 
data1 %>% replace_with_na(replace = list(ABo07_c = c(998, 999))) -> data1
table(data1$ABo07_c)
data1$ABo07_c <- as.factor(data1$ABo07_c)
data1$ABo07_c<- relevel(data1$ABo07_c, "10")
summary(reg <- lm(RCa10 ~ ABo07_c, data1))
sum(is.na(data1$ABo07_c))# 75
## pas si nice que ça a cause des stat

# ABo08 Obama judgement to be president 
data1 %>% replace_with_na(replace = list(ABo08_c = c(998, 999))) -> data1
table(data1$ABo08_c)
data1$ABo08_c <- as.factor(data1$ABo08_c)
data1$ABo08_c<- relevel(data1$ABo08_c, "10")
summary(reg <- lm(RCa10 ~ ABo08_c, data1))
sum(is.na(data1$ABo08_c))# 97
##NICE meilleur R2 et bonnes stats mais pas les meilleures

# ABo09 Obama ready to be president 
data1 %>% replace_with_na(replace = list(ABo09_c = c(998, 999))) -> data1
table(data1$ABo09_c)
data1$ABo09_c <- as.factor(data1$ABo09_c)
data1$ABo09_c<- relevel(data1$ABo09_c, "10")
summary(reg <- lm(RCa10 ~ ABo09_c, data1))
sum(is.na(data1$ABo09_c))# 3737
## toutes les valeurs sont manquantes CACACACACAC


# ABo10 Obama says what he believes 
data1 %>% replace_with_na(replace = list(ABo10_c = c(998, 999))) -> data1
table(data1$ABo10_c)
data1$ABo10_c <- as.factor(data1$ABo10_c)
data1$ABo10_c<- relevel(data1$ABo10_c, "10")
summary(reg <- lm(RCa10 ~ ABo10_c, data1))
sum(is.na(data1$ABo10_c))# 75
## toutes valeurs NA CACA

# ABo11 Obama patriotic
data1 %>% replace_with_na(replace = list(ABo11_c = c(998, 999))) -> data1
table(data1$ABo11_c)
data1$ABo11_c <- as.factor(data1$ABo11_c)
data1$ABo11_c<- relevel(data1$ABo11_c, "10")
summary(reg <- lm(RCa10 ~ ABo11_c, data1))
sum(is.na(data1$ABo11_c))# 100
## Nice masi R2 pas tres haut, et meilleures stats que le reste

cor(data1$ABo05_c,data1$ABo11_c, use = "complete.obs")
cor(data1$ABo05_c,data1$ABo08_c, use = "complete.obs")
cor(data1$ABo11_c,data1$ABo08_c, use = "complete.obs")




## Régression de Educ
data1$WA03_c = recodeVar(
  data1$WA03_c,
  c(unique(data1$WA03_c)),
  c(
    "Some education",
    "Lot of education",
    "Some education",
    "Lot of education",
    "No education",
    "No education",
    "Some education",
    "Lot of education",
    "Some education",
    NA
  )
)

data1$WA03_c <- as.factor(data1$WA03_c)
data1$WA03_c <- relevel(data1$WA03_c, "No education")


unique(data1$WA03_c)
reg_educ = lm(data1$RCa10 ~ data1$WA03_c)
summary(reg_educ)
## OKKKKKK



## Régression de Ethnie
data1$WC03_c = as.factor(data1$WC03_c)
data1$WC03_c <- relevel(data1$WC03_c, "black, african american, or black hispanic")

data1$WC03_c = recodeVar(
  data1$WC03_c,
  c(unique(data1$WC03_c)),
  c("black", "white", "other", "other", "other", "other", "other", NA)
)

table(data1$WC03_c)
unique(data1$WC03_c)
data1$WC03_c = as.factor(data1$WC03_c)
reg_race = lm(RCa10 ~ WC03_c, data1)
summary(reg_race)
## Parfaitement significatif

# test = subset(Obama,WC03_c != "no answer" & WC03_c != "don't know" )
# #test$WC03_c = as.factor(test_res$WC03_c)
# test$WC03_c = recodeVar(test$WC03_c, c(unique(test$WC03_c)),c("0","1","0","0","0","0","0"))
# unique(test$WC03_c)
# reg_test = lm(test$RCa10 ~ test$WC03_c )
# summary(reg_test)

## Pas significatif pour les autres modalités que W/B
