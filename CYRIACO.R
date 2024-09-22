rm(list = ls())
setwd("~/LICENCE 3/SEMESTRE 2/PROJET Obama")
library(ggplot2)
library(doBy)
library(psych)
library(naniar)
library(questionr)
#install.packages("AICcmodavg")
library(AICcmodavg)
library(car)
## Mise en place des données
Obama_data <- read.csv("Obama_data.csv", sep = ",", header = TRUE)
Obama <- Obama_data

Obama %>% replace_with_na(replace = list(ABo04_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(ABo12_c = c(998, 999))) -> Obama
Obama %>% replace_with_na(replace = list(CA02_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(SB01_c = c(998, 999))) -> Obama
Obama %>% replace_with_na(replace = list(SB02_c = c(998, 999))) -> Obama
Obama %>% replace_with_na(replace = list(CDa05_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(CDb01_c = c("no answer", "don't know", "none of these"))) -> Obama
Obama %>% replace_with_na(replace = list(RD01_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(CDd01_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(MA04_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(CBb01_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(MA01_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(WA02_c = c(998, 999))) -> Obama
Obama %>% replace_with_na(replace = list(WA03_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(WA04_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(WC03_c = c("no answer", "don't know"))) -> Obama
Obama %>% replace_with_na(replace = list(ABo05_c = c(998, 999))) -> Obama

unique(Obama$RCa10)

Obama$RD01_c = recodeVar(Obama$RD01_c , c("nader") , c("other")) ## on transforme nader en other car il n'est ni républicain ni démocrate
Obama$RCa10 = recodeVar(Obama$RCa10 , c("Yes", "No", "Don't know") , c(1, 0, 0))
Obama$CA02_c = recodeVar(
  Obama$CA02_c,
  c("right direction", "wrong track", "don't know", "no answer"),
  c("1", "0", "0", "0")
)
#Obama$WA01_c = recodeVar(Obama$WA01_c , c("Female", "Male") , c('1','0'))
Obama$RCa10 = as.integer(Obama$RCa10)
Obama$WC03_c = recodeVar(
  Obama$WC03_c,
  c(unique(Obama$WC03_c)),
  c("black", "white", "other", "other", "other", "other", "other", NA)
)
Obama$CDa05_c = recodeVar(Obama$CDa05_c,
                          c(unique(Obama$CDa05_c)),
                          c("oppose", "neutral", "favor", "oppose", "favor", NA))
Obama$CDd01_c = recodeVar(Obama$CDd01_c,
                          c(unique(Obama$CDd01_c)),
                          c("favor", "oppose", "oppose", "favor", NA))
Obama$MA04_c = recodeVar(
  Obama$MA04_c,
  c(unique(Obama$MA04_c)),
  c(
    "moderate",
    "liberal",
    "liberal",
    "conservative",
    "conservative",
    NA
  )
)
unique(Obama$WA03_c)
Obama$WA03_c = recodeVar(
  Obama$WA03_c,
  c(unique(Obama$WA03_c)),
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

Obama$ABo12_c = recodeVar(
  Obama$ABo12_c ,
  c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') ,
  c(
    'No',
    'No',
    'No',
    'No',
    'No',
    'somewhat',
    'somewhat',
    'yes',
    'yes',
    'yes',
    'yes'
  )
)
Obama$SB02_c = recodeVar(
  Obama$SB02_c ,
  c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') ,
  c(
    'No',
    'No',
    'No',
    'No',
    'No',
    'somewhat',
    'somewhat',
    'yes',
    'yes',
    'yes',
    'yes'
  )
)
Obama$SB01_c = recodeVar(
  Obama$SB01_c ,
  c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10') ,
  c(
    'No',
    'No',
    'No',
    'No',
    'No',
    'somewhat',
    'somewhat',
    'yes',
    'yes',
    'yes',
    'yes'
  )
)
Obama$ABo04_c = recodeVar(
  Obama$ABo04_c ,
  c('very conservative', 'very liberal'),
  c('somewhat conservative', 'somewhat liberal')
)
Obama$WA04_c = recodeVar(Obama$WA04_c,
                         c(unique(Obama$WA04_c)),
                         c("li", "hi", "mi", "mi", "hi", "li", "mi", NA, "hi", "li"))



## Factor-isation
Obama$RD01_c = as.factor(Obama$RD01_c)
Obama$RD01_c = relevel(Obama$RD01_c, "kerry")
Obama$CDb01_c <- as.factor(Obama$CDb01_c)
Obama$CDb01_c <-
  relevel(Obama$CDb01_c, "withdraw as soon as possible")
Obama$CDd01_c <- as.factor(Obama$CDd01_c)
Obama$CDd01_c <- relevel(Obama$CDd01_c, "favor")
Obama$MA04_c <- as.factor(Obama$MA04_c)
Obama$MA04_c <- relevel(Obama$MA04_c, "liberal")
Obama$WA03_c <- as.factor(Obama$WA03_c)
Obama$WA03_c <- relevel(Obama$WA03_c, "No education")
Obama$ABo04_c <- as.factor(Obama$ABo04_c)
Obama$ABo12_c <- as.factor(Obama$ABo12_c)
Obama$ABo12_c = relevel(Obama$ABo12_c, "yes")
Obama$SB01_c <- as.factor(Obama$SB01_c)
Obama$ABo12_c <- as.factor(Obama$ABo12_c)
Obama$CA02_c <- as.factor(Obama$CA02_c)
Obama$CA02_c = relevel(Obama$CA02_c, "wrong track")
Obama$SB02_c <- as.factor(Obama$SB02_c)
Obama$SB02_c = relevel(Obama$SB02_c, "yes")
Obama$WA04_c = as.factor(Obama$WA04_c)
Obama$WA04_c = relevel(Obama$WA04_c,"li")
#Obama$ABo05_c <- as.factor(Obama$ABo05_c)
#Obama$ABo05_c<- relevel(Obama$ABo05_c, "10")
################################################################################


## Régression de Age
summary(reg_age <- lm(Obama$RCa10 ~ Obama$WA02_c))
## Pas siginificatif écomiquement et statistiquement


## Régression de Educ
unique(Obama$WA03_c)
reg_educ = lm(Obama$RCa10 ~ Obama$WA03_c)
summary(reg_educ)
## OKKKKKK


## Régression de Sexe
reg_femme <- lm(Obama$RCa10 ~ Obama$WA01_c)
summary(reg_femme)
## Statistiquement significatif et Economiquement non


## Régression de Ethnie
summary(Obama$WC03_c)
hist(Obama$WC03_c)
table(Obama$WC03_c)
unique(Obama$WC03_c)

Obama$WC03_c = as.factor(Obama$WC03_c)
reg_race = lm(RCa10 ~ WC03_c, Obama)
summary(reg_race)
## Parfaitement significatif

# test = subset(Obama,WC03_c != "no answer" & WC03_c != "don't know" )
# #test$WC03_c = as.factor(test_res$WC03_c)
# test$WC03_c = recodeVar(test$WC03_c, c(unique(test$WC03_c)),c("0","1","0","0","0","0","0"))
# unique(test$WC03_c)
# reg_test = lm(test$RCa10 ~ test$WC03_c )
# summary(reg_test)

## Pas significatif pour les autres modalités que W/B


## Régression de party id
unique(Obama$MA01_c)
reg_id = lm(RCa10 ~ MA01_c, Obama)
summary(reg_id)
## Très significatif


##Régression de Last vote
unique(Obama$RD01_c)
reg_lastvote = lm(RCa10 ~ RD01_c , Obama)
summary(reg_lastvote)
## Très significatif


##Régression de Favor negotiating
unique(Obama$CDa05_c)
reg_negotiation = lm(RCa10 ~ CDa05_c, Obama)
summary(reg_negotiation)
##Significatif Stat et Eco sauf pour neutre, mais logique car neutre => pas d'effet


##Régression de Withdraw Troops
table(Obama$CDb01_c)
reg_withdraw = lm(RCa10 ~ CDb01_c, Obama)
summary(reg_withdraw)
## Parfait !


##Régression de Citizenship
table(Obama$CDd01_c)
summary(lm(RCa10 ~ CDd01_c, Obama))
## Résultat pas significatif (pas d'impact ?)


##Régression de people cons lib
table(Obama$MA04_c)
reg_ppl_cons_lib = lm(RCa10 ~ MA04_c, Obama)
summary(reg_ppl_cons_lib)
## Parfait et logique


##Régression de taxe
table(Obama$CBb01_c)
test = subset(Obama, Obama$CBb01_c != "none of these")
summary(lm(RCa10 ~ CBb01_c, test))
## Bizarre mais peut s'expliquer par le fait que Obama veut absolument sortir de la crise


##Régression de salaire
table(Obama$WA04_c)
summary(lm(RCa10 ~ WA04_c, Obama))
sum(is.na(Obama$WA04_c))
## Pas fou

## regression avec valeurs manquantes salaire
# gros echantillon
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,Obama)
summary(reg123)
confint(reg123)
test = subset(
  Obama,
  !is.na(Obama$WA04_c)
)
##regression sans valeurs manquantes salaire
#echantillon petit 
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,test)
summary(reg123)
reg123$fitted.values[1]

## regression salaire NA
Obama_data$WA04_c_na[is.na(Obama_data$WA04_c)] = "Valeur manquante"
Obama_data$WA04_c_na[!is.na(Obama_data$WA04_c)] = "Valeur non manquante"

Obama_data$RCa10 = recodeVar(Obama_data$RCa10 , c("Yes", "No", "Don't know") , c(1, 0, 0))

reg_salary = lm(RCa10 ~ WA04_c_na ,Obama_data)
summary(reg_salary)



##Regression par bloc !
table(Obama$CDa05_c)

test = subset(
  Obama,
  !is.na(Obama$WA01_c) &
    !is.na(Obama$WA02_c) &
    !is.na(Obama$WA03_c) &
    !is.na(Obama$WC03_c) & !is.na(Obama$RD01_c)
  &
    !is.na(Obama$MA04_c) &
    !is.na(Obama$MA01_c) &
    !is.na(Obama$CDb01_c) &
    #!is.na(Obama$CBb01_c) & 
    !is.na(Obama$ABo12_c)
  & !is.na(Obama$WA04_c) & !is.na(Obama$RCa10) &!is.na(Obama$ABo05_c)
)


summary(lm(RCa10 ~ ABo05_c,test))


## Bloc 1 (Socio-démographiques) : WA01_c + WA02_c + WA03_c + WA04_c + WC03_c
## Bloc 2 (Fixe) : RD01_c + MA04_c + MA01_c
## Bloc 3 (Fluide) : CDb01_c + CDa05_c + CBb01_c + SB02_c + CA02_c + ABo12_c


# reg1 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c +  WC03_c, test)
# reg1 = glm(RCa10 ~ WA01_c + WA02_c + WA03_c +  WC03_c,data = test,family = binomial(link="logit"))
reg1 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c, test)
reg2 = lm(RCa10 ~ RD01_c + MA04_c + MA01_c, test)
reg3 = lm(RCa10 ~ CDb01_c + ABo12_c + ABo05_c , test)
reg12 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c    + RD01_c + MA04_c + MA01_c, test)
reg13 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c    + CDb01_c + ABo12_c + ABo05_c, test)
reg23 = lm(RCa10 ~ RD01_c + MA04_c + MA01_c    + CDb01_c + ABo12_c + ABo05_c, test)
reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,test )
summary(reg123)
anova(reg123, reg12,reg1)
anova(reg123,reg12)
anova(reg1, reg2, reg3, reg12, reg13, reg23, reg123)


a = anova(reg123, reg23, reg13, reg12, reg2, reg3, reg1)
a = aictab(list(reg1, reg2, reg3, reg12, reg13, reg23, reg123),
       c("1", "2", "3", "1&2", "1&3", "2&3", "1&2&3"))
vif(reg3)

a

leveneTest(reg1$residuals ~ interaction(WA01_c, WA02_c, WA03_c, WC03_c),
           data = Obama)

# reg_test_bloc = lm(RCa10 ~ MA01_c + WC03_c + RD01_c , Obama)
# summary(reg_test_bloc)
# A <- subset(Obama, select = c(RD01_c , MA01_c))
# AIC(reg_test_bloc) ## = 1694
# AIC(lm(RCa10 ~ MA01_c + RD01_c , Obama)) ## Sans Ethnic = 1737
# AIC(lm(RCa10 ~ MA01_c + WC03_c , Obama)) ## Sans last vote = 2409
# AIC(lm(RCa10 ~ WC03_c + RD01_c , Obama)) ## Sans party id = 2082

# reg1 = lm(RCa10 ~ MA01_c, Obama)
# reg2 = lm(RCa10 ~ RD01_c + MA01_c, Obama)
# reg3 = lm(RCa10 ~ RD01_c + MA01_c + WC03_c ,Obama)
# reg4 = lm(RCa10 ~ RD01_c + MA01_c + WC03_c + WA01_c,Obama)
# vif(reg4)
# final_res = data.frame(R2 = c(0,0,0,0,0,0,0),AIC = c(0,0,0,0,0,0,0))
# final_res[1,1] = summary(reg1)$r.squared
# final_res[1,2] = AIC(reg1)
# final_res[2,1] = summary(reg2)$r.squared
# final_res[2,2] = AIC(reg2)
# final_res[3,1] = summary(reg3)$r.squared
# final_res[3,2] = AIC(reg3)
# final_res[4,1] = summary(reg12)$r.squared
# final_res[4,2] = AIC(reg12)
# final_res[5,1] = summary(reg13)$r.squared
# final_res[5,2] = AIC(reg13)
# final_res[6,1] = summary(reg23)$r.squared
# final_res[6,2] = AIC(reg23)
# final_res[6,1] = summary(reg123)$r.squared
# final_res[6,2] = AIC(reg123)
# etiquettes = c("1","2","3","1&2","1&3","2&3","1&2&3")


Obama_data$RCa10_na[is.na(Obama_data$RCa10)] = 1
Obama_data$RCa10_na[!is.na(Obama_data$RCa10)] = 0
regNA = lm(RCa10_na ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,Obama_data )
summary(regNA)

Obama$RCa10_na[is.na(Obama$RCa10)] = 1
Obama$RCa10_na[!is.na(Obama$RCa10)] = 0
regNA1 = lm(RCa10_na ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,Obama )
summary(regNA1)

hist(reg123$residuals)

reg123 = lm(RCa10 ~ WA01_c + WA02_c + WA03_c + WA04_c + WC03_c   + RD01_c + MA04_c + MA01_c   + CDb01_c + ABo12_c + ABo05_c,test )

summary(reg123)
windows()
plot(reg123)

