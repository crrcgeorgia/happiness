###The Easterlin Paradox and Happiness U-curve in Georgia

library(haven)
library(stringr)
library(tidyverse)
library(ggeffects)
library(survey)
library(MASS)
library(effects)
library(descr)

nothap <- read_dta("UN_Women_Geo_2018_14.05.18.dta")

###Setting up the survey data
UNWsvy_Sep5 <- svydesign(id=~psu, strata=~stratum, 
                         weights=~indwt,  
                         data=nothap)

####Economic data
##Unemployment
UNWsvy_Sep5$variables$q12_1_r<-UNWsvy_Sep5$variables$q12_1
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==1]<-100
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==2]<-200
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==3]<-300
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==4]<-400
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==5]<-500
UNWsvy_Sep5$variables$q12_1_r[UNWsvy_Sep5$variables$q12_1_r==6]<-600
table(UNWsvy_Sep5$variables$q12_1_r)

UNWsvy_Sep5$variables$primarystatus<-ifelse(UNWsvy_Sep5$variables$q12_1==-7, UNWsvy_Sep5$variables$q13_1,UNWsvy_Sep5$variables$q12_1_r)
table(UNWsvy_Sep5$variables$primarystatus)
UNWsvy_Sep5$variables$primarystatus_r<-UNWsvy_Sep5$variables$primarystatus
UNWsvy_Sep5$variables$primarystatus_r[UNWsvy_Sep5$variables$primarystatus_r<=-1]<-NA
table(UNWsvy_Sep5$variables$primarystatus_r)

#Household working status
table(UNWsvy_Sep5$variables$q1)
table(UNWsvy_Sep5$variables$q8_1)
UNWsvy_Sep5$variables$q8_1_r<-UNWsvy_Sep5$variables$q8_1
UNWsvy_Sep5$variables$q8_1_r[UNWsvy_Sep5$variables$q8_1_r!=1]<-0
table(UNWsvy_Sep5$variables$q8_1_r)

UNWsvy_Sep5$variables$householdworkertwo<-(UNWsvy_Sep5$variables$q8_1_r+UNWsvy_Sep5$variables$q1)
UNWsvy_Sep5$variables$householdworkertwo[UNWsvy_Sep5$variables$householdworkertwo!=2]<-0
UNWsvy_Sep5$variables$householdworkertwo[UNWsvy_Sep5$variables$householdworkertwo==2]<-1
table(UNWsvy_Sep5$variables$householdworkertwo)

UNWsvy_Sep5$variables$primarystatus<-ifelse(UNWsvy_Sep5$variables$q12_1==-7, UNWsvy_Sep5$variables$q13_1,UNWsvy_Sep5$variables$q12_1_r)
table(UNWsvy_Sep5$variables$primarystatus)
UNWsvy_Sep5$variables$primarystatus_r<-UNWsvy_Sep5$variables$primarystatus
UNWsvy_Sep5$variables$primarystatus_r[UNWsvy_Sep5$variables$primarystatus_r<=-1]<-NA
table(UNWsvy_Sep5$variables$primarystatus_r)

UNWsvy_Sep5$variables$primarystatus_r<-ifelse(UNWsvy_Sep5$variables$householdworkertwo==1,1000,UNWsvy_Sep5$variables$primarystatus)
table(UNWsvy_Sep5$variables$primarystatus_r)
UNWsvy_Sep5$variables$primarystatus_r_r <- factor(UNWsvy_Sep5$variables$primarystatus_r,
                                                  levels = c(-7,-3,1,2,3,4,5,100,200,300,400,500,600,1000),
                                                  labels = c("notapplicable", 
                                                             "interviewer error", 
                                                             "Employee with contract",
                                                             "Employee without a contract",
                                                             "Self-employed formal",
                                                             "Self-employed informal",
                                                             "Other Employed",
                                                             "Student not working",
                                                             "Homemaker and not working",
                                                             "Retired and not working",
                                                             "Disabled and unable to work",
                                                             "Unemployed",
                                                             "Other Unemployed",
                                                             "Contributing Household Worker"))
table(UNWsvy_Sep5$variables$primarystatus_r_r)
UNWsvy_Sep5$variables$primarystatus_r_r[UNWsvy_Sep5$variables$primarystatus_r_r=="notapplicable"]<-NA
UNWsvy_Sep5$variables$primarystatus_r_r[UNWsvy_Sep5$variables$primarystatus_r_r=="interviewer error"]<-NA


#Wants a job
UNWsvy_Sep5$variables$q9_1_r<-UNWsvy_Sep5$variables$q9_1
UNWsvy_Sep5$variables$q9_1_r[UNWsvy_Sep5$variables$q9_1_r<=-1]<-0
table(UNWsvy_Sep5$variables$q9_1_r)

#Sought job
table(UNWsvy_Sep5$variables$q10_1)
UNWsvy_Sep5$variables$q10_1_r<-UNWsvy_Sep5$variables$q10_1
UNWsvy_Sep5$variables$q10_1_r[UNWsvy_Sep5$variables$q10_1_r<=-1]<-0
table(UNWsvy_Sep5$variables$q10_1_r)

#Can start working
table(UNWsvy_Sep5$variables$q11_1)
UNWsvy_Sep5$variables$q11_1_r<-UNWsvy_Sep5$variables$q11_1
UNWsvy_Sep5$variables$q11_1_r[UNWsvy_Sep5$variables$q11_1_r<=-1]<-0
table(UNWsvy_Sep5$variables$q11_1_r)


#Unemployment calculation
UNWsvy_Sep5$variables$seekingwork<-(UNWsvy_Sep5$variables$q11_1_r+UNWsvy_Sep5$variables$q10_1_r+UNWsvy_Sep5$variables$q9_1_r)
table(UNWsvy_Sep5$variables$seekingwork)
UNWsvy_Sep5$variables$seekingwork[UNWsvy_Sep5$variables$seekingwork<=2]<-0
UNWsvy_Sep5$variables$seekingwork[UNWsvy_Sep5$variables$seekingwork==3]<-100

UNWsvy_Sep5$variables$tocalculateunemployment<-(as.numeric(UNWsvy_Sep5$variables$primarystatus_r_r)+UNWsvy_Sep5$variables$seekingwork)

table(UNWsvy_Sep5$variables$tocalculateunemployment)
table(UNWsvy_Sep5$variables$primarystatus_r_r)
table(as.numeric(UNWsvy_Sep5$variables$primarystatus_r_r))

UNWsvy_Sep5$variables$laborforcebreakdown<-UNWsvy_Sep5$variables$tocalculateunemployment
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==3]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==4]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==5]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==6]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==7]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==8]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==9]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==10]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==11]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==12]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==13]<-0
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==14]<-3
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==108]<-2
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==109]<-2
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==110]<-2
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==112]<-2
UNWsvy_Sep5$variables$laborforcebreakdown[UNWsvy_Sep5$variables$laborforcebreakdown==114]<-3

freq(UNWsvy_Sep5$variables$laborforcebreakdown, UNWsvy_Sep5$variables$indwt)

freq(UNWsvy_Sep5$variables$laborforcebreakdown, UNWsvy_Sep5$variables$indwt)
crosstab(UNWsvy_Sep5$variables$laborforcebreakdown, UNWsvy_Sep5$variables$sex, UNWsvy_Sep5$variables$indwt, prop.c=TRUE)

##0 out of labor force
##2 unemployed
##3 employed

UNWsvy_Sep5$variables$laborforceparticipation<-UNWsvy_Sep5$variables$laborforcebreakdown
UNWsvy_Sep5$variables$laborforceparticipation[UNWsvy_Sep5$variables$laborforceparticipation<=1]<-0
UNWsvy_Sep5$variables$laborforceparticipation[UNWsvy_Sep5$variables$laborforceparticipation>=2]<-1
table(UNWsvy_Sep5$variables$laborforcebreakdown)
table(UNWsvy_Sep5$variables$laborforceparticipation)
crosstab(UNWsvy_Sep5$variables$laborforceparticipation, UNWsvy_Sep5$variables$sex, w=UNWsvy_Sep5$variables$indwt, prop.c = TRUE)
workingage<-subset(UNWsvy_Sep5, UNWsvy_Sep5$variables$age<=64)
crosstab(workingage$laborforceparticipation, workingage$sex, w=workingage$indwt, prop.c = TRUE)

UNWsvy_Sep5$variables$employedorunemployed<-UNWsvy_Sep5$variables$laborforcebreakdown
UNWsvy_Sep5$variables$employedorunemployed[UNWsvy_Sep5$variables$employedorunemployed<=1]<-NA
UNWsvy_Sep5$variables$employedorunemployed[UNWsvy_Sep5$variables$employedorunemployed==2]<-0
UNWsvy_Sep5$variables$employedorunemployed[UNWsvy_Sep5$variables$employedorunemployed==3]<-1
table(UNWsvy_Sep5$variables$employedorunemployed)
freq(UNWsvy_Sep5$variables$employedorunemployed, w=UNWsvy_Sep5$variables$indwt)
crosstab(UNWsvy_Sep5$variables$employedorunemployed, UNWsvy_Sep5$variables$sex, w=UNWsvy_Sep5$variables$indwt, prop.c = TRUE)
##0 out of labor force
##2 unemployed
##3 employed

##DISPCON
#Sum
names(UNWsvy_Sep5)
summary(UNWsvy_Sep5$variables$q86)
table(UNWsvy_Sep5$variables$q86)
#Cleaning
UNWsvy_Sep5$variables$DISPCON<-UNWsvy_Sep5$variables$q86
UNWsvy_Sep5$variables$DISPCON[UNWsvy_Sep5$variables$DISPCON==-3]<-NA
table(UNWsvy_Sep5$variables$DISPCON)

###SELFCON
names(UNWsvy_Sep5)
summary(UNWsvy_Sep5$variables$q80)
table(UNWsvy_Sep5$variables$q80)
#Cleaning (0-3 no sc, 4-6 some sc, 7-8 sc, 9-10 very sc)
UNWsvy_Sep5$variables$SELFCON<-UNWsvy_Sep5$variables$q80
UNWsvy_Sep5$variables$SELFCON[UNWsvy_Sep5$variables$SELFCON==-1]<-NA
UNWsvy_Sep5$variables$SELFCON[UNWsvy_Sep5$variables$SELFCON==-2]<-NA
table(UNWsvy_Sep5$variables$SELFCON)

#####OWN SERIES



table(UNWsvy_Sep5$variables$q90_)
UNWsvy_Sep5$variables$q90_1[UNWsvy_Sep5$variables$q90_1<=-1]<-NA
UNWsvy_Sep5$variables$q90_2[UNWsvy_Sep5$variables$q90_2<=-1]<-NA
UNWsvy_Sep5$variables$q90_3[UNWsvy_Sep5$variables$q90_3<=-1]<-NA
UNWsvy_Sep5$variables$q90_4[UNWsvy_Sep5$variables$q90_4<=-1]<-NA
UNWsvy_Sep5$variables$q90_5[UNWsvy_Sep5$variables$q90_5<=-1]<-NA
UNWsvy_Sep5$variables$q90_6[UNWsvy_Sep5$variables$q90_6<=-1]<-NA
UNWsvy_Sep5$variables$q90_7[UNWsvy_Sep5$variables$q90_7<=-1]<-NA
UNWsvy_Sep5$variables$q90_8[UNWsvy_Sep5$variables$q90_8<=-1]<-NA
UNWsvy_Sep5$variables$q90_9[UNWsvy_Sep5$variables$q90_9<=-1]<-NA
UNWsvy_Sep5$variables$q90_10[UNWsvy_Sep5$variables$q90_10<=-1]<-NA
UNWsvy_Sep5$variables$q90_11[UNWsvy_Sep5$variables$q90_11<=-1]<-NA
###Own as new category
UNWsvy_Sep5$variables$OWN <- (UNWsvy_Sep5$variables$q90_1+
                                UNWsvy_Sep5$variables$q90_2+
                                UNWsvy_Sep5$variables$q90_3+
                                UNWsvy_Sep5$variables$q90_4+
                                UNWsvy_Sep5$variables$q90_5+
                                UNWsvy_Sep5$variables$q90_6+
                                UNWsvy_Sep5$variables$q90_7+
                                UNWsvy_Sep5$variables$q90_8+
                                UNWsvy_Sep5$variables$q90_9+
                                UNWsvy_Sep5$variables$q90_10+
                                UNWsvy_Sep5$variables$q90_11)

UNWsvy_Sep5$variables$OWN_f<-as.factor(UNWsvy_Sep5$variables$OWN)
hist(UNWsvy_Sep5$variables$OWN)

##Children or not in household
UNWsvy_Sep5$variables$childdummy<-(UNWsvy_Sep5$variables$n4-UNWsvy_Sep5$variables$n5)
table(UNWsvy_Sep5$variables$childdummy)
UNWsvy_Sep5$variables$childdummy[UNWsvy_Sep5$variables$childdummy>=1]<-1
table(UNWsvy_Sep5$variables$childdummy)
#HAPPMEA
table(UNWsvy_Sep5$variables$q81)

UNWsvy_Sep5$variables$HAPPMEA<-UNWsvy_Sep5$variables$q81
UNWsvy_Sep5$variables$HAPPMEA[UNWsvy_Sep5$variables$HAPPMEA<=-1]<-NA
table(UNWsvy_Sep5$variables$HAPPMEA)

#AGEGRO
names(UNWsvy_Sep5)
summary(UNWsvy_Sep5$variables$age)
table(UNWsvy_Sep5$variables$age)
#Cleaning (18 - 34, 35 - 54, 55+)
UNWsvy_Sep5$variables$AGEGRO<-UNWsvy_Sep5$variables$age
UNWsvy_Sep5$variables$AGEGRO[UNWsvy_Sep5$variables$AGEGRO >=18 & UNWsvy_Sep5$variables$AGEGRO <= 35]<-0
UNWsvy_Sep5$variables$AGEGRO[UNWsvy_Sep5$variables$AGEGRO >=36 & UNWsvy_Sep5$variables$AGEGRO <= 55]<-1
UNWsvy_Sep5$variables$AGEGRO[UNWsvy_Sep5$variables$AGEGRO >=56]<-2
table(UNWsvy_Sep5$variables$AGEGRO)

#EDUGRO
names(UNWsvy_Sep5)
summary(UNWsvy_Sep5$variables$q14)
table(UNWsvy_Sep5$variables$q14)
#cleaning
UNWsvy_Sep5$variables$EDUGRO<-UNWsvy_Sep5$variables$q14
UNWsvy_Sep5$variables$EDUGRO[UNWsvy_Sep5$variables$EDUGRO==-3]<-NA
UNWsvy_Sep5$variables$EDUGRO[UNWsvy_Sep5$variables$EDUGRO >=1 & UNWsvy_Sep5$variables$EDUGRO <= 4]<-0
UNWsvy_Sep5$variables$EDUGRO[UNWsvy_Sep5$variables$EDUGRO >=5 & UNWsvy_Sep5$variables$EDUGRO <= 6]<-1
UNWsvy_Sep5$variables$EDUGRO[UNWsvy_Sep5$variables$EDUGRO >=7]<-2
table(UNWsvy_Sep5$variables$EDUGRO)

#MARAGE
names(UNWsvy_Sep5)
summary(UNWsvy_Sep5$variables$q87)
table(UNWsvy_Sep5$variables$q87)
#cleaning (14 - 17 = 0, 18,25 = 1, 26,35=2, 36+=3, never=4)
UNWsvy_Sep5$variables$MARAGE<-UNWsvy_Sep5$variables$q87
freq(UNWsvy_Sep5$variables$MARAGE)
UNWsvy_Sep5$variables$MARAGE[UNWsvy_Sep5$variables$MARAGE==-5]<-0
UNWsvy_Sep5$variables$MARAGE[UNWsvy_Sep5$variables$MARAGE >=1]<-1
table(UNWsvy_Sep5$variables$MARAGE)

####Factor
UNWsvy_Sep5$variables$OWN_f <-as.factor(UNWsvy_Sep5$variables$OWN)
UNWsvy_Sep5$variables$AGEGRO_f <-as.factor(UNWsvy_Sep5$variables$AGEGRO)
UNWsvy_Sep5$variables$EDUGRO_f <-as.factor(UNWsvy_Sep5$variables$EDUGRO)

#Own and happiness
happ_sep5_own= svyglm(HAPPMEA ~ OWN_f + 
                        sex +
                        DISPCON +
                        age*childdummy +
                        stratum +
                        nadhh +
                        EDUGRO_f, design=UNWsvy_Sep5)
summary(happ_sep5_own)

x<-ggpredict(happ_sep5_own, terms = c("OWN_f"))
x$predicted
y<-ggpredict(happ_sep5_own, terms = c("age", "childdummy"))
plot(y)

###age and children
happ_a21= svyglm(HAPPMEA ~ OWN + 
                   sex +
                   DISPCON +
                   AGEGRO_f*childdummy +
                   stratum +
                   nadhh +
                   EDUGRO_f, design=UNWsvy_Sep5)
summary(happ_a21)

plot(ggpredict(happ_a21, terms = c("AGEGRO_f")))

