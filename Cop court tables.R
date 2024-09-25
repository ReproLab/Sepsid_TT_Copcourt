#cleaned codes####
#Load libraries####
library(dplyr)
library(ggplot2)
library(lme4)
library(MASS)
library(pscl)
library(DHARMa)
library(readr)
library(MuMIn)
library(rstanarm)
library(lmerTest)

#Input final dataset####
#fertile_rate=fr   #mate_rate=mr
mr <- read_csv("C:/Users/tyron/OneDrive/Desktop/Sepsid manuscript/Statistical analysis/Plot for offspring success/M.nm.rawdata_Final.csv")
View(mr)
fr <- read_csv("C:/Users/tyron/OneDrive/Desktop/Sepsid manuscript/Statistical analysis/Plot for offspring success/bp_abundance_Final.csv")
View(fr)
bt <- read_csv("C:/Users/tyron/OneDrive/Desktop/Sepsid manuscript/Statistical analysis/Plot for offspring success/BORIS time.csv")
View(bt)

#############################################################################################################################
#####Setting up baselines for table 1########################################################################################
#############################################################################################################################

#ottawa control
ON_C_fr<-subset(fr, Population=="ON")
View(ON_C_fr)
mean(ON_C_fr$Offsprings)
sd(ON_C_fr$Offsprings)
nrow(ON_C_fr)

ON_C_mr<-subset(mr,Population=="ON"&Status=="Control")
View(ON_C_mr)
nrow(ON_C_mr)

#ottawa T1
ON_T1_mr<-subset(mr,Population=="ON"&Status=="T1")
View(ON_T1_mr)
nrow(ON_T1_mr)

#ottawa T2
ON_T2_mr<-subset(mr,Population=="ON"&Status=="T2")
View(ON_T2_mr)
nrow(ON_T2_mr)

#ottawa T3
ON_T3_mr<-subset(mr,Population=="ON"&Status=="T3")
View(ON_T3_mr)
nrow(ON_T3_mr)

#new york control
NY_C_fr<-subset(fr, Population=="NY"&Status=="Control")
View(NY_C_fr)
mean(NY_C_fr$Offsprings)
sd(NY_C_fr$Offsprings)
nrow(NY_C_fr)

NY_C_mr<-subset(mr, Population=="NY"&Status=="Control")
View(NY_C_mr)
nrow(NY_C_mr)

#new york T1
NY_T1_mr<-subset(mr,Population=="NY"&Status=="T1")
View(NY_T1_mr)
nrow(NY_T1_mr)

#new york T2
NY_T2_fr<-subset(fr, Population=="NY"&Status=="T2")
View(NY_T2_fr)
mean(NY_T2_fr$Offsprings)
sd(NY_T2_fr$Offsprings)
nrow(NY_T2_fr)

NY_T2_mr<-subset(mr, Population=="NY"&Status=="T2")
View(NY_T2_mr)
nrow(NY_T2_mr)

#new york T3
NY_T3_fr<-subset(fr, Population=="NY"&Status=="T3")
View(NY_T3_fr)
mean(NY_T3_fr$Offsprings)
sd(NY_T3_fr$Offsprings)
nrow(NY_T3_fr)

NY_T3_mr<-subset(mr, Population=="NY"&Status=="T3")
View(NY_T3_mr)
nrow(NY_T3_mr)

#ludwig control
DE_C_fr<-subset(fr, Population=="DE" & Status=="Control")
View(DE_C_fr)
mean(DE_C_fr$Offsprings)
sd(DE_C_fr$Offsprings)
nrow(DE_C_fr)

DE_C_mr<-subset(mr, Population=="DE" & Status=="Control")
View(DE_C_mr)
nrow(DE_C_mr)

#ludwig T1
DE_T1_mr<-subset(mr,Population=="DE"&Status=="T1")
View(DE_T1_mr)
nrow(DE_T1_mr)

#ludwig T2
DE_T2_fr<-subset(fr, Population=="DE" & Status=="T2")
View(DE_T2_fr)
mean(DE_T2_fr$Offsprings)
sd(DE_T2_fr$Offsprings)
nrow(DE_T2_fr)

DE_T2_mr<-subset(mr, Population=="DE" & Status=="T2")
View(DE_T2_mr)
nrow(DE_T2_mr)

#ludwig T3
DE_T3_fr<-subset(fr, Population=="DE" & Status=="T3")
View(DE_T3_fr)
mean(DE_T3_fr$Offsprings)
sd(DE_T3_fr$Offsprings)
nrow(DE_T3_fr)

DE_T3_mr<-subset(mr, Population=="DE" & Status=="T3")
View(DE_T3_mr)
nrow(DE_T3_mr)

#italy control
IT_C_fr<-subset(fr, Population=="IT" & Status=="Control")
View(IT_C_fr)
mean(IT_C_fr$Offsprings)
sd(IT_C_fr$Offsprings)
nrow(IT_C_fr)

IT_C_mr<-subset(mr, Population=="IT" & Status=="Control")
View(IT_C_mr)
nrow(IT_C_mr)

#italy T1
IT_T1_mr<-subset(mr, Population=="IT" & Status=="T1")
View(IT_T1_mr)
nrow(IT_T1_mr)

#italy T2
IT_T2_fr<-subset(fr, Population=="IT" & Status=="T2")
View(IT_T2_fr)
mean(IT_T2_fr$Offsprings)
sd(IT_T2_fr$Offsprings)
nrow(IT_T2_fr)

IT_T2_mr<-subset(mr, Population=="IT" & Status=="T2")
View(IT_T2_mr)
nrow(IT_T2_mr)

#italy T3
IT_T3_fr<-subset(fr, Population=="IT" & Status=="T3")
View(IT_T3_fr)
mean(IT_T3_fr$Offsprings)
sd(IT_T3_fr$Offsprings)
nrow(IT_T3_fr)

IT_T3_mr<-subset(mr, Population=="IT" & Status=="T3")
View(IT_T3_mr)
nrow(IT_T3_mr)

#############################################################################################################################
#####Setting up baselines for table 1########################################################################################
#############################################################################################################################
#setting up data####
#removing T1
View(mr)
mr_noT1<-mr[-(which(mr$Status%in%"T1")),] #code for removing certain values
View(mr_noT1)

#View(fr)
#fr_noT1<-fr[-(which(fr$Status%in%"T1")),] #code for removing certain values
#View(fr_noT1)

#removing ON
View(mr)
mr_noON<-mr[-(which(mr$Population%in%"ON")),]
View(mr_noON)

View(fr)
fr_noON<-fr[-(which(fr$Population%in%"ON")),]
View(fr_noON)

#removing T1 and ON
View(mr)
mr_noT1<-mr[-(which(mr$Status%in%"T1")),]
View(mr_noT1)
mr_noT1noON<-mr[-(which(mr_noT1$Population%in%"ON")),]
View(mr_noT1noON)

########################################################
#global model for mate acquisition#### with full set####
BLR_full0<-glm(Mated~Continent*Status+Population*Status,data=mr,family="binomial")
summary(BLR_full0)

BLR_full1<-glm(Mated~Continent*Status,data=mr,family="binomial")
summary(BLR_full1)

BLR_full2<-glm(Mated~Population*Status,data=mr,family="binomial")
summary(BLR_full2)

AIC(BLR_full0,BLR_full1,BLR_full2)

########################################################
#global model for mate acquisition#### with no T1#######
BLR_noT1_0<-glm(Mated~Continent*Status+Population*Status,data=mr_noT1,family="binomial")
summary(BLR_noT1_0)

BLR_noT1_1<-glm(Mated~Continent*Status,data=mr_noT1,family="binomial")
summary(BLR_noT1_1)

BLR_noT1_2<-glm(Mated~Population*Status,data=mr_noT1,family="binomial")
summary(BLR_noT1_2)

AIC(BLR_noT1_0,BLR_noT1_1,BLR_noT1_2)

########################################################
#global model for mate acquisition#### with no ON#######
BLR_noON_0<-glm(Mated~Continent*Status+Population*Status,data=mr_noON,family="binomial")
summary(BLR_noON_0)

BLR_noON_1<-glm(Mated~Continent*Status,data=mr_noON,family="binomial")
summary(BLR_noON_1)

BLR_noON_2<-glm(Mated~Population*Status,data=mr_noON,family="binomial")
summary(BLR_noON_2)

AIC(BLR_noON_0,BLR_noON_1,BLR_noON_2)

########################################################
#global model for mate acquisition#### with no T1 and ON#######
BLR_noT1noON_0<-glm(Mated~Continent*Status+Population*Status,data=mr_noT1noON,family="binomial")
summary(BLR_noT1noON_0)

BLR_noT1noON_1<-glm(Mated~Continent*Status,data=mr_noT1noON,family="binomial")
summary(BLR_noT1noON_1)

BLR_noT1noON_2<-glm(Mated~Population*Status,data=mr_noT1noON,family="binomial")
summary(BLR_noT1noON_2)

AIC(BLR_noT1noON_0,BLR_noT1noON_1,BLR_noT1noON_2)

AIC(BLR_full0,BLR_full1,BLR_full2,
    BLR_noT1_0,BLR_noT1_1,BLR_noT1_2,
    BLR_noON_0,BLR_noON_1,BLR_noON_2,
    BLR_noT1noON_0,BLR_noT1noON_1,BLR_noT1noON_2)


########################################################
#global model for mate acquisition#### with full set####
POI_full0<-glm(Offsprings~Continent*Status+Population*Status,data=fr,family="poisson")
summary(POI_full0)

POI_full1<-glm(Offsprings~Continent*Status,data=fr,family="poisson")
summary(POI_full1)

POI_full2<-glm(Offsprings~Population*Status,data=fr,family="poisson")
summary(POI_full2)

AIC(POI_full0,POI_full1,POI_full2)

########################################################
#global model for mate acquisition#### with no T1#######
#POI_noT1_0<-glm(Offsprings~Continent*Status+Population*Status,data=fr_noT1,family="poisson")
#summary(POI_noT1_0)

#POI_noT1_1<-glm(Offsprings~Continent*Status,data=fr_noT1,family="poisson")
#summary(POI_noT1_1)

#POI_noT1_2<-glm(Offsprings~Population*Status,data=fr_noT1,family="poisson")
#summary(POI_noT1_2)

#AIC(POI_noT1_0,POI_noT1_1,POI_noT1_2)

########################################################
#global model for mate acquisition#### with no ON#######
POI_noON_0<-glm(Offsprings~Continent*Status+Population*Status,data=fr_noON,family="poisson")
summary(POI_noON_0)

POI_noON_1<-glm(Offsprings~Continent*Status,data=fr_noON,family="poisson")
summary(POI_noON_1)

POI_noON_2<-glm(Offsprings~Population*Status,data=fr_noON,family="poisson")
summary(POI_noON_2)

AIC(POI_noON_0,POI_noON_1,POI_noON_2)

########################################################
#global model for mate acquisition#### with no T1 and ON#######
#POI_noT1noON_0<-glm(Offsprings~Continent*Status+Population*Status,data=fr_noT1noON,family="poisson")
#summary(POI_noT1noON_0)

#POI_noT1noON_1<-glm(Offsprings~Continent*Status,data=fr_noT1noON,family="poisson")
#summary(POI_noT1noON_1)

#POI_noT1noON_2<-glm(Offsprings~Population*Status,data=fr_noT1noON,family="poisson")
#summary(POI_noT1noON_2)

#AIC(POI_noT1noON_0,POI_noT1noON_1,POI_noT1noON_2)

AIC(POI_full0,POI_full1,POI_full2,
    POI_noON_0,POI_noON_1,POI_noON_2)


#CONCLUSION####
#For maximal model, using the dataset without ON and using
#population is better in explaining

#breaking the questions into different components####

  #qns 1: comparing mating and fertilising within a species
    #Find sig diff in mr and fr across continents
    #use only controls, compare continents

  #qns 2: comparing if limb loss affects mating/fertile
    #find sig diff in mr and fr across treatments 
    #split dataset by country, compare treatments

#1. mr across continents ####
#BLR - Control - fixed Continent and Population
View(mr)
mr_control<-subset(mr,Status=="Control")
nrow(mr_control)
View(mr_control)
str(mr_control)
#mr_control$Population<-as.factor(mr_control$Population)
BLR_control_1<-glm(Mated~Continent+Population,data=mr_control,family="binomial")
summary(BLR_control_1)                  #Continent and population not sig here
anova(BLR_control_1,test="Chisq")       #Continent sig but population not sig


#BLR - Control - fixed Continent and random Population
BLR_control_2<-glmer(Mated~Continent+(1|Population),data=mr_control,family="binomial")
summary(BLR_control_2)
anova(BLR_control_2,test="Chisq")
BLR_control_3<-glmer(Mated~(Continent|Population),data=mr_control,family="binomial")
summary(BLR_control_3)
anova(BLR_control_3,test="Chisq")
#giving signular error - aka no systematic effect from pop as random
#indicating pop as random is complicating the model

#BLR - Control - fixed Continent only
BLR_control_4<-glm(Mated~Continent,data=mr_control,family="binomial")
summary(BLR_control_4)
anova(BLR_control_4,test="Chisq")

#BLR - Control - fixed Population only
BLR_control_5<-glm(Mated~Population,data=mr_control,family="binomial")
summary(BLR_control_5)
anova(BLR_control_5,test="Chisq")

#BLR - Control - fixed Continent and Population and interaction
BLR_control_6<-glm(Mated~Continent*Population,data=mr_control,family="binomial")
summary(BLR_control_6)
anova(BLR_control_6,test="Chisq")

#AIC to explain
AIC(BLR_control_1,BLR_control_2,
    BLR_control_3,BLR_control_4,
    BLR_control_5,BLR_control_6)

#1. fr across continents ####
#poisson - Control - fixed Continent and Population
View(fr)
fr_control<-subset(fr,Status=="Control")
View(fr_control)
fr_control_1<-glm(Offsprings~Continent+Population,data=fr_control,family="poisson")
summary(fr_control_1)
anova(fr_control_1,test="Chisq")
alias(fr_control_1)
#negbin - control - fixed continent and population
fr_control_2<-glm.nb(Offsprings~Continent+Population,data=fr_control) #same warning messages as Terni
summary(fr_control_2) #can still run but with correlation error too
alias(fr_control_2)
anova(fr_control_2,test="Chisq") #Warning messages

#test for excess 0
sum(fr_control$Offsprings==0) #actual number of 0s in the dataset
fr_control_2_preds=predict(fr_control_2,type="response")
fr_control_2_esttheta=summary(fr_control_2$theta)
fr_control_2_prop0=dnbinom(x=0,mu=fr_control_2_preds,size=fr_control_2_esttheta)
round(sum(fr_control_2_prop0)) #predicted number of 0s from model
#number of 0 observed > number of 0 predicted
#therefore moving forward, I should be using zero-infl to model

#0-inf - control - fixed continent and population
fr_control_3<-zeroinfl(Offsprings~Continent+Population,data=fr_control,dist="negbin",maxit=1000)
#error messages meaning lack of variance in data unable to proceed
#too many variables choose one or the other
summary(fr_control_3)
anova(fr_control_3,test="Chisq")

#0-inf - control - fixed continent
fr_control_3<-zeroinfl(Offsprings~Continent,data=fr_control,dist="negbin")
summary(fr_control_3)
anova(fr_control_3,test="Chisq") #not the way to do chisq for 0-inf

#0-inf - control - fixed population
fr_control_4<-zeroinfl(Offsprings~Population,data=fr_control,dist="negbin")
summary(fr_control_4)

#model selection
model.sel(fr_control_1,fr_control_2,
          fr_control_3,fr_control_4)

#template for how to do chisq for 0-inf ####
#issues now is that the chisq isn't appropriate for continuous sets
fr_control_3_null<-update(fr_control_3,.~1)
pchisq(2*(logLik(fr_control_3)-logLik(fr_control_3_null)),df=5,lower.tail=F) #TO CHECK IF THIS IS HOW WE SHOW CHISQ
qchisq(p=0.09020217,df=5,lower.tail = T)

fr_control_4_null<-update(fr_control_4,.~1)
pchisq(2*(logLik(fr_control_4)-logLik(fr_control_4_null)),df=9,lower.tail=F) #TO CHECK IF THIS IS HOW WE SHOW CHISQ
qchisq(p=0.1181791,df=9,lower.tail = T)

#qns 2: comparing if limb loss affects mating/fertile
#find sig diff in mr and fr across treatments 
#split dataset by country, compare treatments

# 2. mr by ON only ####
#technically can't use cause T1,T2,T3 are not
View(mr)
mr_ON<-subset(mr,Population=="ON")
View(mr_ON)
mr_ON<-mr_ON[-c(55:64),]
mr_ON_1<-glm(Mated~Status,data=mr_ON,family="binomial")
summary(mr_ON_1)
anova(mr_ON_1,test="Chisq")
#NP suggestion to just run a chisq test for each
View(mr_ON)
#for T2
mr_ON_T2<-mr_ON[-c(134:193),]
chisq.test(x=mr_ON_T2$Status,y=mr_ON_T2$Mated)
#for T3
mr_ON_T3<-mr_ON[-c(55:133),]
chisq.test(x=mr_ON_T3$Status,y=mr_ON_T3$Mated)

#try 0-inf
mr_ON_2<-zeroinfl(Mated~Status,data=mr_ON,dist="negbin")
summary(mr_ON_2)
stan_glm(data=mr_ON,Mated~Status,offset=NULL,family=neg_binomial_2)
?print.stanreg
1/(1+1/(exp(-0.5306-17.0354)))

#idea to use proportion test -chisq test####
#NA:ON C vs T1
prop.test(c(20,0),c(54,10))
#NA:ON C vs T2
prop.test(c(20,0),c(54,79))
#NA:ON C vs T3
prop.test(c(20,0),c(54,69))

#NA:NY C vs T1
prop.test(c(20,0),c(56,40))
#NA:NY C vs T2
prop.test(c(20,13),c(56,90))
#NA:NY C vs T3
prop.test(c(20,1),c(56,69))

#EU:DE C vs T1
prop.test(c(30,0),c(58,10))
#EU:DE C vs T2
prop.test(c(30,20),c(58,32))
#EU:DE C vs T3
prop.test(c(30,13),c(58,50))

#EU:IT C vs T1
prop.test(c(34,0),c(64,40))
#EU:IT C vs T2
prop.test(c(34,20),c(64,67))
#EU:IT C vs T3
prop.test(c(34,20),c(64,69))

# 2. mr by NY only ####
mr_NY<-subset(mr,Population=="NY")
View(mr_NY)
mr_NY<-mr_NY[-c(57:96),] #omitted T1 because not using
View(mr_NY)
mr_NY_1<-glm(Mated~Status,data=mr_NY,family="binomial")
summary(mr_NY_1)
anova(mr_NY_1,test="Chisq")

# 3. mr by DE only ####
mr_DE<-subset(mr,Population=="DE")
View(mr_DE)
mr_DE<-mr_DE[-c(59:68),] #omitted T1 because not using
View(mr_DE)
mr_DE_1<-glm(Mated~Status,data=mr_DE,family="binomial")
summary(mr_DE_1)
anova(mr_DE_1,test="Chisq")

# 3. mr by IT only ####
mr_IT<-subset(mr,Population=="IT")
View(mr_IT)
mr_IT<-mr_IT[-c(65:104),] #omitted T1 because not using
View(mr_IT)
mr_IT_1<-glm(Mated~Status,data=mr_IT,family="binomial")
summary(mr_IT_1)
anova(mr_IT_1,test="Chisq")

#  3. mr as a whole across all populations ####
View(mr)
mr_no_T1<-mr[-c(673:712,517:526,260:299,55:64),]
View(mr_no_T1)
mr_overall_1 <- glm(Mated~Status, data=mr_no_T1,family="binomial")
summary(mr_overall_1)

# 3. fr by ON only ####
fr_ON<-subset(fr,Population=="ON")
View(fr_ON)
#no T to test for status - unable to proceed
fr_ON_1<-glm(Offsprings~Status,data=fr_ON,family="poisson")
#yeap, double confirmed it didnt work

# 3. fr by NY only ####
fr_NY<-subset(fr,Population=="NY")
View(fr_NY)
#no need to remove T1 cause no T1
fr_NY_1<-glm(Offsprings~Status,data=fr_NY,family="poisson")
summary(fr_NY_1)
#super duper over dispersed
fr_NY_2<-glm.nb(Offsprings~Status,data=fr_NY)
summary(fr_NY_2)
model.sel(fr_NY_1,fr_NY_2)
#still super over dispersed -> #test for excess 0
sum(fr_NY$Offsprings==0) #actual number of 0s in the dataset
fr_NY_2_preds=predict(fr_NY_2,type="response")
fr_NY_2_esttheta=summary(fr_NY_2$theta)
fr_NY_2_prop0=dnbinom(x=0,mu=fr_NY_2_preds,size=fr_NY_2_esttheta)
round(sum(fr_NY_2_prop0))
#observed is 6, predicted is 0
#use 0-inf
fr_NY_3<-zeroinfl(Offsprings~Status,data=fr_NY,dist="negbin")
summary(fr_NY_3)
model.sel(fr_NY_1,fr_NY_2,fr_NY_3)
#0-inf worked wew #Now copy and paste for ON,DE,IT
#T not sig diff from C
anova(fr_NY_3,test="Chisq")
fr_NY_null<-update(fr_NY_3,.~1)
pchisq(2*(logLik(fr_NY_3)-logLik(fr_NY_null)),df=7,lower.tail=F)
qchisq(p=0.9705513,df=7,lower.tail = T)

# 3. fr by DE only ####
fr_DE<-subset(fr,Population=="DE")
View(fr_DE)
#No T1 again
fr_DE_1<-glm(Offsprings~Status,data=fr_DE,family="poisson")
summary(fr_DE_1)
#overdispersed
fr_DE_2<-glm.nb(Offsprings~Status,data=fr_DE)
summary(fr_DE_2)
model.sel(fr_DE_1,fr_DE_2)
#kinda overdispersed -> #test for 0-inf in case
sum(fr_DE$Offsprings==0) #actual number of 0s in the dataset
fr_DE_2_preds=predict(fr_DE_2,type="response")
fr_DE_2_esttheta=summary(fr_DE_2$theta)
fr_DE_2_prop0=dnbinom(x=0,mu=fr_DE_2_preds,size=fr_DE_2_esttheta)
round(sum(fr_DE_2_prop0))
#observed > predicted
#try 0-inf
fr_DE_3<-zeroinfl(Offsprings~Status,data=fr_DE,dist="negbin")
summary(fr_DE_3)
fr_DE_null<-update(fr_DE_3,.~1)
pchisq(2*(logLik(fr_DE_3)-logLik(fr_DE_null)),df=7,lower.tail=F)
qchisq(p=0.6783381,df=7,lower.tail = T)
model.sel(fr_DE_1,fr_DE_2,fr_DE_3)
#O-inf is best of 3
#T has no sig diff from C

# 3. fr by IT only ####
fr_IT<-subset(fr,Population=="IT")
View(fr_IT)
#no T1 again
fr_IT_1<-glm(Offsprings~Status,data=fr_IT,family="poisson")
summary(fr_IT_1)
#super overdispersed
fr_IT_2<-glm.nb(Offsprings~Status,data=fr_IT)
summary(fr_IT_2) #error message of n being too small
model.sel(fr_IT_1,fr_IT_2)
#super overdispersed -> #test for 0-inf in case
sum(fr_IT$Offsprings==0) #actual number of 0s in the dataset
fr_IT_2_preds=predict(fr_IT_2,type="response")
fr_IT_2_esttheta=summary(fr_IT_2$theta)
fr_IT_2_prop0=dnbinom(x=0,mu=fr_IT_2_preds,size=fr_IT_2_esttheta)
round(sum(fr_IT_2_prop0))
#observed >>>> predicted
#try 0-inf
fr_IT_3<-zeroinfl(Offsprings~Status,data=fr_IT,dist="negbin")
summary(fr_IT_3)
fr_IT_null<-update(fr_IT_3,.~1)
pchisq(2*(logLik(fr_IT_3)-logLik(fr_IT_null)),df=7,lower.tail=F)
qchisq(p=0.9210463,df=7,lower.tail = T)
model.sel(fr_IT_1,fr_IT_2,fr_IT_3)

#  3. fr as a whole across all populations ####
View(fr)
fr_overall<-glm(Offsprings~Status,data=fr,family="poisson")
summary(fr_overall)


########################################################
#Test for time variation############################
View(mr)
View(fr)
View(bt)
shapiro.test(mr$Mated)
shapiro.test(fr$Offsprings)
shapiro.test(bt$Duration)

bt_c<-subset(bt,Status=="control")
View(bt_c)

kruskal.test(Duration~Population, data=bt_c)
summary(kruskal.test(Duration~Population, data=bt_c))

kruskal.test(Duration~Continent, data=bt_c)

#ottawa duration
bt_ON_C<-subset(bt, Population=="ON" & Status=="control")
View(bt_ON_C)
mean(bt_ON_C$Duration)
sd(bt_ON_C$Duration)
nrow(ON_C_fr)

#syracuse duration
bt_NY_C<-subset(bt, Population=="NY" & Status=="control")
View(bt_NY_C)
mean(bt_NY_C$Duration)
sd(bt_NY_C$Duration)
nrow(NY_C_fr)

bt_NY_t2<-subset(bt, Population=="NY" & Status=="t2")
View(bt_NY_t2)
mean(bt_NY_t2$Duration)
sd(bt_NY_t2$Duration)
nrow(NY_t2_fr)

bt_NY_t3<-subset(bt, Population=="NY" & Status=="t3")
View(bt_NY_t3)
mean(bt_NY_t3$Duration)
sd(bt_NY_t3$Duration)
nrow(NY_t3_fr)

#ludwig duration
bt_DE_C<-subset(bt, Population=="DE" & Status=="control")
View(bt_DE_C)
mean(bt_DE_C$Duration)
sd(bt_DE_C$Duration)
nrow(DE_C_fr)

bt_DE_t2<-subset(bt, Population=="DE" & Status=="t2")
View(bt_DE_t2)
mean(bt_DE_t2$Duration)
sd(bt_DE_t2$Duration)
nrow(DE_t2_fr)

bt_DE_t3<-subset(bt, Population=="DE" & Status=="t3")
View(bt_DE_t3)
mean(bt_DE_t3$Duration)
sd(bt_DE_t3$Duration)
nrow(DE_t3_fr)

#terni duration
bt_IT_C<-subset(bt, Population=="IT" & Status=="control")
View(bt_IT_C)
mean(bt_IT_C$Duration)
sd(bt_IT_C$Duration)
nrow(IT_C_fr)

bt_IT_t2<-subset(bt, Population=="IT" & Status=="t2")
View(bt_IT_t2)
mean(bt_IT_t2$Duration)
sd(bt_IT_t2$Duration)
nrow(IT_t2_fr)

bt_IT_t3<-subset(bt, Population=="IT" & Status=="t3")
View(bt_IT_t3)
mean(bt_IT_t3$Duration)
sd(bt_IT_t3$Duration)
nrow(IT_t3_fr)






