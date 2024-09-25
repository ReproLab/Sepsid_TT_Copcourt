#libraries for analysis
library(ggplot2)
library(ggfortify)
library(magrittr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(imputeTS)
library(readr)
library(devtools)
library(stats)


#upload documents
sepsids<-read_csv("analysis all.csv")
View(sepsids)
str(sepsids)
#to make sure all characters are categorised as numeric
i<-c(5:102)
sepsids[ , i] <- apply(sepsids[ , i], 2,            # Specify own function within apply
                       function(x) as.numeric(as.character(x)))
#check categorisation
sapply(sepsids,class)

###### creating plots for Control PCA #####
#Subset out the controls
sepsids.c<-subset(sepsids,Status=="control")
View(sepsids.c)
#tested for replacing NAs with 0.1 or omitting NAs, no difference but going with replacement for accuracy
#replacing NA in columns
sepsids.c.replace<-na_replace(sepsids.c,0.1)
View(sepsids.c.replace)
#reording populations
sepsids.c.replace$Population<-factor(sepsids.c.replace$Population,levels = c("ON","NY","DE","IT"))
#prepping dataset for pca
crtl.replace<-sepsids.c.replace[c(5:102)]
View(crtl.replace)
#PCA for control with replaced NAs
crtl.replace.pca<-prcomp(crtl.replace)
crtl.replace.pca
summary(crtl.replace.pca)
loading.replace<-crtl.replace.pca$rotation[,1:5]
View(loading.replace)


### PCA plot for controls ###
autoplot(crtl.replace.pca,data=sepsids.c.replace,col="Population",shape="Population",
         size=3,loadings=T,loadings.label=T,frame.type="t") +
  scale_fill_manual(values = c("#ffc000","#ed7d31","#0070c0","#C46FFD")) +     #for the frames
  scale_colour_manual(values = c("#ffc000","#ed7d31","#0070c0","#C46FFD")) +   #for the points 
  #scale_color_discrete(breaks=c("ON","NY","DE","IT"))+ 
  scale_shape_manual(breaks=c("ON","NY","DE","IT"),values=c(15:18))+
  #scale_shape_manual()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),     #removing grid
        panel.background = element_blank(), axis.line = element_line(colour = "black"))   #removing grey background


###### creating plots for T2 PCA #####
sepsids.t2<-subset(sepsids,Status=="t2")
View(sepsids.t2)
#replacing NA in columns
sepsids.t2.replace<-na_replace(sepsids.t2,0.1)
View(sepsids.t2.replace)
#reordering
sepsids.t2.replace$Population<-factor(sepsids.t2.replace$Population,levels = c("NY","DE","IT"))
#prepping NAs replaced dataset for T2 PCA
t2.replace<-sepsids.t2.replace[c(5:102)]
View(t2.replace)
#PCA for T2 with replaced NAs
t2.replace.pca<-prcomp(t2.replace)
t2.replace.pca
summary(t2.replace.pca)
t2.loading<-t2.replace.pca$rotation[,1:5]
View(t2.loading)

### PCA plot for T2 ###
autoplot(t2.replace.pca,data=sepsids.t2.replace,col="Population",shape="Population",
         size=3,loadings=T,loadings.label=T,frame.type="t") +
  scale_fill_manual(values = c("#ed7d31","#0070c0","#C46FFD")) +     #for the frames
  scale_colour_manual(values = c("#ed7d31","#0070c0","#C46FFD")) +   #for the points 
  #scale_color_discrete(breaks=c("NY","DE","IT"))+ 
  scale_shape_manual(breaks=c("NY","DE","IT"),values=c(16:18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),     #removing grid
        panel.background = element_blank(), axis.line = element_line(colour = "black"))   #removing grey background

###### creating plots for T3 PCA #####
sepsids.t3<-subset(sepsids,Status=="t3")
View(sepsids.t3)
#replacing NA in columns
sepsids.t3.replace<-na_replace(sepsids.t3,0.1)
View(sepsids.t3.replace)
#reordering
sepsids.t3.replace$Population<-factor(sepsids.t3.replace$Population,levels = c("NY","DE","IT"))
#prepping NAs replaced dataset for T3 PCA
t3.replace<-sepsids.t3.replace[c(5:102)]
View(t3.replace)
#PCA for T3 with replaced NAs
t3.replace.pca<-prcomp(t3.replace)
t3.replace.pca
summary(t3.replace.pca)
t3.loading<-t3.replace.pca$rotation[,1:5]
View(t3.loading)

### PCA plot for T3 ###
autoplot(t3.replace.pca,data=sepsids.t3.replace,col="Population",shape="Population",
         size=3,loadings=F,loadings.label=F,frame.type="t") +
  scale_fill_manual(values = c("#ed7d31","#0070c0","#C46FFD")) +     #for the frames
  scale_colour_manual(values = c("#ed7d31","#0070c0","#C46FFD")) +   #for the points 
  #scale_color_discrete(breaks=c("NY","DE","IT"))+ 
  scale_shape_manual(breaks=c("NY","DE","IT"),values=c(16:18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),     #removing grid
        panel.background = element_blank(), axis.line = element_line(colour = "black"))   #removing grey background



###################################################################
#GLM with loadings
#top loading~population
#do one for each treatment
View(sepsids.c.replace)
View(crtl.replace.pca$rotation[,1:2])
sepsids.c.replace$c3.1d.1<-sepsids.c.replace$c3.1d/100
View(sepsids.c.replace)

glm_c.1<-glm(c3.1d.1~Population,data=sepsids.c.replace,family="binomial")
summary(glm_c.1)
glm_c.2<-glm(c3.1d.1~Population,data=sepsids.c.replace,family="quasibinomial")
summary(glm_c.2)
AIC(glm_c.1,glm_c.2)


View(sepsids.t2.replace)
View(t2.replace.pca$rotation[,1:2])
#sepsids.t2.replace$c10.2f.1<-sepsids.t2.replace$c10./100

glm_t2.1<-glm(c10.2f~Population,data=sepsids.t2.replace,family="poisson")
summary(glm_t2.1)
glm_t2.2<-glm(c11.3f~Population,data=sepsids.t2.replace,family="poisson")
summary(glm_t2.2)
AIC(glm_t2.1,glm_t2.2)


View(sepsids.t3.replace)
View(t3.replace.pca$rotation[,1:2])

glm_t3.1<-glm(c1.2f~Population,data=sepsids.t3.replace,family="poisson")
summary(glm_t3.1)
glm_t3.2<-glm(c1.1f~Population,data=sepsids.t3.replace,family="poisson")
summary(glm_t3.2)
AIC(glm_t3.1,glm_t3.2)
