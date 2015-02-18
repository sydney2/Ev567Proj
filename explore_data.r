#IMPORT DATASET "spider_data3.csv" from Environment window
spiders <- spider_data3

#Summarize each numerical variable using commonly used summary statistics 
#TURN ON DPLYR PACKAGE
means <- function(x) mean(x, na.rm = T)
SDs <- function(x) sd(x, na.rm = T)
Vars <- function(x) var(x, na.rm = T)

#Summary statistics for all sites
spdr_means <- numcolwise(means)(spiders)
spdr_sds <- numcolwise(SDs)(spiders)
spdr_vars <- numcolwise(Vars)(spiders)

#Summary statistics by COUNTY
byCOUNTY_FEM = ddply(spiders,"COUNTY",summarize,Avg = mean(FEM), Var = var(FEM), SD = sd(FEM))
byCOUNTY_CHELA = ddply(spiders,"COUNTY",summarize,Avg = mean(CHELA), Var = var(CHELA), SD = sd(CHELA))
byCOUNTY_TIB = ddply(spiders,"COUNTY",summarize,Avg = mean(TIB), Var = var(TIB), SD = sd(TIB))
byCOUNTY_ABD = ddply(spiders,"COUNTY",summarize,Avg = mean(ABD), Var = var(ABD), SD = sd(ABD))
byCOUNTY_CHELI = ddply(spiders,"COUNTY",summarize,Avg = mean(CHELI), Var = var(CHELI), SD = sd(CHELI))
byCOUNTY_CARP = ddply(spiders,"COUNTY",summarize,Avg = mean(CARP), Var = var(CARP), SD = sd(CARP))
byCOUNTY_TL = ddply(spiders,"COUNTY",summarize,Avg = mean(TL), Var = var(TL), SD = sd(TL))

#Split out morphological data only
body_ratios <- select(spiders,FEM,CHELA, CARP,ABD, CHELI,TL) 
cor.matrix(body_ratios) #Quick check of histograms for normality

#                             ~~oOoOoOo~~

## QQ Plots, checking for normality in morphological data
par(mfrow=c(2,3))
qqnorm(body_ratios$FEM, main = list("Normal Q-Q Plot\nFemur", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$FEM)
qqnorm(body_ratios$CHELA, main = list("Normal Q-Q Plot\nChela.", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$CHELA)
qqnorm(body_ratios$CARP, main = list("Normal Q-Q Plot\nCarapice", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$CARP)
qqnorm(body_ratios$ABD, main = list("Normal Q-Q Plot\nAbdomen", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$ABD)
qqnorm(body_ratios$CHELI, main = list("Normal Q-Q Plot\nCheli.", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$CHELI)
qqnorm(body_ratios$TL, main = list("Normal Q-Q Plot\nTotal Length", cex = 1.5, col = "red", font = 10))
qqline(body_ratios$TL)

#                             ~~oOoOoOo~~

##Boxplot for morphological data by County
#femur
fbx<-ggplot(spiders,aes(y=FEM, x=COUNTY))+theme(text=element_text(size=20))
fbx+geom_boxplot()
fbx+geom_boxplot()+geom_point()
#chela.
chabx<-ggplot(spiders,aes(y=CHELA, x=COUNTY))+theme(text=element_text(size=20))
chabx+geom_boxplot()
chabx+geom_boxplot()+geom_point()
#carapice
crpbx<-ggplot(spiders,aes(y=CARP, x=COUNTY))+theme(text=element_text(size=20))
crpbx+geom_boxplot()
crpbx+geom_boxplot()+geom_point()
#tibia
tbbx<-ggplot(spiders,aes(y=TIB, x=COUNTY))+theme(text=element_text(size=20))
tbbx+geom_boxplot()
tbbx+geom_boxplot()+geom_point()
#abdomen
abbx<-ggplot(spiders,aes(y=ABD, x=COUNTY))+theme(text=element_text(size=20))
abbx+geom_boxplot()
abbx+geom_boxplot()+geom_point()
#cheli.
chibx<-ggplot(spiders,aes(y=CHELI, x=COUNTY))+theme(text=element_text(size=20))
chibx+geom_boxplot()
chibx+geom_boxplot()+geom_point()
#total length
tlbx<-ggplot(spiders,aes(y=CHELI, x=COUNTY))+theme(text=element_text(size=20))
tlbx+geom_boxplot()
tlbx+geom_boxplot()+geom_point()

#                             ~~oOoOoOo~~

##Check relationships with ANOVA by COUNTY
lin_mod = lm(FEM~COUNTY, data = spiders) #Check for significant differnce of femur length amoung countys
anova(lin_mod) #Results: F = 1.4, p = .20 no

lin_mod1 = lm(CHELA~COUNTY, data = spiders) #Check for significant differnce of Chela. length amoung countys
anova(lin_mod1) #Results: F = 1.01, p = .43 no

lin_mod2 = lm(TIB~COUNTY, data = spiders) #Check for significant differnce of Chela. length amoung countys
anova(lin_mod2) #Results: F = 1.2594 p = 0.2779 no

lin_mod3 = lm(CARP~COUNTY, data = spiders) #Check for significant differnce of Chela. length amoung countys
anova(lin_mod3) #Results: F = 0.8592 p = 0.5546 no

lin_mod4 = lm(CHELI~COUNTY, data = spiders) #Check for significant differnce of Chela. length amoung countys
anova(lin_mod4) #Results: F = 3.6008 p = 0.001394 ** YES

lin_mod5 = lm(ABD~COUNTY, data = spiders) #Check for significant differnce of abdomn length amoung countys
anova(lin_mod5) #Results: F = 3.6416 p = 0.001267 ** YES

lin_mod6 = lm(TL~COUNTY, data = spiders) #Check for significant differnce of total. length amoung countys
anova(lin_mod6) #Results: F = 2.8771 p = 0.007625 ** YES

##Check relationships with ANOVA by ECOREGION
lin_mod7 = lm(FEM~ECO, data = spiders) #Check for significant differnce of femur length amoung ECOs
anova(lin_mod7) #Results: F = 1.4, p = .20 no

lin_mod8 = lm(CHELA~ECO, data = spiders) #Check for significant differnce of Chela. length amoung ECOs
anova(lin_mod8) #Results: F =  1.0103 p=0.3178 no

lin_mod9 = lm(TIB~ECO, data = spiders) #Check for significant differnce of Chela. length amoung ECOs
anova(lin_mod9) #Results: F = 3.2629 p=0.07458 no

lin_mod10 = lm(CARP~ECO, data = spiders) #Check for significant differnce of Chela. length amoung ECOs
anova(lin_mod10) #Results: F = 1.4331 p=0.2348 no

lin_mod11 = lm(CHELI~ECO, data = spiders) #Check for significant differnce of Chela. length amoung ECOs
anova(lin_mod11) #Results: F = 0.6086 p=0.4376 no

lin_mod12 = lm(ABD~ECO, data = spiders) #Check for significant differnce of abdomn length amoung ECOs
anova(lin_mod12) #Results: F = 2.9333 p=0.0906 no

lin_mod13 = lm(TL~ECO, data = spiders) #Check for significant differnce of total. length amoung ECOs
anova(lin_mod13) #Results: F = 0.2135 p=0.6453 no

##Check relationships with ANOVA by Litter
lin_mod14 = lm(FEM~TYP, data = spiders) #Check for significant differnce of femur length amoung Litter
anova(lin_mod14) #Results: F = 2.1829 0.09658 no

lin_mod15 = lm(CHELA~TYP, data = spiders) #Check for significant differnce of Chela. length amoung Litter
anova(lin_mod15) #Results: F =  0.7453 0.5283 no

lin_mod16 = lm(TIB~TYP, data = spiders) #Check for significant differnce of Chela. length amoung Litter
anova(lin_mod16) #Results: F = 1.2583 0.2945 no

lin_mod17 = lm(CARP~TYP, data = spiders) #Check for significant differnce of Chela. length amoung Litter
anova(lin_mod17) #Results: F = 0.8163 0.4887 no

lin_mod18 = lm(CHELI~TYP, data = spiders) #Check for significant differnce of Chela. length amoung Litter
anova(lin_mod18) #Results: F = 2.8972 0.04025* YES to .05

lin_mod19 = lm(ABD~TYP, data = spiders) #Check for significant differnce of abdomn length amoung Litter
anova(lin_mod19) #Results: F = 1.7078 0.1721 no

lin_mod20 = lm(TL~TYP, data = spiders) #Check for significant differnce of total. length amoung Litter
anova(lin_mod20) #Results: F = 0.4517 0.7168 no

#                             ~~oOoOoOo~~

#CHELI, ABD, and TL show significant differnce amoung counties, run Tukey test
plot(TukeyHSD(aov(CHELI~COUNTY, data = spiders))) #Tillamook-Mason
plot(TukeyHSD(aov(ABD~COUNTY, data = spiders))) #Mason-Clatsop, Tillomok-Clatsop, Mason-Del Norte
plot(TukeyHSD(aov(TL~COUNTY, data = spiders))) #No significant differnce using family-wise correction
#CHELI showed significant difference amoung litter type, run Tukey test
plot(TukeyHSD(aov(CHELI~TYP, data = spiders))) #Wood-Litter

#                             ~~oOoOoOo~~

#Separate explanatory variables
exp <- spiders[c(1:25,28:79),c(13:15,18:24)]
body_ratios1 <- body_ratios[c(1:25,28:79),]

#                             ~~oOoOoOo~~

#Run RDA
library(vegan)
rda1<-rda(body_ratios1~.,data=exp,scale=T)     #run RDA with standardized data and save everything in a R object (rda.ca)
par(mfrow=c(1,1))
plot(rda1) #triplot
summary(rda1) #detailed RDA output and use the key results to interpret the triplot
round(vif.cca(rda1),2)    #calculate Variance inflation factor for the RDA model to check multi-colliearity issue
anova.cca(rda1,step=1000)   #Overall RDA model significant test
anova.cca(rda1,by="axis", step=1000) #check which RDA axis is signficant

#                             ~~oOoOoOo~~


