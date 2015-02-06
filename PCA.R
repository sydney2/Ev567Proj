#Bern Romey, 04Feb15 ~ ESM567 Term Project
#PCA

#Data
dta<-read.csv("ApochthoniusMorphLatLon04Feb15.csv", header=T) 
dt <-na.omit(dta)
am <- dt[c(6:24)]

#Assumptions
boxplot(am, main = "Not scaled")
boxplot(scale(am), main="Scaled (centered) with Z-score")
boxplot(scale(log(am+1)),main="log transformed")

cor.matrix(scale(am))#source cor.matrix function

cov(scale(am)) #calculate correlatin matrix with the standardized data: 
#Z-score from -1 to 1 (PCC)
cor(am) #same as covariance with scale

#PCA Analysis
require(MASS) #loads the PCA package
pca <- princomp(scale(am)) #creates a PC matrix using the correlation matrix
biplot(pca, expand = 1.05,main = "Biplot", xlab = "Comp.1 (30.1%)", ylab = "Comp.2 (14.8%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC

broken.stick(18) #After comparing, keep components with eigenvalues > broken stick from summary
plot(pca, main="Scree Plot") #Scree plot

round(loadings(pca),2) #Check eigenvectors: length of vector is relative variance and how much it contributes to the PC
#Principal component loading (pg 50).  The further from zero, the greater the contribution.
round(loadings(pca)[,c(1:2)],2) #Loading for PC1 & 2 only

round((pca$scores),2) #PC matrix showing site scores for all PCs. How far each is(SD) from the the grand centroid
#This is the distribution of PC1 and PC2 site scores (top scale).  Each variable for each component. 
#In this case due to broken stick, PC1 and PC2
