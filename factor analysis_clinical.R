# Factor analysis of clinical questionnaire, PTSD data set

# Load packages -----------------------------------------------------------
# psych package
# install.packages('psych')
library(psych)

# MVN package for multivariate normality test
# install.packages("MVN")
library(MVN)

# nFactors package 
# install.packages('nFactors')
library(nFactors)

#install.packages('GPArotation')
library(GPArotation)

library(ggplot2)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# install.packages('xlsx')
# library(xlsx)


# Load data and select subjects ---------------------------------------------------------------

tb = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/question scores EFA_09152018.csv", header = TRUE)

# exclude those who have incomplete data 
exclude = c(117, 1285, 100, 102, 50, 3) 

# find out who are remitted PTSD
remitted = tb$id[tb$group=='R']

# include control, PTSD, remitted PTSD (should be 69 participants)
tb2faGain <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 1
            & tb$isMale==1 & !is.element(tb$id,exclude),]

tb2faLoss <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 0
                & tb$isMale==1 & !is.element(tb$id,exclude),]

# include only control and PTSD, (should be 55 participants)
tb2faGain <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 1
                & tb$isMale==1 & !is.element(tb$id,exclude) 
                & !is.element(tb$id,remitted),]

tb2faLoss <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 0
                & tb$isMale==1 & !is.element(tb$id,exclude) 
                & !is.element(tb$id,remitted),]

# include only control and PTSD, but also include women (should be 59 participants)
tb2faGain <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 1
                & !is.element(tb$id,exclude) 
                & !is.element(tb$id,remitted),]

tb2faLoss <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 0
                & !is.element(tb$id,exclude) 
                & !is.element(tb$id,remitted),]

names(tb2faGain)


# Prepare for PCA or factor analysis ---------------------------------------------
array2fa <- tb2faGain[,c(34:39, 46, 47:49, 54)] # 11 questionnaitre
colnames(array2fa) = c("BDI", "CAPS-ReExp","CAPS-Avoid","CAPS-Numb","CAPS-DysA","CAPS-AnxA",
                       "CTQ","STAI-1","STAI-2","CES","DES")

# test multivariate normality
mvn(array2fa)

# Visualize data structure
cor.plot(array2fa, numbers = TRUE, main="11 Quest Scores", xlas=2)
chart.Correlation(array2fa,
                  histogram = TRUE,
                  method = 'pearson')

# pairs.panels(array2fa,pch='.')


# Z-score
# generate z-scores for variable A using the scale() function
zscored <- scale(array2fa, center = TRUE, scale = TRUE)

# PCA ----------------------------------------------------------------------------
# pcaresult <- princomp(zscored)
pcaresult <- prcomp(array2fa, retx = TRUE, center = TRUE, scale. = TRUE)
summary(pcaresult)
plot(pcaresult)

# plot loadings
# pcaload <- pcaresult$loadings[,]
pcaload <- pcaresult$rotation[,]
barplot(pcaload[,1], main="Component 1", ylab="Loadings", las=2)
barplot(pcaload[,2], main="Component 2", ylab="Loadings", las=2)
barplot(pcaload[,3], main="Component 3", ylab="Loadings", las=2)

# rotated values
score = pcaresult$x


# combine component into data
tb2faGain$comp1 = score[,1]
tb2faGain$comp2 = score[,2]
tb2faGain$comp3 = score[,3]

tb2faLoss$comp1 = score[,1]
tb2faLoss$comp2 = score[,2]
tb2faLoss$comp3 = score[,3]

# save data
write.csv(tb2faGain,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_allSubj_10102018.csv")
write.csv(tb2faLoss,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_allSubj_10102018.csv")

# caculate the components for remitted groups
tb2faGainRemitted <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 1
                        & tb$isMale==1 & !is.element(tb$id,exclude) 
                        & is.element(tb$id,remitted),]

tb2faLossRemitted <- tb[tb$isExcluded_behavior == 0 & tb$isGain == 0
                        & tb$isMale==1 & !is.element(tb$id,exclude) 
                        & is.element(tb$id,remitted),]

scoreRemitted = scale(tb2faGainRemitted[,c(34:39, 46, 47:49, 54)], center= pcaresult$center) %*% pcaload

tb2faGainRemitted$comp1 = scoreRemitted[,1]
tb2faGainRemitted$comp2 = scoreRemitted[,2]
tb2faGainRemitted$comp3 = scoreRemitted[,3]

tb2faLossRemitted$comp1 = scoreRemitted[,1]
tb2faLossRemitted$comp2 = scoreRemitted[,2]
tb2faLossRemitted$comp3 = scoreRemitted[,3]

write.csv(tb2faGainRemitted,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_Remitted_10112018.csv")
write.csv(tb2faLossRemitted,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_Remitted_10112018.csv")


# Exploratory factor analysis ------------------------------------------------------

# determine the number of factors to extract
ev <- eigen(cor(zscored)) # get eigenvalues
ap <- parallel(subject=nrow(array2fa),var=ncol(array2fa),
               rep=100,cent=.05)
# nScree an analysis of the number of component or factors to retain
# in anexploratory principal component or factor analysis.
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, cor = TRUE, model="factors") 

plotnScree(nS)

nCNG = nCng(ev$values, cor=TRUE, model = "factors", details = TRUE)
# Plot of the Usual Cattell's Scree Test
plotuScree(ev$values, main=paste(nCNG$nFactors, 
                                 'factors retained by the CNG procedure', 
                                 sel = ""))

# Factor Analysis
# main reference: https://www.statmethods.net/advstats/factor.html

# with varimax rotation
# factanal uses maximum likelihood method
#faresult <- factanal(zscored, factor=3, rotation="varimax") # orthogonal rotation
#print(faresult, digits=2, cutoff=.3, sort=TRUE)

faresult <- fa(zscored, nfactors=3, rotate="oblimin", impute = "mean",
               fm="minres") # oblimin one of the oblique rotation

faresult <- fa(zscored, nfactors=3, rotate="oblimin", impute = "mean",
               fm="ml") # oblimin one of the oblique rotation

faresult <- fa(zscored, nfactors=3, rotate="oblimin", impute = "mean",
               fm="ols") # oblimin one of the oblique rotation

faresult <- fa(zscored, nfactors=3, rotate="varimax", impute = "mean",
               fm="minres") # orthogonal rotation

faresult <- fa(zscored, nfactors=1, rotate="oblimin", impute = "mean",
               fm="minres") # oblimin one of the oblique rotation



faresult <- fa(zscored, nfactors=3, rotate="varimax", impute = "mean") # orthogonal rotation

print(faresult, digits=2, cutoff=.3, sort=TRUE)
fadia <- fa.diagram(faresult, cut=0.3, digits = 2)
fa.graph(faresult)
fa.plot(faresult)

varexplained <- faresult$Vaccounted
plot(varexplained[5,], type="b",ylab='Variance explained')


# plot factor 1 by factor 2 
load <- faresult$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(array2fa),cex=.7) # add variable names

# plot loadings for each factor 
load <- faresult$loadings[,1:3]
barplot(load[,1], main="Factor 1", ylab="Loadings", las=2)
barplot(load[,2], main="Factor 2", ylab="Loadings", las=2)
barplot(load[,3], main="Factor 3", ylab="Loadings", las=2)

# weights
weight = faresult$weights

# factor score
score = faresult$scores
  
# factor score correlation
score.cor = faresult$score.cor
score.cor2 = faresult$r.scores

# Save factor scores ----------------------------------------------------------- 
# clinical-behavior analysis with factor scores

# check if subject id matches
temp <- cbind(tb2faGain$id, tb2faLoss$id)

# combine factor into data
tb2faGain$factor1 = score[,1]
tb2faGain$factor2 = score[,2]
tb2faGain$factor3 = score[,3]

tb2faLoss$factor1 = score[,1]
tb2faLoss$factor2 = score[,2]
tb2faLoss$factor3 = score[,3]

write.csv(tb2faGain,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score gain_allSubj_orthogonal_10102018.csv")
write.csv(tb2faLoss,"D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score loss_allSubj_orthogonal_10102018.csv")
names(tb2faGain)



