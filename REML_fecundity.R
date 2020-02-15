#REML models of fecundity
#7/23/2018

#using lme4 v1.1-18-1
#needs R 3.2+, using R v3.4.0
#needs Matrix 1.2-1, using Matrix v1.2-10
# update.packages("Matrix")
#need to reinstall RccpEigen and minqua to avoid seg fault
install.packages("minqa", "RcppEigen")
#new version lme4
install.packages("lme4")

library("lme4")
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
fieldData <- read.csv("FieldDataWithDiversityInfo_14May19.csv", na.strings=c(""," ","NA"))#using data from excel file 3_21_19
lineData <- read.csv("chosenLines.csv", na.strings=c(""," ","NA"))
damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt", na.strings=c(""," ","NA")) #census data based on photos from 6/12/2018
SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))
fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)

#column explanations
#PCs5_distPSU - climate distance from PSU for that line
# 'tmean_4' is meanApril temp
# ''Mean.Diurn.Rng'is mean diurnal temp range
# 'MinTmpCldMo' is minimum temp of coldest month
# 'tmin_meangro' is mean monthly min temp during growing season
# 'Prc.Seas' is CV of monthly precip
# 'tmax_meangro' is mean monthly max temp during growing season
# gs.AI is the ratio of precip to PET (potential evapotranspiration) in the growing season.
# interAnn_pvar_meangro is the interannual variation in precipitation in the growing season (CV)
# 'groseason' is the growing season length
# 'cv_prec_meangro' is the CV (coefficient of variation) of monthly precip in the growing season
# FT1001_mean - mean flowerint time from 1001 genomes paper
# FT1001_var - variance
# meanKin (how calculated?) - mean knship among plants in a plot (1001 genomes kinship matrix
# treeDiv - different way to calculated relatedness
# FT1001plast_mean - mean flowering time plasticity (reponse to 10 vs 16 C)
# FT1001plast_var - variance in plasticity 
# PlantNum_Initial - plants counted after 1 week in field (i.e. after transplant mortality)
# GermRating - categorical assesment of germ 1 week (?) before transplant to field
# Seed_Num_estimate - total silique # X silique length
# X1-X27 - kinship eigenvectors (snp pca). THey run sequentially from PC1-PC9, with 3 columns each, which are in order the 0.1, 0.5, and 0.9 quantile. 
# Seed_Num_estimate = Total.Slique.Number X Slique.Length..mm.

# this needs rethinking
# # fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)
# # fec_freq <- as.data.frame(table(fecundityData$Pot.Number))
# # colnames(fec_freq)[1] <- "PotID"

####merging for modeldataset####
#created in REML_biomass.R
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA"))

###################################################
####modeling dataset for fecundity####
#dataset for modeling
library(plyr)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))
modeldata_f <- merge.data.frame(modeldata, seed_mean, by.x="PotID", by.y="Pot.Number", all.x=TRUE)

modeldata_f<-modeldata_f[!is.na(modeldata_f$seedEstMean),] #lose 18 damaged pots
summary(modeldata_f)

summary(seed_mean$Pot.Number %!in% modeldata_f$PotID)

## fec_freq <- as.data.frame(table(fecundityData$Pot.Number)) #Var1 = Pot ID, Freq = # of plants harvested early w/ fruit
# (sum of seed number estimate by plot)/(fecundity frequncy + FH_Num)
# (plant num initial - fecundity frequncy - FH_Num) = mortality
# (fecundity frequncy + FH_Num) = survival


####fecundity ~ div level models####
#example full model
# total seed number estimate in plot (i.e. plot avg of number of siliques on each plant x silique length on each plant) ~
#   divLevel X trt + PC5 +FT1001 + MaxPlantNum (OR # of plants measured for fec?)

# model1<-lmer(seedEstMean ~ divLevel*trt+MaxPlantNum + PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)
# model1.1<-lmer(seedEstMean ~ divLevel*trt+numFecPlants + PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)
# (a0 <- anova(model1,model1.1)) # is maxnumplants better than numfecplants?
# # refitting model(s) with ML (instead of REML)
# # Data: modeldata_f
# # Models:
# #   model1: seedEstMean ~ divLevel * trt + MaxPlantNum + PC5dist_mean + FT1001_mean + 
# #   model1:     mean_SLAbv + (1 | stripNo)
# # model1.1: seedEstMean ~ divLevel * trt + numFecPlants + PC5dist_mean + 
# #   model1.1:     FT1001_mean + mean_SLAbv + (1 | stripNo)
# # Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# # model1   10 2082.7 2114.6 -1031.3   2062.7                             
# # model1.1 10 2074.8 2106.7 -1027.4   2054.8 7.9006      0  < 2.2e-16 ***
# #   ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # dchisq(X,df) #where X is the Chisq value from the anova table (37.085 in your case) 
# #and df is Df in the anova table +1 (0 + 1 in your case)
# dchisq(7.9006,1) #result is p-value
# # [1] 0.2078941 so MaxPlantNum and numFecPlants are different. model1.1 (which uses numfecplants) has lower AIC value


model1.1<-lmer(seedEstMean ~ divLevel*trt+numFecPlants + PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)

model2<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)
model3<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)

modeldata_f2 <- modeldata_f[!is.na(modeldata_f$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f2)
model4.1<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+FT1001_mean+(1|stripNo), data=modeldata_f2)

model4<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+FT1001_mean+(1|stripNo), data=modeldata_f)
model5<-lmer(seedEstMean ~ divLevel+trt+numFecPlants+(1|stripNo), data=modeldata_f)
model6<-lmer(seedEstMean ~ divLevel+numFecPlants +(1|stripNo), data=modeldata_f)
model7<-lmer(seedEstMean ~ divLevel+trt+(1|stripNo), data=modeldata_f)
model8 <- lmer(seedEstMean ~ trt+numFecPlants+(1|stripNo), data=modeldata_f)



(a1 <- anova(model2,model1.1)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model2: seedEstMean ~ divLevel + trt + numFecPlants + PC5dist_mean + 
#   model2:     FT1001_mean + mean_SLAbv + (1 | stripNo)
# model1.1: seedEstMean ~ divLevel * trt + numFecPlants + PC5dist_mean + 
#   model1.1:     FT1001_mean + mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2    9 2073.8 2102.5 -1027.9   2055.8                         
# model1.1 10 2074.8 2106.7 -1027.4   2054.8 0.9767      1      0.323
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model3: seedEstMean ~ divLevel + trt + numFecPlants + FT1001_mean + mean_SLAbv + 
#   model3:     (1 | stripNo)
# model2: seedEstMean ~ divLevel + trt + numFecPlants + PC5dist_mean + 
#   model2:     FT1001_mean + mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model3  8 2072.4 2097.9 -1028.2   2056.4                         
# model2  9 2073.8 2102.5 -1027.9   2055.8 0.6452      1     0.4218
(a3 <- anova(model4.1, model3.1)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f2
# Models:
#   model4.1: seedEstMean ~ divLevel + trt + numFecPlants + FT1001_mean + (1 | 
#                                                                            model4.1:     stripNo)
# model3.1: seedEstMean ~ divLevel + trt + numFecPlants + FT1001_mean + mean_SLAbv + 
#   model3.1:     (1 | stripNo)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# model4.1  7 2071.0 2093.3 -1028.5   2057.0                        
# model3.1  8 2072.4 2097.9 -1028.2   2056.4 0.575      1     0.4483
(a4 <- anova(model5, model4)) #is FT1001 mean sig? marginal
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model5: seedEstMean ~ divLevel + trt + numFecPlants + (1 | stripNo)
# model4: seedEstMean ~ divLevel + trt + numFecPlants + FT1001_mean + (1 | 
#                                                                        model4:     stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model5  6 2306.3 2326.1 -1147.1   2294.3                           
# model4  7 2305.1 2328.2 -1145.5   2291.1 3.1875      1     0.0742 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5 <- anova(model6, model5)) #is trt sig?  yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model6: seedEstMean ~ divLevel + numFecPlants + (1 | stripNo)
# model5: seedEstMean ~ divLevel + trt + numFecPlants + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model6  5 2309.8 2326.4 -1149.9   2299.8                           
# model5  6 2306.3 2326.1 -1147.1   2294.3 5.5637      1    0.01834 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a7 <- anova(model7, model5)) #is numFecPlants sig? yes!
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model7: seedEstMean ~ divLevel + trt + (1 | stripNo)
# model5: seedEstMean ~ divLevel + trt + numFecPlants + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model7  5 2312.8 2329.4 -1151.4   2302.8                            
# model5  6 2306.3 2326.1 -1147.1   2294.3 8.5668      1   0.003423 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a8 <- anova(model8, model5)) #is divLevel sig? yes!
# refitting model(s) with ML (instead of REML)
# Data: modeldata_f
# Models:
#   model8: seedEstMean ~ trt + numFecPlants + (1 | stripNo)
# model5: seedEstMean ~ divLevel + trt + numFecPlants + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model8  5 2314.0 2330.5 -1152.0   2304.0                            
# model5  6 2306.3 2326.1 -1147.1   2294.3 9.6949      1   0.001848 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(model5)
# Linear mixed model fit by REML ['lmerMod']
# Formula: seedEstMean ~ divLevel + trt + numFecPlants + (1 | stripNo)
# Data: modeldata_f
# 
# REML criterion at convergence: 2279.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.3343 -0.5783 -0.2022  0.2167  6.0560 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# stripNo  (Intercept)  160.4   12.67   
# Residual             5022.2   70.87   
# Number of obs: 202, groups:  stripNo, 4
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)   48.0836    14.8817   3.231
# divLevel       2.5522     0.7924   3.221
# trtS         -23.4595    10.1001  -2.323
# numFecPlants   2.6806     0.8994   2.980
# 
# Correlation of Fixed Effects:
#   (Intr) divLvl trtS  
# divLevel    -0.541              
# trtS        -0.389  0.062       
# numFecPlnts -0.734  0.418  0.091