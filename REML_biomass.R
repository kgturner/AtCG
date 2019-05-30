#REML models of biomass
#8/27/2018

#using lme4 v1.1-18-1
#needs R 3.2+, using R v3.4.0
#needs Matrix 1.2-1, using Matrix v1.2-10
# update.packages("Matrix")
#need to reinstall RccpEigen and minqua to avoid seg fault
install.packages("minqa", "RcppEigen")
#new version lme4
install.packages("lme4")

library("lme4")

#data
fieldData <- read.csv("FieldDataWithDiversityInfo_14May19.csv")#using data from excel file 3_21_19
lineData <- read.csv("chosenLines.csv")
damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt") #census data based on photos from 6/12/2018

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


# this needs rethinking
# # fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)
# # fec_freq <- as.data.frame(table(fecundityData$Pot.Number))
# # colnames(fec_freq)[1] <- "PotID"
# # survival_prelim <- merge(subset(fieldData, select=c(PotID, PlantNum_Initial,FH_Num)), fec_freq, all.x=T)
# # colnames(survival_prelim)[4] <- "FecundityNum"
# # survival_prelim[is.na(survival_prelim$FecundityNum),]$FecundityNum <- 0
# # survival_prelim$SurvivalNum <- survival_prelim$FH_Num + survival_prelim$FecundityNum
# # survival_prelim$Mortality <- survival_prelim$PlantNum_Initial - survival_prelim$SurvivalNum
# # survival_prelim$Damaged_pot <- NA
# # survival_prelim[survival_prelim$PotID %in% damaged_pot,]$Damaged_pot <- "damaged"
# # summary(survival_prelim)
# # subset(survival_prelim, Mortality<0)
prelimSurv <- subset(prelimSurv, select = c(1:3,7:10))
prelimSurv$totMinusGerm <- prelimSurv$X6_12_total_num - prelimSurv$X6_12_late_germ_num
prelimSurv$MaxPlantNum <- pmax(prelimSurv$PlantNum_Initial, prelimSurv$totMinusGerm)
# write.table(survival_prelim, "PrelimSurvivalData_20190321.txt", quote=F, sep="\t")

####merging for modeldataset####
#merge plant num data
modeldata <- merge.data.frame(fieldData, prelimSurv, by="PotID")
#also remove non-NA plots that were trampled/dug up/washed out
modeldata <- subset(modeldata, !(PotID %in% damaged_pot)) #removed cow/dog/flood damaged pots
hist(modeldata$MaxPlantNum, breaks = 20)

####modeling dataset for biomass####
#dataset for modeling
modeldata<-fieldData[!is.na(fieldData$FH_Wt),]


# modeldata <- subset(modeldata, PlantNum_Initial >12) #exclude plots with fewer than 12 plants (out of 20)? Check if this changes results does not change results
#modeldata$testRand <- as.factor(rep("A", times=nrow(modeldata))) #for testing random effects...?
summary(modeldata)
# subset(modeldata, GermRating =="NoGerm") #pot 74, no germ, yet PlantNum_Initial = 18, had canopy and FH_Wt
# modeldata$perPlantFH_Wt <- modeldata$FH_Wt/modeldata$PlantNum_Initial

#####Biomass ~ diversity level models####
#example full model
#biomass/density after transplant ~ 
#     diversity level * stress trt +  
#     plot average climate pca distance from PSU(PC5dist_mean) + FT1001_mean + SLApred
#     (1|MaxPlantNum) + (1| block)

#div level models
model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+(1|stripNo), data=modeldata)
model2<-lmer(perPlantFH_Wt ~ divLevel+trt+PC5dist_mean+(1|stripNo), data=modeldata)
model3<-lmer(perPlantFH_Wt ~ divLevel+trt+(1|stripNo), data=modeldata)
model4<-lmer(perPlantFH_Wt ~ trt+(1|stripNo), data=modeldata)
model5<-lmer(perPlantFH_Wt ~ divLevel+(1|stripNo), data=modeldata)
model6<-lmer(perPlantFH_Wt ~ trt+(1|stripNo)+(1|GermRating), data=modeldata)
# model7<-lmer(perPlantFH_Wt ~ trt+(1|stripNo)+(1|PlantNum_Initial), data=modeldata)
model8<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata) ####
model9<-lmer(perPlantFH_Wt ~ trt+FT1001plast_mean+(1|stripNo), data=modeldata)
model10<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata)##
model11<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+FT1001plast_mean+(1|stripNo), data=modeldata)
model12 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
# model13 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | PlantNum_Initial), data=modeldata)
model14 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+ (1 | GermRating), data=modeldata)

model15 <- glm(perPlantFH_Wt ~ divLevel+trt+FT1001_mean, data=modeldata)
model16 <- glm(perPlantFH_Wt ~ trt+FT1001_mean, data=modeldata)

###
#these results not currently in ms
###

(a1 <- anova(model2,model1)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model2: perPlantFH_Wt ~ divLevel + trt + PC5dist_mean + (1 | stripNo)
# model1: perPlantFH_Wt ~ divLevel * trt + PC5dist_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2  6 -2290.1 -2266.5 1151.0  -2302.1                         
# model1  7 -2288.2 -2260.7 1151.1  -2302.2 0.1058      1      0.745
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? marginal!
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model3: perPlantFH_Wt ~ divLevel + trt + (1 | stripNo)
# model2: perPlantFH_Wt ~ divLevel + trt + PC5dist_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model3  5 -2289.2 -2269.6 1149.6  -2299.2                           
# model2  6 -2290.1 -2266.5 1151.0  -2302.1 2.9102      1    0.08802 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a3 <- anova(model4, model3)) #is divLevel sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: perPlantFH_Wt ~ trt + (1 | stripNo)
# model3: perPlantFH_Wt ~ divLevel + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4  4 -2290.7 -2275.0 1149.3  -2298.7                         
# model3  5 -2289.2 -2269.6 1149.6  -2299.2 0.4915      1     0.4832
(a4 <- anova(model5, model3)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5: perPlantFH_Wt ~ divLevel + (1 | stripNo)
# model3: perPlantFH_Wt ~ divLevel + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model5  4 -2283.0 -2267.3 1145.5  -2291.0                            
# model3  5 -2289.2 -2269.6 1149.6  -2299.2 8.2118      1   0.004162 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5 <- anova(model4, model6)) #is germ rating sig? no ### see model comparisons including FT1001_mean
# (a6 <- anova(model4, model7)) #is initial plant number sig? no #do not include initial plant number if using perPlantFH_Wt
(a7 <- anova(model4, model8)) #is FT1001 mean sig? yes!
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: perPlantFH_Wt ~ trt + (1 | stripNo)
# model8: perPlantFH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model4  4 -2290.7 -2275.0 1149.3  -2298.7                             
# model8  5 -2304.8 -2285.2 1157.4  -2314.8 16.097      1  6.017e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a8 <- anova(model4, model9)) #is FT1001plasticity sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: perPlantFH_Wt ~ trt + (1 | stripNo)
# model9: perPlantFH_Wt ~ trt + FT1001plast_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model4  4 -2290.7 -2275.0 1149.3  -2298.7                           
# model9  5 -2295.0 -2275.3 1152.5  -2305.0 6.2561      1    0.01238 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? no
(a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? no
(a11 <- anova(model10, model12)) #is germ rating sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model10: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# model12: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo) + 
#   model12:     (1 | GermRating)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# model10  6 -2303.5 -2279.9 1157.7  -2315.5                        
# model12  7 -2301.5 -2274.0 1157.7  -2315.5     0      1          1
# (a12 <- anova(model10, model13)) #is plant number sig? no #don't include initial plant number if using perPlantFH_Wt
(a13 <- anova(model14, model12)) #is block number sig? no (but currently in all the models....?)
(a14 <- anova(model16, model15)) #is div level sig with no random effects? no
# Analysis of Deviance Table
# 
# Model 1: perPlantFH_Wt ~ trt + FT1001_mean
# Model 2: perPlantFH_Wt ~ divLevel + trt + FT1001_mean
# Resid. Df Resid. Dev Df   Deviance
# 1       373   0.046714              
# 2       372   0.046620  1 9.4072e-05




####biomass ~ genetic kinship models####
#example full models
#biomass ~ plot average genetic distance (meanKin) * stress trt * climate pca(PC5dist_mean) + density after transplant + (1| block)
#include Germ rating?

model1_gen<-lmer(FH_Wt ~ meanKin*trt+PC5dist_mean+(1|GermRating)+(1|stripNo), data=modeldata)
model2_gen<-lmer(FH_Wt ~ meanKin+trt+PC5dist_mean+(1|GermRating)+(1|stripNo), data=modeldata)
model3_gen<-lmer(FH_Wt ~ meanKin+trt+(1|GermRating)+(1|stripNo), data=modeldata)
model4_gen<-lmer(FH_Wt ~ trt+(1|GermRating)+(1|stripNo), data=modeldata)
model5_gen<-lmer(FH_Wt ~ meanKin+(1|GermRating)+(1|stripNo), data=modeldata)
model6_gen<-lmer(FH_Wt ~ meanKin+trt+FT1001_mean+(1|GermRating)+(1|stripNo), data=modeldata)
model7_gen<-lmer(FH_Wt ~ meanKin+trt+FT1001_mean+(1|stripNo), data=modeldata)
model8_gen<-lmer(FH_Wt ~ meanKin+trt+FT1001_mean+(1|GermRating), data=modeldata)

(a1_gen <- anova(model2_gen,model1_gen)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model2_gen: FH_Wt ~ meanKin + trt + PC5dist_mean + (1 | GermRating) + (1 | 
#                                                                            model2_gen:     stripNo)
# model1_gen: FH_Wt ~ meanKin * trt + PC5dist_mean + (1 | GermRating) + (1 | 
#                                                                          model1_gen:     stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2_gen  7 -117.56 -89.346 65.781  -131.56                         
# model1_gen  8 -115.66 -83.412 65.829  -131.66 0.0968      1     0.7557
(a2_gen <- anova(model3_gen,model2_gen)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model3_gen: FH_Wt ~ meanKin + trt + (1 | GermRating) + (1 | stripNo)
# model2_gen: FH_Wt ~ meanKin + trt + PC5dist_mean + (1 | GermRating) + (1 | 
#                                                                          model2_gen:     stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# model3_gen  6 -117.34 -93.155 64.669  -129.34                        
# model2_gen  7 -117.56 -89.346 65.781  -131.56 2.222      1     0.1361
(a3_gen <- anova(model4_gen, model3_gen)) #is meanKin sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4_gen: FH_Wt ~ trt + (1 | GermRating) + (1 | stripNo)
# model3_gen: FH_Wt ~ meanKin + trt + (1 | GermRating) + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4_gen  5 -117.94 -97.783 63.968  -127.94                         
# model3_gen  6 -117.34 -93.155 64.669  -129.34 1.4026      1     0.2363
(a4_gen <- anova(model5_gen, model3_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5_gen: FH_Wt ~ meanKin + (1 | GermRating) + (1 | stripNo)
# model3_gen: FH_Wt ~ meanKin + trt + (1 | GermRating) + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5_gen  5 -107.70 -87.549 58.851  -117.70                             
# model3_gen  6 -117.34 -93.155 64.669  -129.34 11.637      1  0.0006467 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5_gen <- anova(model3_gen, model6_gen)) #is FT1001_mean sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model3_gen: FH_Wt ~ meanKin + trt + (1 | GermRating) + (1 | stripNo)
# model6_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | GermRating) + (1 | 
#                                                                         model6_gen:     stripNo)
# Df     AIC      BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model3_gen  6 -117.34  -93.155 64.669  -129.34                             
# model6_gen  7 -132.62 -104.409 73.312  -146.62 17.285      1  3.217e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_gen <- anova(model7_gen, model6_gen)) #is germ rating sig? marginal
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model7_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | stripNo)
# model6_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | GermRating) + (1 | 
#                                                                         model6_gen:     stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model7_gen  6 -131.91 -107.73 71.955  -143.91                           
# model6_gen  7 -132.62 -104.41 73.312  -146.62 2.7143      1    0.09945 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a7_gen <- anova(model8_gen, model6_gen)) #is strip no sig?


###
#compare meanKin and divlevel
(ax <- anova(model12, model6_gen)) 
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model12: FH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo) + (1 | GermRating)
# model6_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | GermRating) + (1 | 
#                                                                         model6_gen:     stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model12     7 -131.21 -102.99 72.602  -145.21                             
# model6_gen  7 -132.62 -104.41 73.312  -146.62 1.4192      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# dchisq(X,df) #where X is the Chisq value from the anova table (37.085 in your case) 
#and df is Df in the anova table +1 (0 + 1 in your case)
dchisq(1.4192,1) #result is p-value
# [1] 0.1647073 so div level and meanKin are not different

#pred vs div
model9_gen<-lmer(FH_Wt ~ trt+FT1001_mean+plotType+(1|stripNo) + (1 | GermRating), data=modeldata)
model10_gen<-lmer(FH_Wt ~ trt+FT1001_mean+(1|stripNo) + (1 | GermRating), data=modeldata)

(a8_gen <- anova(model10_gen, model9_gen)) #plot type? no


####biomass ~ tree div models####
#example full models
#biomass/density after transplant ~ plot genetic distance (treeDiv) * stress trt * climate pca(PC5dist_mean) +  (1| block)
#include Germ rating?

#tree distance models
model1<-lmer(perPlantFH_Wt ~ treeDiv*trt+PC5dist_mean+(1|stripNo), data=modeldata)
model2<-lmer(perPlantFH_Wt ~ treeDiv+trt+PC5dist_mean+(1|stripNo), data=modeldata)
model3<-lmer(perPlantFH_Wt ~ treeDiv+trt+(1|stripNo), data=modeldata)
model4<-lmer(perPlantFH_Wt ~ trt+(1|stripNo), data=modeldata)
model5<-lmer(perPlantFH_Wt ~ treeDiv+(1|stripNo), data=modeldata)
model6<-lmer(perPlantFH_Wt ~ trt+(1|stripNo)+(1|GermRating), data=modeldata)

model8<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata) ####
model9<-lmer(perPlantFH_Wt ~ trt+FT1001plast_mean+(1|stripNo), data=modeldata)
model10<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+(1|stripNo), data=modeldata)##
model11<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+FT1001plast_mean+(1|stripNo), data=modeldata)
model12 <- lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)

model14 <- lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+ (1 | GermRating), data=modeldata)

model15 <- glm(perPlantFH_Wt ~ divLevel+trt+FT1001_mean, data=modeldata)
model16 <- glm(perPlantFH_Wt ~ trt+FT1001_mean, data=modeldata)

###
#these results not currently in ms
###

(a1 <- anova(model2,model1)) # is interaction sig? 

(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? 

(a3 <- anova(model4, model3)) #is divLevel sig? 

(a4 <- anova(model5, model3)) #is trt sig? 

(a5 <- anova(model4, model6)) #is germ rating sig? 

(a7 <- anova(model4, model8)) #is FT1001 mean sig? 

(a8 <- anova(model4, model9)) #is FT1001plasticity sig? 

(a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? 

(a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? 

(a11 <- anova(model10, model12)) #is germ rating sig? 

(a13 <- anova(model14, model12)) #is block number sig? 
(a14 <- anova(model16, model15)) #is div level sig with no random effects? 

####canopy area ~ div level models####
#dataset for modeling
modeldata<-fieldData[!is.na(fieldData$CanopyArea),]

#example full models
#CanopyArea ~ diversity level * stress trt *  plot average climate pca distance from PSU(PC5dist_mean) + density after transplant + (1| block)
#CanopyArea ~ plot average genetic distance (meanKin) * stress trt * climate pca(PC5dist_mean) + density after transplant + (1| block)

#div level models
model1<-lmer(CanopyArea ~ divLevel*trt+PC5dist_mean+(1|stripNo) + (1 | GermRating), data=modeldata)
model2<-lmer(CanopyArea ~ divLevel+trt+PC5dist_mean+(1|stripNo) + (1 | GermRating), data=modeldata)
model3<-lmer(CanopyArea ~ divLevel+trt+(1|stripNo) + (1 | GermRating), data=modeldata)
model4<-lmer(CanopyArea ~ trt+(1|stripNo) + (1 | GermRating), data=modeldata)
model4a <- lmer(CanopyArea ~ trt+(1|stripNo) + (1|PlantNum_Initial), data=modeldata)
model5<-lmer(CanopyArea ~ divLevel+(1|stripNo) + (1 | GermRating), data=modeldata)
model6<-lmer(CanopyArea ~ trt+(1|stripNo), data=modeldata)
model7<-lmer(CanopyArea ~ trt+(1|stripNo)+ (1 | GermRating) + (1|PlantNum_Initial), data=modeldata)

model8<-lmer(CanopyArea ~ trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata) ####
model9<-lmer(CanopyArea ~ trt+FT1001plast_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
model10<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)##
model11<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+FT1001plast_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)

(a1 <- anova(model2,model1)) # is interaction sig? no
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
(a3 <- anova(model4, model3)) #is divLevel sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: CanopyArea ~ trt + (1 | stripNo) + (1 | GermRating)
# model3: CanopyArea ~ divLevel + trt + (1 | stripNo) + (1 | GermRating)
#         Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4  5 2899.2 2919.7 -1444.6   2889.2                         
# model3  6 2901.1 2925.8 -1444.6   2889.1 0.0241      1     0.8766
(a4 <- anova(model5, model3)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5: CanopyArea ~ divLevel + (1 | stripNo) + (1 | GermRating)
# model3: CanopyArea ~ divLevel + trt + (1 | stripNo) + (1 | GermRating)
#         Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5  5 3007.8 3028.4 -1498.9   2997.8                             
# model3  6 2901.1 2925.8 -1444.6   2889.1 108.68      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5 <- anova(model4, model6)) #is germ rating sig? yes ###
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model6: CanopyArea ~ trt + (1 | stripNo)
# model4: CanopyArea ~ trt + (1 | stripNo) + (1 | GermRating)
#         Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model6  4 2911.6 2928.0 -1451.8   2903.6                             
# model4  5 2899.2 2919.7 -1444.6   2889.2 14.403      1  0.0001476 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6 <- anova(model4, model7)) #is initial plant number sig? no
(a6a <- anova(model4a, model4)) #is germ better than plant num? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4a: CanopyArea ~ trt + (1 | stripNo) + (1 | PlantNum_Initial)
# model4: CanopyArea ~ trt + (1 | stripNo) + (1 | GermRating)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model4a  5 2909.3 2929.9 -1449.7   2899.3                             
# model4   5 2899.2 2919.7 -1444.6   2889.2 10.162      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a7 <- anova(model4, model8)) #is FT1001 mean sig? no
(a8 <- anova(model4, model9)) #is FT1001plasticity sig? no
(a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? no
(a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? no





####canopy area ~ genetic distance models####
model1_gen<-lmer(CanopyArea ~ meanKin*trt+PC5dist_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
model2_gen<-lmer(CanopyArea ~ meanKin+trt+PC5dist_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
model3_gen<-lmer(CanopyArea ~ meanKin+trt+(1|stripNo)+ (1 | GermRating), data=modeldata)
model4_gen<-lmer(CanopyArea ~ trt+(1|stripNo)+ (1 | GermRating), data=modeldata)
model5_gen<-lmer(CanopyArea ~ meanKin+(1|stripNo)+ (1 | GermRating), data=modeldata)
model6_gen<-lmer(CanopyArea ~ meanKin+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
model7_gen<-lmer(CanopyArea ~ trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)

(a1_gen <- anova(model2_gen,model1_gen)) # is interaction sig? no
(a2_gen <- anova(model3_gen,model2_gen)) # is PC5dist_mean covariate sig? no
(a3_gen <- anova(model4_gen, model3_gen)) #is meanKin sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4_gen: CanopyArea ~ trt + (1 | stripNo) + (1 | GermRating)
# model3_gen: CanopyArea ~ meanKin + trt + (1 | stripNo) + (1 | GermRating)
#             Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4_gen  5 2899.2 2919.7 -1444.6   2889.2                         
# model3_gen  6 2901.2 2925.8 -1444.6   2889.2 0.0034      1     0.9535
(a4_gen <- anova(model5_gen, model3_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5_gen: CanopyArea ~ meanKin + (1 | stripNo) + (1 | GermRating)
# model3_gen: CanopyArea ~ meanKin + trt + (1 | stripNo) + (1 | GermRating)
#             Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5_gen  5 3007.8 3028.3 -1498.9   2997.8                             
# model3_gen  6 2901.2 2925.8 -1444.6   2889.2 108.62      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5_gen <- anova(model3_gen, model6_gen)) #is FT1001_mean sig? no
(a6_gen <- anova(model7_gen, model6_gen)) #is meanKin sig with FT1001_mean?

#compare meanKin and divlevel
(ax <- anova(model3_gen,model3)) #are kin and div different?
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model3_gen: CanopyArea ~ meanKin + trt + (1 | stripNo) + (1 | GermRating)
# model3: CanopyArea ~ divLevel + trt + (1 | stripNo) + (1 | GermRating)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model3_gen  6 2901.2 2925.8 -1444.6   2889.2                             
# model3      6 2901.1 2925.8 -1444.6   2889.1 0.0207      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



####canopy area ~ tree div models####
####fecundity ~ div level models####
fec_freq <- as.data.frame(table(fecundityData$Pot.Number)) #Var1 = Pot ID, Freq = # of plants harvested early w/ fruit

#example full model
#(sum of seed number estimate by plot)/(fecundity frequncy + FH_Num)
#(plant num initial - fecundity frequncy - FH_Num) = mortality
#(fecundity frequncy + FH_Num) = survival


#####model expected genotype number in polyculture from monocultures - use instead of divLevel?####
#code from Jesse 11/27/2018
# fieldD <- read.csv('~/Dropbox/jesse/Arabidopsis/BEF/FieldDataWithDiversityInfo.csv', as.is = T)
fieldD <- read.csv("FieldDataWithDiversityInfo.csv", as.is = T)
isurvC <- tapply(fieldD$PlantNum_Initial[fieldD$plotType == 'mono' & fieldD$trt == 'C'], fieldD$lines1[fieldD$plotType == 'mono' & fieldD$trt == 'C'], mean, na.rm = T)

isurvS <- tapply(fieldD$PlantNum_Initial[fieldD$plotType == 'mono' & fieldD$trt == 'S'], fieldD$lines1[fieldD$plotType == 'mono' & fieldD$trt == 'S'], mean, na.rm = T)


#get expected biomass for each plot, accounting for different survival rates
RandExpBiomPerPlant <- matrix(NA, nrow = nrow(fieldD), ncol = 100)


for(i in 1:nrow(fieldD)){
  for(j in 1:100){
    
    if(fieldD$plotType[i] != 'mono'){
      
      if(fieldD$trt[i] == 'C'){
        expg <-     names(isurvC)[names(isurvC) %in% fieldD[i,paste0('lines', 1:fieldD$divLevel[i])]]
        expg <- rep(expg, 20/fieldD$divLevel[i])
        #make random comm.
        newcomm <- sample(expg, fieldD$PlantNum_Initial[i], prob = isurvC[expg])
        
        RandExpBiomPerPlant[i,j] <- mean(biomC[newcomm],na.rm = T)
      }
      
      if(fieldD$trt[i] == 'S'){
        expg <-     names(isurvS)[names(isurvS) %in% fieldD[i,paste0('lines', 1:fieldD$divLevel[i])]]
        expg <- rep(expg, 20/fieldD$divLevel[i])
        #make random comm.
        newcomm <- sample(expg, fieldD$PlantNum_Initial[i], prob = isurvS[expg])
        
        RandExpBiomPerPlant[i,j] <- mean(biomS[newcomm],na.rm = T)
      }
      
    }
  }
}

length(unique(newcomm)) #gives you expected number of genotypes in polyculture based on survival probability

########example using lme4.0#####
exprs.LR<- function(trait,df,cov, family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin*Trt+modeldata[[cov]]+ (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin+Trt+ modeldata[[cov]] + (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model3<-lmer(modeldata[[trait]]  ~ Origin+Trt + (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model4 <- lmer(modeldata[[trait]]  ~ Trt+ modeldata[[cov]]+(Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  
  a1 <- anova(model2,model1) # is interaction sig?
  a2 <- anova(model3,model2) # is covariate sig?
  a3 <- anova(model4, model2) #is origin sig?
  
  pval <- as.data.frame(cbind(Contig=trait,intLRT=a1[[7]][2],covLRT=a2[[7]][2],originLRT=a3[[7]][2]))
  
  return(pval)
}

#test!
test <- read.table("test_lme4dat.txt", header=T, sep="\t") 

test <- exprs.df[, c(1:20)]
test.LRT <- do.call(rbind,lapply(names(test)[15:20],function(n) exprs.LR(n,df=test,cov="Latitude")))#apply func to all things in list
