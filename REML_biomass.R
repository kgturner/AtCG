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
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA")) #see construction below. Made from:
# fieldData <- read.csv("FieldDataWithDiversityInfo_2Aug19.csv", na.strings=c(""," ","NA"))
# lineData <- read.csv("chosenLines.csv", na.strings=c(""," ","NA"))
# # damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
# prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt", na.strings=c(""," ","NA")) #census data based on photos from 6/12/2018
# SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))

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
# modeldata <- merge.data.frame(fieldData, prelimSurv[,c(1,4:6,8:9)], by="PotID")#avoid duplicate columns
# #480 obvs of 87 var
# #also remove non-NA plots that were trampled/dug up/washed out
# modeldata <- subset(modeldata, Damaged_pot %!in% "damaged")
# modeldata <- subset(modeldata, select = c(1,3,5:31,35:37, 52:54, 68, 80:87))
# #429 obvs of 44 var
# ##modeldata <- subset(modeldata, !(PotID %in% damaged_pot), select = c(1,3,5:31,35:37, 52:54, 68:81)) #removed cow/dog/flood damaged pots
# # hist(modeldata$MaxPlantNum, breaks = 20)
# 
# #add SLA breeding values to model data set
# #for control
# md_c <- subset(modeldata, trt %in% "C" & plotType %!in% "mono", select = c(1, 3:25))
# md_c3 <- data.frame(PotID=md_c$PotID)
# md_c3$temp <- NA
# for(i in seq_along(md_c)[5:24]) {
#   md_c3$temp <- SLAbv[match(md_c[,i], SLAbv$lines),2] #col 2 is trtC
#   names(md_c3)[names(md_c3)=="temp"] <- paste0("SLAcol",i)
# }
# md_c3$mean_SLAbv <- rowMeans(md_c3[,2:21], na.rm=TRUE)
# 
# #for stress
# md_s <- subset(modeldata, trt %in% "S" & plotType %!in% "mono", select = c(1, 3:25))
# md_s3 <- data.frame(PotID=md_s$PotID)
# md_s3$temp <- NA
# for(i in seq_along(md_s)[5:24]) {
#   md_s3$temp <- SLAbv[match(md_s[,i], SLAbv$lines),3] #col 2 is trtC
#   names(md_s3)[names(md_s3)=="temp"] <- paste0("SLAcol",i)
# }
# md_s3$mean_SLAbv <- rowMeans(md_s3[,2:21], na.rm=TRUE)
# 
# #for mono
# library("tidyr")
# SLAbv2 <- SLAbv %>% gather(trtC, trtS, key = "trt", value = "mean_SLAbv")
# SLAbv2[SLAbv2$trt %in% "trtC",]$trt <- "C"
# SLAbv2[SLAbv2$trt %in% "trtS",]$trt <- "S"
# SLAbv2$trt <- as.factor(SLAbv2$trt)
# mono <- subset(modeldata, plotType %in% "mono")
# mono <- merge.data.frame(mono, SLAbv2, by.x=c("lines1", "trt"), by.y=c("lines","trt"), all = TRUE)
# #319 obvs of 45 var
# 
# #merge back!
# modeldata <- merge.data.frame(modeldata, mono, all.x=TRUE)
# #429 obs of 45 var
# # modeldata[11:20,c(1:6,48)] #to check
# modeldata[modeldata$PotID %in% md_c3$PotID,]$mean_SLAbv <- md_c3$mean_SLAbv
# modeldata[modeldata$PotID %in% md_s3$PotID,]$mean_SLAbv <- md_s3$mean_SLAbv
# 
# write.table(modeldata, "modeldata_20190828.txt", quote=F, sep="\t")
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA"))


###################################################
####modeling dataset for biomass####
#dataset for modeling
modeldata_m<-modeldata[!is.na(modeldata$FH_Wt),]
# modeldata <- subset(modeldata, PlantNum_Initial >12) #exclude plots with fewer than 12 plants (out of 20)? Check if this changes results does not change results
#modeldata$testRand <- as.factor(rep("A", times=nrow(modeldata))) #for testing random effects...?
summary(modeldata_m)
# subset(modeldata, GermRating =="NoGerm") #pot 74, no germ, yet PlantNum_Initial = 18, had canopy and FH_Wt


#####Biomass ~ diversity level models####
library("lme4")
#example full model
#biomass ~ 
#     diversity level * stress trt +  
#     plot average climate pca distance from PSU(PC5dist_mean) + FT1001_mean + SLApred +
#     (1|MaxPlantNum) + (1| block)

#issues with singular fit models when (1|MaxPlantNum) included on right side eq. as random effect
# model1<-lmer(FH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_m)
#use per plant harvest wt as response var instead?
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum

#div level models
model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)



model2<-lmer(perPlantFH_Wt ~ divLevel+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
model3<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)

modeldata_m2 <- modeldata_m[!is.na(modeldata_m$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m2)
model4.1<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata_m2)

model4<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata_m)
model5<-lmer(perPlantFH_Wt ~ divLevel+trt+(1|stripNo), data=modeldata_m)
model6<-lmer(perPlantFH_Wt ~ divLevel+FT1001_mean +(1|stripNo), data=modeldata_m)
model7<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata_m)
# model8<-lmer(FH_Wt ~ trt, data=modeldata_m)
# model8<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata_m)
# model9<-lmer(perPlantFH_Wt ~ trt+FT1001plast_mean+(1|stripNo), data=modeldata_m)
# model10<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata_m)##
# model11<-lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+FT1001plast_mean+(1|stripNo), data=modeldata_m)
# model12 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata_m)
# # model13 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | PlantNum_Initial), data=modeldata_m)
# model14 <- lmer(perPlantFH_Wt ~ divLevel+trt+FT1001_mean+ (1 | GermRating), data=modeldata_m)
# 
# model15 <- glm(perPlantFH_Wt ~ divLevel+trt+FT1001_mean, data=modeldata_m)
# model16 <- glm(perPlantFH_Wt ~ trt+FT1001_mean, data=modeldata_m)

###
#these results not currently in ms
###

(a1 <- anova(model2,model1)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model2: perPlantFH_Wt ~ divLevel + trt + PC5dist_mean + FT1001_mean + 
#   model2:     mean_SLAbv + (1 | stripNo)
# model1: perPlantFH_Wt ~ divLevel * trt + PC5dist_mean + FT1001_mean + 
#   model1:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2  8 -2308.8 -2277.3 1162.4  -2324.8                         
# model1  9 -2306.9 -2271.5 1162.5  -2324.9 0.1298      1     0.7186
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model3: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                          model3:     stripNo)
# model2: perPlantFH_Wt ~ divLevel + trt + PC5dist_mean + FT1001_mean + 
#   model2:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model3  7 -2309.1 -2281.5 1161.5  -2323.1                         
# model2  8 -2308.8 -2277.3 1162.4  -2324.8 1.7113      1     0.1908
(a3 <- anova(model4.1, model3.1)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m2
# Models:
#   model4.1: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# model3.1: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                          model3.1:     stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4.1  6 -2308.5 -2284.9 1160.2  -2320.5                         
# model3.1  7 -2309.1 -2281.5 1161.5  -2323.1 2.6012      1     0.1068
(a4 <- anova(model5, model4)) #is FT1001 mean sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model5: perPlantFH_Wt ~ divLevel + trt + (1 | stripNo)
# model4: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5  5 -2455.1 -2435.1 1232.6  -2465.1                             
# model4  6 -2468.2 -2444.2 1240.1  -2480.2 15.088      1  0.0001026 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5 <- anova(model6, model4)) #is trt sig?  yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model6: perPlantFH_Wt ~ divLevel + FT1001_mean + (1 | stripNo)
# model4: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model6  5 -2461.4 -2441.4 1235.7  -2471.4                            
# model4  6 -2468.2 -2444.2 1240.1  -2480.2 8.7828      1   0.003041 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a7 <- anova(model7, model4)) #is divLevel sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model7: perPlantFH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# model4: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7  5 -2469.8 -2449.8 1239.9  -2479.8                         
# model4  6 -2468.2 -2444.2 1240.1  -2480.2 0.4162      1     0.5188

# #other covars or models?
# # (a13 <- anova(model14, model12)) #is block number sig? no (but currently in all the models....?)
# # (a14 <- anova(model16, model15)) #is div level sig with no random effects? no
# (a8 <- anova(model4, model9)) #is FT1001plasticity sig? 
# (a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? no
# (a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? no
# (a11 <- anova(model10, model12)) #is germ rating sig? no
# # (a12 <- anova(model10, model13)) #is plant number sig? no #don't include initial plant number if using perPlantFH_Wt



####biomass ~ genetic kinship models####
#example full models
#biomass ~ plot average genetic distance (meanGenD) * stress trt * climate pca(PC5dist_mean) + density after transplant + (1| block)
#include Germ rating?

model1_gen<-lmer(perPlantFH_Wt ~ meanGenD*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
model2_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
model3_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)

modeldata_m2 <- modeldata_m[!is.na(modeldata_m$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m2)
model4.1_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+FT1001_mean+(1|stripNo), data=modeldata_m2)

model4_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+FT1001_mean+(1|stripNo), data=modeldata_m)
model5_gen<-lmer(perPlantFH_Wt ~ meanGenD+trt+(1|stripNo), data=modeldata_m)
model6_gen<-lmer(perPlantFH_Wt ~ meanGenD+FT1001_mean +(1|stripNo), data=modeldata_m)
model7_gen<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata_m)


(a1_gen <- anova(model2_gen,model1_gen)) # is interaction sig? no 
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
# model2_gen: perPlantFH_Wt ~ meanGenD + trt + PC5dist_mean + FT1001_mean + 
#   model2_gen:     mean_SLAbv + (1 | stripNo)
# model1_gen: perPlantFH_Wt ~ meanGenD * trt + PC5dist_mean + FT1001_mean + 
#   model1_gen:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2_gen  8 -2310.3 -2278.9 1163.2  -2326.3                         
# model1_gen  9 -2308.4 -2272.9 1163.2  -2326.4 0.0172      1     0.8958
(a2_gen <- anova(model3_gen,model2_gen)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model3_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                              model3_gen:     stripNo)
# model2_gen: perPlantFH_Wt ~ meanGenD + trt + PC5dist_mean + FT1001_mean + 
#   model2_gen:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model3_gen  7 -2310.4 -2282.9 1162.2  -2324.4                         
# model2_gen  8 -2310.3 -2278.9 1163.2  -2326.3 1.9068      1     0.1673
(a3_gen <- anova(model4.1_gen, model3.1_gen)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m2
# Models:
#   model4.1_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + (1 | stripNo)
# model3.1_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                              model3.1_gen:     stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# model4.1_gen  6 -2309.8 -2286.2 1160.9  -2321.8                        
# model3.1_gen  7 -2310.4 -2282.9 1162.2  -2324.4 2.642      1     0.1041
(a4_gen <- anova(model5_gen, model4_gen)) #is FT1001_mean sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model5_gen: perPlantFH_Wt ~ meanGenD + trt + (1 | stripNo)
# model4_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
# model5_gen  5 -2456.0 -2436.1 1233.0  -2466.0                            
# model4_gen  6 -2469.6 -2445.6 1240.8  -2481.6 15.54      1   8.08e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5_gen <- anova(model6_gen, model4_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model6_gen: perPlantFH_Wt ~ meanGenD + FT1001_mean + (1 | stripNo)
# model4_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model6_gen  5 -2462.8 -2442.8 1236.4  -2472.8                            
# model4_gen  6 -2469.6 -2445.6 1240.8  -2481.6 8.8208      1   0.002978 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_gen <- anova(model7_gen, model4_gen)) #is meanGenD sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model7_gen: perPlantFH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# model4_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7_gen  5 -2469.8 -2449.8 1239.9  -2479.8                         
# model4_gen  6 -2469.6 -2445.6 1240.8  -2481.6 1.7963      1     0.1802

# (a7_gen <- anova(model8_gen, model6_gen)) #is  sig?


###
#compare meanGenD and divlevel
(ax <- anova(model4, model4_gen)) #is one better? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model4: perPlantFH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# model4_gen: perPlantFH_Wt ~ meanGenD + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model4      6 -2468.2 -2444.2 1240.1  -2480.2                             
# model4_gen  6 -2469.6 -2445.6 1240.8  -2481.6 1.3801      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# dchisq(X,df) #where X is the Chisq value from the anova table (37.085 in your case) 
#and df is Df in the anova table +1 (0 + 1 in your case)
dchisq(1.3801,1) #result is p-value
# [1] 0.1703217 so div level and meanGenD are not different

#pred vs div
model9_gen<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+plotType+(1|stripNo), data=modeldata_m)
model10_gen<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata_m)

(a8_gen <- anova(model10_gen, model9_gen)) #plot type? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model10_gen: perPlantFH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# model9_gen: perPlantFH_Wt ~ trt + FT1001_mean + plotType + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model10_gen  5 -2469.8 -2449.8 1239.9  -2479.8                         
# model9_gen   7 -2467.2 -2439.3 1240.6  -2481.2 1.4382      2     0.4872

####biomass ~ tree div models####
#example full models
#biomass/density after transplant ~ plot genetic distance (treeDiv) * stress trt * climate pca(PC5dist_mean) +  (1| block)
#include Germ rating?

#tree distance models
model1_tr<-lmer(perPlantFH_Wt ~ treeDiv*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
model2_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
model3_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)

# modeldata_m2 <- modeldata_m[!is.na(modeldata_m$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m2)
model4.1_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+(1|stripNo), data=modeldata_m2)

model4_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+FT1001_mean+(1|stripNo), data=modeldata_m)
model5_tr<-lmer(perPlantFH_Wt ~ treeDiv+trt+(1|stripNo), data=modeldata_m)
model6_tr<-lmer(perPlantFH_Wt ~ treeDiv+FT1001_mean +(1|stripNo), data=modeldata_m)
model7_tr<-lmer(perPlantFH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata_m)


(a1_tr <- anova(model2_tr,model1_tr)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model2_tr: perPlantFH_Wt ~ treeDiv + trt + PC5dist_mean + FT1001_mean + 
#   model2_tr:     mean_SLAbv + (1 | stripNo)
# model1_tr: perPlantFH_Wt ~ treeDiv * trt + PC5dist_mean + FT1001_mean + 
#   model1_tr:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2_tr  8 -2309.6 -2278.1 1162.8  -2325.6                         
# model1_tr  9 -2307.7 -2272.2 1162.8  -2325.7 0.0502      1     0.8226
(a2_tr <- anova(model3_tr,model2_tr)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model3_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                            model3_tr:     stripNo)
# model2_tr: perPlantFH_Wt ~ treeDiv + trt + PC5dist_mean + FT1001_mean + 
#   model2_tr:     mean_SLAbv + (1 | stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# model3_tr  7 -2309.8 -2282.2 1161.9  -2323.8                        
# model2_tr  8 -2309.6 -2278.1 1162.8  -2325.6 1.811      1     0.1784
(a3_tr <- anova(model4.1_tr, model3.1_tr)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m2
# Models:
#   model4.1_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# model3.1_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                            model3.1_tr:     stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4.1_tr  6 -2309.2 -2285.5 1160.6  -2321.2                         
# model3.1_tr  7 -2309.8 -2282.2 1161.9  -2323.8 2.6447      1     0.1039
(a4_tr <- anova(model5_tr, model4_tr)) #is FT1001_mean sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model5_tr: perPlantFH_Wt ~ treeDiv + trt + (1 | stripNo)
# model4_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5_tr  5 -2455.5 -2435.6 1232.8  -2465.5                             
# model4_tr  6 -2468.8 -2444.9 1240.4  -2480.8 15.307      1  9.136e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5_tr <- anova(model6_tr, model4_tr)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model6_tr: perPlantFH_Wt ~ treeDiv + FT1001_mean + (1 | stripNo)
# model4_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)   
# model6_tr  5 -2462.0 -2442.1 1236.0  -2472.0                           
# model4_tr  6 -2468.8 -2444.9 1240.4  -2480.8 8.829      1   0.002965 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_tr <- anova(model7_tr, model4_tr)) #is treeDiv sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_m
# Models:
#   model7_tr: perPlantFH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# model4_tr: perPlantFH_Wt ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7_tr  5 -2469.8 -2449.8 1239.9  -2479.8                         
# model4_tr  6 -2468.8 -2444.9 1240.4  -2480.8 1.0648      1     0.3021














######################################################
####canopy area ~ div level models####
#dataset for modeling
modeldata_c<-modeldata[!is.na(modeldata$CanopyArea),]
modeldata_c$perPlantCanopy <- modeldata_c$CanopyArea/modeldata_c$MaxPlantNum

#example full models
#CanopyArea ~ diversity level * stress trt *  plot average climate pca distance from PSU(PC5dist_mean) + density after transplant + (1| block)
#CanopyArea ~ plot average genetic distance (meanKin) * stress trt * climate pca(PC5dist_mean) + density after transplant + (1| block)
# model1<-lmer(FH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_m)
# model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)

#div level models
model1<-lmer(CanopyArea ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)

model2<-lmer(CanopyArea ~ divLevel+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)
model3<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)

modeldata_c2 <- modeldata_c[!is.na(modeldata_c$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+mean_SLAbv+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c2)
model4.1<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c2)

model4<-lmer(CanopyArea ~ divLevel+trt+FT1001_mean+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)
model5<-lmer(CanopyArea ~ divLevel+trt+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)
model6<-lmer(CanopyArea ~ divLevel+(1|MaxPlantNum) +(1|stripNo), data=modeldata_c)
model7<-lmer(CanopyArea ~ trt+(1|MaxPlantNum)+(1|stripNo), data=modeldata_c)


(a1 <- anova(model2,model1)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model2: CanopyArea ~ divLevel + trt + PC5dist_mean + FT1001_mean + mean_SLAbv + 
#   model2:     (1 | MaxPlantNum) + (1 | stripNo)
# model1: CanopyArea ~ divLevel * trt + PC5dist_mean + FT1001_mean + mean_SLAbv + 
#   model1:     (1 | MaxPlantNum) + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2  9 2517.5 2553.1 -1249.8   2499.5                         
# model1 10 2519.5 2559.0 -1249.8   2499.5 0.0263      1     0.8713
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model3: CanopyArea ~ divLevel + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                       model3:     MaxPlantNum) + (1 | stripNo)
# model2: CanopyArea ~ divLevel + trt + PC5dist_mean + FT1001_mean + mean_SLAbv + 
#   model2:     (1 | MaxPlantNum) + (1 | stripNo)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# model3  8 2515.6 2547.2 -1249.8   2499.6                        
# model2  9 2517.5 2553.1 -1249.8   2499.5 0.061      1      0.805
(a3 <- anova(model4.1, model3.1)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c2
# Models:
#   model4.1: CanopyArea ~ divLevel + trt + FT1001_mean + (1 | MaxPlantNum) + 
#   model4.1:     (1 | stripNo)
#   model3.1: CanopyArea ~ divLevel + trt + FT1001_mean + mean_SLAbv + (1 | 
#   model3.1:     MaxPlantNum) + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4.1  7 2514.0 2541.7 -1250.0   2500.0                         
# model3.1  8 2515.6 2547.2 -1249.8   2499.6 0.3971      1     0.5286
(a4 <- anova(model5, model4)) #is FT1001 sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model5: CanopyArea ~ divLevel + trt + (1 | MaxPlantNum) + (1 | stripNo)
# model4: CanopyArea ~ divLevel + trt + FT1001_mean + (1 | MaxPlantNum) + 
#   model4:     (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model5  6 2642.7 2666.7 -1315.3   2630.7                         
# model4  7 2644.6 2672.6 -1315.3   2630.6 0.0893      1     0.7651
(a5 <- anova(model6, model5)) #is trt sig? yes ###
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model6: CanopyArea ~ divLevel + (1 | MaxPlantNum) + (1 | stripNo)
# model5: CanopyArea ~ divLevel + trt + (1 | MaxPlantNum) + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model6  5 2727.4 2747.4 -1358.7   2717.4                             
# model5  6 2642.7 2666.7 -1315.3   2630.7 86.732      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6 <- anova(model7, model5)) #is divlevel sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model7: CanopyArea ~ trt + (1 | MaxPlantNum) + (1 | stripNo)
# model5: CanopyArea ~ divLevel + trt + (1 | MaxPlantNum) + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7  5 2640.7 2660.7 -1315.3   2630.7                         
# model5  6 2642.7 2666.7 -1315.3   2630.7 0.0201      1     0.8872

# (a7 <- anova(model4, model8)) #is FT1001 mean sig? no
# (a8 <- anova(model4, model9)) #is FT1001plasticity sig? no
# (a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? no
# (a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? no





####canopy area ~ genetic distance models####
model1_gen<-lmer(perPlantCanopy ~ meanKin*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)

model2_gen<-lmer(perPlantCanopy ~ meanKin+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)
model3_gen<-lmer(perPlantCanopy ~ meanKin+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)

modeldata_c2 <- modeldata_c[!is.na(modeldata_c$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1_gen<-lmer(perPlantCanopy ~ meanKin+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c2)
model4.1_gen<-lmer(perPlantCanopy ~ meanKin+trt+FT1001_mean+(1|stripNo), data=modeldata_c2)

model4_gen<-lmer(perPlantCanopy ~ meanKin+trt+FT1001_mean+(1|stripNo), data=modeldata_c)
model5_gen<-lmer(perPlantCanopy ~ meanKin+trt+(1|stripNo), data=modeldata_c)
model6_gen<-lmer(perPlantCanopy ~ meanKin+(1|stripNo), data=modeldata_c)
model7_gen<-lmer(perPlantCanopy ~ trt+(1|stripNo), data=modeldata_c)

(a1_gen <- anova(model2_gen,model1_gen)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model2_gen: perPlantCanopy ~ meanKin + trt + PC5dist_mean + FT1001_mean + 
#   model2_gen:     mean_SLAbv + (1 | stripNo)
# model1_gen: perPlantCanopy ~ meanKin * trt + PC5dist_mean + FT1001_mean + 
#   model1_gen:     mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2_gen  8 242.15 273.75 -113.07   226.15                         
# model1_gen  9 244.12 279.67 -113.06   226.12 0.0291      1     0.8645
(a2_gen <- anova(model3_gen,model2_gen)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model3_gen: perPlantCanopy ~ meanKin + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                              model3_gen:     stripNo)
# model2_gen: perPlantCanopy ~ meanKin + trt + PC5dist_mean + FT1001_mean + 
#   model2_gen:     mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model3_gen  7 240.39 268.04 -113.19   226.39                         
# model2_gen  8 242.15 273.75 -113.07   226.15 0.2425      1     0.6224
(a3_gen <- anova(model4.1_gen, model3.1_gen)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c2
# Models:
#   model4.1_gen: perPlantCanopy ~ meanKin + trt + FT1001_mean + (1 | stripNo)
# model3.1_gen: perPlantCanopy ~ meanKin + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                              model3.1_gen:     stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4.1_gen  6 238.77 262.48 -113.39   226.77                         
# model3.1_gen  7 240.39 268.04 -113.19   226.39 0.3857      1     0.5346
(a4_gen <- anova(model5_gen, model4_gen)) #is FT1001_mean sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model5_gen: perPlantCanopy ~ meanKin + trt + (1 | stripNo)
# model4_gen: perPlantCanopy ~ meanKin + trt + FT1001_mean + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model5_gen  5 244.33 264.31 -117.16   234.33                         
# model4_gen  6 246.32 270.30 -117.16   234.32 0.0025      1     0.9598
(a5_gen <- anova(model6_gen, model5_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model6_gen: perPlantCanopy ~ meanKin + (1 | stripNo)
# model5_gen: perPlantCanopy ~ meanKin + trt + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model6_gen  4 335.72 351.71 -163.86   327.72                             
# model5_gen  5 244.33 264.31 -117.16   234.33 93.393      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_gen <- anova(model7_gen, model5_gen)) #is meanKin sig ? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model7_gen: perPlantCanopy ~ trt + (1 | stripNo)
# model5_gen: perPlantCanopy ~ meanKin + trt + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7_gen  4 242.51 258.50 -117.25   234.51                         
# model5_gen  5 244.33 264.31 -117.16   234.33 0.1815      1     0.6701

#compare meanKin and divlevel
modeldata_c3 <- modeldata_c[!is.na(modeldata_c$meanKin),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model5.1_gen<-lmer(perPlantCanopy ~ meanKin+trt+(1|stripNo), data=modeldata_c3)
model5.1<-lmer(perPlantCanopy ~ divLevel+trt+(1|stripNo), data=modeldata_c3)
(ax <- anova(model5.1, model5.1_gen)) #are kin and div different? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c3
# Models:
#   model5.1: perPlantCanopy ~ divLevel + trt + (1 | stripNo)
# model5.1_gen: perPlantCanopy ~ meanKin + trt + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model5.1      5 244.51 264.49 -117.25   234.51                             
# model5.1_gen  5 244.33 264.31 -117.16   234.33 0.1814      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



####canopy area ~ tree div models####
model1_tr<-lmer(perPlantCanopy ~ treeDiv*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)

model2_tr<-lmer(perPlantCanopy ~ treeDiv+trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)
model3_tr <- lmer(perPlantCanopy ~ treeDiv+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c)

modeldata_c2 <- modeldata_c[!is.na(modeldata_c$mean_SLAbv),] #remove rows with NAs for mean_SLAbv so datasets are the same size
model3.1_tr<-lmer(perPlantCanopy ~ treeDiv+trt+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_c2)
model4.1_tr<-lmer(perPlantCanopy ~ treeDiv+trt+FT1001_mean+(1|stripNo), data=modeldata_c2)

model4_tr<-lmer(perPlantCanopy ~ treeDiv+trt+FT1001_mean+(1|stripNo), data=modeldata_c)
model5_tr<-lmer(perPlantCanopy ~ treeDiv+trt+(1|stripNo), data=modeldata_c)
model6_tr<-lmer(perPlantCanopy ~ treeDiv+(1|stripNo), data=modeldata_c)
model7_tr<-lmer(perPlantCanopy ~ trt+(1|stripNo), data=modeldata_c)

(a1_tr <- anova(model2_tr,model1_tr)) # is interaction sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model2_tr: perPlantCanopy ~ treeDiv + trt + PC5dist_mean + FT1001_mean + 
#   model2_tr:     mean_SLAbv + (1 | stripNo)
# model1_tr: perPlantCanopy ~ treeDiv * trt + PC5dist_mean + FT1001_mean + 
#   model1_tr:     mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model2_tr  8 242.31 273.91 -113.15   226.31                         
# model1_tr  9 244.31 279.86 -113.15   226.31 0.0015      1     0.9689
(a2_tr <- anova(model3_tr,model2_tr)) # is PC5dist_mean covariate sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model3_tr: perPlantCanopy ~ treeDiv + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                             model3_tr:     stripNo)
# model2_tr: perPlantCanopy ~ treeDiv + trt + PC5dist_mean + FT1001_mean + 
#   model2_tr:     mean_SLAbv + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model3_tr  7 240.53 268.19 -113.27   226.53                         
# model2_tr  8 242.31 273.91 -113.15   226.31 0.2246      1     0.6356
(a3_tr <- anova(model4.1_tr, model3.1_tr)) #is SLA sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c2
# Models:
#   model4.1_tr: perPlantCanopy ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# model3.1_tr: perPlantCanopy ~ treeDiv + trt + FT1001_mean + mean_SLAbv + (1 | 
#                                                                             model3.1_tr:     stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4.1_tr  6 238.91 262.61 -113.45   226.91                         
# model3.1_tr  7 240.53 268.19 -113.27   226.53 0.3749      1     0.5403
(a4_tr <- anova(model5_tr, model4_tr)) #is FT1001_mean sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model5_tr: perPlantCanopy ~ treeDiv + trt + (1 | stripNo)
# model4_tr: perPlantCanopy ~ treeDiv + trt + FT1001_mean + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model5_tr  5 244.45 264.43 -117.22   234.45                         
# model4_tr  6 246.44 270.42 -117.22   234.44 0.0016      1     0.9677
(a5_tr <- anova(model6_tr, model5_tr)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model6_tr: perPlantCanopy ~ treeDiv + (1 | stripNo)
# model5_tr: perPlantCanopy ~ treeDiv + trt + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model6_tr  4 335.86 351.85 -163.93   327.86                             
# model5_tr  5 244.45 264.43 -117.22   234.45 93.417      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_tr <- anova(model7_tr, model5_tr)) #is treeDiv sig ? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata_c
# Models:
#   model7_tr: perPlantCanopy ~ trt + (1 | stripNo)
# model5_tr: perPlantCanopy ~ treeDiv + trt + (1 | stripNo)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model7_tr  4 242.51 258.50 -117.25   234.51                         
# model5_tr  5 244.45 264.43 -117.22   234.45 0.0629      1      0.802







# #####model expected genotype number in polyculture from monocultures - use instead of divLevel?####
# #code from Jesse 11/27/2018
# # fieldD <- read.csv('~/Dropbox/jesse/Arabidopsis/BEF/FieldDataWithDiversityInfo.csv', as.is = T)
# fieldD <- read.csv("FieldDataWithDiversityInfo.csv", as.is = T)
# isurvC <- tapply(fieldD$PlantNum_Initial[fieldD$plotType == 'mono' & fieldD$trt == 'C'], fieldD$lines1[fieldD$plotType == 'mono' & fieldD$trt == 'C'], mean, na.rm = T)
# 
# isurvS <- tapply(fieldD$PlantNum_Initial[fieldD$plotType == 'mono' & fieldD$trt == 'S'], fieldD$lines1[fieldD$plotType == 'mono' & fieldD$trt == 'S'], mean, na.rm = T)
# 
# 
# #get expected biomass for each plot, accounting for different survival rates
# RandExpBiomPerPlant <- matrix(NA, nrow = nrow(fieldD), ncol = 100)
# 
# 
# for(i in 1:nrow(fieldD)){
#   for(j in 1:100){
#     
#     if(fieldD$plotType[i] != 'mono'){
#       
#       if(fieldD$trt[i] == 'C'){
#         expg <-     names(isurvC)[names(isurvC) %in% fieldD[i,paste0('lines', 1:fieldD$divLevel[i])]]
#         expg <- rep(expg, 20/fieldD$divLevel[i])
#         #make random comm.
#         newcomm <- sample(expg, fieldD$PlantNum_Initial[i], prob = isurvC[expg])
#         
#         RandExpBiomPerPlant[i,j] <- mean(biomC[newcomm],na.rm = T)
#       }
#       
#       if(fieldD$trt[i] == 'S'){
#         expg <-     names(isurvS)[names(isurvS) %in% fieldD[i,paste0('lines', 1:fieldD$divLevel[i])]]
#         expg <- rep(expg, 20/fieldD$divLevel[i])
#         #make random comm.
#         newcomm <- sample(expg, fieldD$PlantNum_Initial[i], prob = isurvS[expg])
#         
#         RandExpBiomPerPlant[i,j] <- mean(biomS[newcomm],na.rm = T)
#       }
#       
#     }
#   }
# }
# 
# length(unique(newcomm)) #gives you expected number of genotypes in polyculture based on survival probability

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
