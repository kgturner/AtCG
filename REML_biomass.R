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
fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
lineData <- read.csv("chosenLines.csv")

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
# FT1001plast_mean - mean flowering time plasticity (reponse to 10 vs 16 C)
# FT1001plast_var - variance in plasticity 
# PlantNum_Initial - plants counted after 1 week in field (i.e. after transplant mortality)
# GermRating - categorical assesment of germ 1 week (?) before transplant to field

#####Final harvest weight####
#dataset for modeling
modeldata<-fieldData[!is.na(fieldData$FH_Wt),]
summary(modeldata)
subset(modeldata, GermRating =="NoGerm") #pot 74, no germ, yet PlantNum_Initial = 18, had canopy and FH_Wt


###
#example full models
#biomass ~ diversity level * stress trt *  plot average climate pca distance from PSU(PC5dist_mean) + density after transplant + (1| block)
#biomass ~ plot average genetic distance (meanKin) * stress trt * climate pca(PC5dist_mean) + density after transplant + (1| block)

#div level models
model1<-lmer(FH_Wt ~ divLevel*trt+PC5dist_mean+(1|stripNo), data=modeldata)
model2<-lmer(FH_Wt ~ divLevel+trt+PC5dist_mean+(1|stripNo), data=modeldata)
model3<-lmer(FH_Wt ~ divLevel+trt+(1|stripNo), data=modeldata)
model4<-lmer(FH_Wt ~ trt+(1|stripNo), data=modeldata)
model5<-lmer(FH_Wt ~ divLevel+(1|stripNo), data=modeldata)
model6<-lmer(FH_Wt ~ trt+(1|stripNo)+(1|GermRating), data=modeldata)
model7<-lmer(FH_Wt ~ trt+(1|stripNo)+(1|PlantNum_Initial), data=modeldata)
model8<-lmer(FH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata) ####
model9<-lmer(FH_Wt ~ trt+FT1001plast_mean+(1|stripNo), data=modeldata)
model10<-lmer(FH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata)##
model11<-lmer(FH_Wt ~ divLevel+trt+FT1001_mean+FT1001plast_mean+(1|stripNo), data=modeldata)
model12 <- lmer(FH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | GermRating), data=modeldata)
model13 <- lmer(FH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo)+ (1 | PlantNum_Initial), data=modeldata)
  
(a1 <- anova(model2,model1)) # is interaction sig? no
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
(a3 <- anova(model4, model3)) #is divLevel sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: FH_Wt ~ trt + (1 | stripNo)
# model3: FH_Wt ~ divLevel + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4  4 -130.31 -114.05 69.155  -138.31                         
# model3  5 -128.53 -108.20 69.267  -138.53 0.2237      1     0.6363
(a4 <- anova(model5, model3)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5: FH_Wt ~ divLevel + (1 | stripNo)
# model3: FH_Wt ~ divLevel + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model5  4 -120.93 -104.67 64.466  -128.93                            
# model3  5 -128.53 -108.20 69.267  -138.53 9.6021      1   0.001944 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5 <- anova(model4, model6)) #is germ rating sig? no
(a6 <- anova(model4, model7)) #is initial plant number sig? no
(a7 <- anova(model4, model8)) #is FT1001 mean sig? yes!
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: FH_Wt ~ trt + (1 | stripNo)
# model8: FH_Wt ~ trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model4  4 -130.31 -114.05 69.155  -138.31                             
# model8  5 -143.96 -123.63 76.982  -153.96 15.654      1  7.603e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a8 <- anova(model4, model9)) #is FT1001plasticity sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4: FH_Wt ~ trt + (1 | stripNo)
# model9: FH_Wt ~ trt + FT1001plast_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
# model4  4 -130.31 -114.05 69.155  -138.31                           
# model9  5 -134.82 -114.50 72.413  -144.82 6.5153      1     0.0107 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a9 <- anova(model8, model10)) #is div level sig with FT1001mean included? no
(a10 <- anova(model10, model11)) #is FT1001plast_mean sig if FT1001mean included? no
(a11 <- anova(model10, model12)) #is germ rating sig? no
(a12 <- anova(model10, model13)) #is plant number sig? no




#genetic distance models
#include Germ_rating
model1_gen<-lmer(FH_Wt ~ meanKin*trt+PC5dist_mean+(1|stripNo), data=modeldata)
model2_gen<-lmer(FH_Wt ~ meanKin+trt+PC5dist_mean+(1|stripNo), data=modeldata)
model3_gen<-lmer(FH_Wt ~ meanKin+trt+(1|stripNo), data=modeldata)
model4_gen<-lmer(FH_Wt ~ trt+(1|stripNo), data=modeldata)
model5_gen<-lmer(FH_Wt ~ meanKin+(1|stripNo), data=modeldata)
model6_gen<-lmer(FH_Wt ~ meanKin+trt+FT1001_mean+(1|stripNo), data=modeldata)
model7_gen<-lmer(FH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata)

(a1_gen <- anova(model2_gen,model1_gen)) # is interaction sig? no
(a2_gen <- anova(model3_gen,model2_gen)) # is PC5dist_mean covariate sig? no
(a3_gen <- anova(model4_gen, model3_gen)) #is meanKin sig? no
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model4_gen: FH_Wt ~ trt + (1 | stripNo)
# model3_gen: FH_Wt ~ meanKin + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)
# model4_gen  4 -130.31 -114.05 69.155  -138.31                        
# model3_gen  5 -129.14 -108.81 69.572  -139.14 0.834      1     0.3611
(a4_gen <- anova(model5_gen, model3_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5_gen: FH_Wt ~ meanKin + (1 | stripNo)
# model3_gen: FH_Wt ~ meanKin + trt + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
# model5_gen  4 -121.56 -105.30 64.781  -129.56                            
# model3_gen  5 -129.14 -108.81 69.572  -139.14 9.5811      1   0.001966 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a5_gen <- anova(model3_gen, model6_gen)) #is FT1001_mean sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model3_gen: FH_Wt ~ meanKin + trt + (1 | stripNo)
# model6_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model3_gen  5 -129.14 -108.81 69.572  -139.14                             
# model6_gen  6 -143.20 -118.80 77.598  -155.20 16.052      1  6.162e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(a6_gen <- anova(model7_gen, model6_gen)) #is meanKin sig with FT1001_mean? no

#compare meanKin and divlevel
(ax <- anova(model10, model6_gen)) 
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model10: FH_Wt ~ divLevel + trt + FT1001_mean + (1 | stripNo)
# model6_gen: FH_Wt ~ meanKin + trt + FT1001_mean + (1 | stripNo)
# Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model10     6 -142.29 -117.89 77.145  -154.29                             
# model6_gen  6 -143.20 -118.80 77.598  -155.20 0.9057      0  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#pred vs div
model8_gen<-lmer(FH_Wt ~ trt+FT1001_mean+plotType+(1|stripNo), data=modeldata)
model9_gen<-lmer(FH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata)

(a7_gen <- anova(model9_gen, model8_gen)) #plot type? no

####canopy area####
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
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4  5 2899.2 2919.7 -1444.6   2889.2                         
# model3  6 2901.1 2925.8 -1444.6   2889.1 0.0241      1     0.8766
(a4 <- anova(model5, model3)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5: CanopyArea ~ divLevel + (1 | stripNo) + (1 | GermRating)
# model3: CanopyArea ~ divLevel + trt + (1 | stripNo) + (1 | GermRating)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
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
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
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





#genetic distance models
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
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# model4_gen  5 2899.2 2919.7 -1444.6   2889.2                         
# model3_gen  6 2901.2 2925.8 -1444.6   2889.2 0.0034      1     0.9535
(a4_gen <- anova(model5_gen, model3_gen)) #is trt sig? yes
# refitting model(s) with ML (instead of REML)
# Data: modeldata
# Models:
#   model5_gen: CanopyArea ~ meanKin + (1 | stripNo) + (1 | GermRating)
# model3_gen: CanopyArea ~ meanKin + trt + (1 | stripNo) + (1 | GermRating)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
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

#####model expected genotype number in polyculture from monocultures - use instead of divLevel?####
#code from Jesse 11/27/2018
fieldD <- read.csv('~/Dropbox/jesse/Arabidopsis/BEF/FieldDataWithDiversityInfo.csv', as.is = T)
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
