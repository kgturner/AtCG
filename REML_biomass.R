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


#####Final harvest weight####
#dataset for modeling
modeldata<-fieldData[!is.na(fieldData$FH_Wt),]

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
model8<-lmer(FH_Wt ~ trt+FT1001_mean+(1|stripNo), data=modeldata) ########
model9<-lmer(FH_Wt ~ trt+FT1001plast_mean+(1|stripNo), data=modeldata)
model10<-lmer(FH_Wt ~ divLevel+trt+FT1001_mean+(1|stripNo), data=modeldata)##
model11<-lmer(FH_Wt ~ divLevel+trt+FT1001_mean+FT1001plast_mean+(1|stripNo), data=modeldata)

(a1 <- anova(model2,model1)) # is interaction sig? no
(a2 <- anova(model3,model2)) # is PC5dist_mean covariate sig? no
(a3 <- anova(model4, model3)) #is divLevel sig? no
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





#genetic distance models
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
(ax <- anova(model6_gen,model10)) #no sig diff

####canopy area####

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
