#LM models of biomass
#1/22/2020

#using lm() in {stats}
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA")) #see construction in REML_biomass.R. Made from:
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


####From JRLasky:####
# #Here's an example making the results tables with parameter estimate (slope), pvalue, and R2adjusted, for sla and spad, both singularly (tmp) and with flowering time (tmp2):
# 
# #table to save results
# tmp <- c()
# tmp2 <- c() #w FT
# 
#w/sla 
# tmod <- lm(DivEffect  ~ trt + trt: slaPlot , data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp <- rbind(tmp, cbind(summary(tmod)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
# 
# tmod2 <- lm(DivEffect  ~ trt + trt: slaPlot + trt: FT1001_mean, data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
# 
# w/spad
# tmod <- lm(DivEffect  ~ trt + trt: spadPlot , data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp <- rbind(tmp, cbind(summary(tmod)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
# 
# tmod2 <- lm(DivEffect  ~ trt + trt: spadPlot + trt: FT1001_mean, data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))


####LM models####
# #the results tables with parameter estimate (slope), pvalue, and R2adjusted, 
# for sla and spad, both singularly (tmp) and with flowering time (tmp2):

modeldata_m<-modeldata[!is.na(modeldata$FH_Wt),]
summary(modeldata_m)
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum
# model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
# 
#table to save results
tmp <- c()
tmp2 <- c() #w FT

#sla
tmod <- lm(perPlantFH_Wt  ~ divLevel + divLevel:trt + trt+PC5dist_mean +mean_SLAbv , data = modeldata_m[modeldata_m$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))

# > tmp
# Estimate   Pr(>|t|)      r2adj
# (Intercept)    7.101957e-03 0.08464996 0.03288282
# divLevel      -6.090313e-05 0.65655692 0.03288282
# trtS          -2.088654e-03 0.14814378 0.03288282
# PC5dist_mean  -4.255722e-04 0.07201557 0.03288282
# mean_SLAbv    -6.988735e-03 0.00948545 0.03288282
# divLevel:trtS -8.735370e-05 0.66599437 0.03288282

tmod2 <- lm(perPlantFH_Wt  ~ divLevel + divLevel:trt + trt+PC5dist_mean +mean_SLAbv+FT1001_mean, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))

# > tmp2
# Estimate   Pr(>|t|)      r2adj
# (Intercept)    1.820759e-03 0.69982812 0.04361548
# divLevel      -6.429604e-05 0.63690367 0.04361548
# trtS          -2.626208e-03 0.07157998 0.04361548
# PC5dist_mean  -3.191816e-04 0.18316329 0.04361548
# mean_SLAbv    -4.924329e-03 0.08165734 0.04361548
# FT1001_mean    1.034349e-04 0.02604115 0.04361548
# divLevel:trtS -7.004966e-05 0.72796065 0.04361548

# 
# tmod <- lm(perPlantFH_Wt  ~ trt + trt: spadPlot , data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp <- rbind(tmp, cbind(summary(tmod)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
# 
# tmod2 <- lm(perPlantFH_Wt  ~ trt + trt: spadPlot + trt: FT1001_mean, data = fieldD[fieldD$MaxPlantNum > 10,] )
# tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][3:4,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))