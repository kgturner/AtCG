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


####LM models - biomass####
# #the results tables with parameter estimate (slope), pvalue, and R2adjusted, 
# for sla and spad, both singularly (tmp) and with flowering time (tmp2):

modeldata_m<-modeldata[!is.na(modeldata$FH_Wt),]
summary(modeldata_m)
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum
# model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
# 
#covariates to test:
# + trt
# divLevel +PC5dist_mean +mean_SLAbv +FT1001_mean

#table to save results
tmp <- c() #divLevel
tmp2 <- c() #PC5dist_mean
tmp3 <- c()#mean_SLAbv
tmp4 <- c()#FT1001_mean
tmp5 <- c() # meanGenD
tmp6 <- c() # treeDiv

tmod <- lm(perPlantFH_Wt  ~ trt + trt:divLevel, data = modeldata_m[modeldata_m$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

# > tmp
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)    1.306575e-02 2.993049e-34 0.01520509
# trtS          -3.107557e-03 2.306755e-02 0.01520509
# trtC:divLevel -3.502689e-05 7.957953e-01 0.01520509
# trtS:divLevel -1.050102e-04 4.743391e-01 0.01520509

tmod2 <- lm(perPlantFH_Wt  ~ trt + trt:PC5dist_mean, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

# > tmp2
#                        Estimate     Pr(>|t|)     r2adj
# (Intercept)        0.0166153853 2.657130e-10 0.0220554
# trtS              -0.0047592612 1.813189e-01 0.0220554
# trtC:PC5dist_mean -0.0004968652 1.298481e-01 0.0220554
# trtS:PC5dist_mean -0.0003077844 3.325025e-01 0.0220554

tmod3 <- lm(perPlantFH_Wt  ~ trt + trt:mean_SLAbv, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

# > tmp3
#                      Estimate    Pr(>|t|)      r2adj
# (Intercept)      0.0006687895 0.889595120 0.02950734
# trtS             0.0044807106 0.526026032 0.02950734
# trtC:mean_SLAbv -0.0092673145 0.009512552 0.02950734
# trtS:mean_SLAbv -0.0039210231 0.338832791 0.02950734

tmod4 <- lm(perPlantFH_Wt  ~ trt + trt:FT1001_mean, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

# > tmp4
#                       Estimate    Pr(>|t|)      r2adj
# (Intercept)       0.0037788071 0.346707868 0.05047903
# trtS             -0.0062664771 0.271968884 0.05047903
# trtC:FT1001_mean  0.0001311531 0.020378018 0.05047903
# trtS:FT1001_mean  0.0001700767 0.002460613 0.05047903

tmod5 <- lm(perPlantFH_Wt  ~ trt + trt:meanGenD, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

# > tmp5
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)    0.013325596 4.034789e-36 0.01754255
# trtS          -0.003296981 1.424902e-02 0.01754255
# trtC:meanGenD -0.001608663 4.250669e-01 0.01754255
# trtS:meanGenD -0.001923780 3.577663e-01 0.01754255

tmod6 <- lm(perPlantFH_Wt  ~ trt + trt:treeDiv, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6

# > tmp6
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)   1.317365e-02 1.071261e-37 0.01628405
# trtS         -3.255342e-03 1.197231e-02 0.01628405
# trtC:treeDiv -4.087812e-05 5.626533e-01 0.01628405
# trtS:treeDiv -6.141870e-05 4.164889e-01 0.01628405

####LM models - canopy area####
# #the results tables with parameter estimate (slope), pvalue, and R2adjusted, 
# for sla and spad, both singularly (tmp) and with flowering time (tmp2):

modeldata_c<-modeldata[!is.na(modeldata$CanopyArea),]
modeldata_c$perPlantCanopy <- modeldata_c$CanopyArea/modeldata_c$MaxPlantNum
# 
#covariates to test:
# + trt
# divLevel +PC5dist_mean +mean_SLAbv +FT1001_mean

#table to save results
tmp <- c() #divLevel
tmp2 <- c() #PC5dist_mean
tmp3 <- c()#mean_SLAbv
tmp4 <- c()#FT1001_mean
tmp5 <- c() # meanGenD
tmp6 <- c() # treeDiv

tmod <- lm(perPlantCanopy  ~ trt + trt:divLevel, data = modeldata_c[modeldata_c$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

# > tmp
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)    0.5654690504 1.106487e-60 0.201335
# trtS          -0.3416236396 2.743305e-16 0.201335
# trtC:divLevel -0.0012060263 7.710441e-01 0.201335
# trtS:divLevel  0.0000904267 9.831759e-01 0.201335

tmod2 <- lm(perPlantCanopy  ~ trt + trt:PC5dist_mean, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

# > tmp2
#                        Estimate     Pr(>|t|)     r2adj
# (Intercept)        0.5649024355 2.278522e-13 0.2011672
# trtS              -0.3368427725 1.198225e-03 0.2011672
# trtC:PC5dist_mean -0.0005176112 9.571620e-01 0.2011672
# trtS:PC5dist_mean -0.0005328769 9.539398e-01 0.2011672

tmod3 <- lm(perPlantCanopy  ~ trt + trt:mean_SLAbv, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

# > tmp3
#                    Estimate    Pr(>|t|)      r2adj
# (Intercept)      0.54040192 0.0005891324 0.2051472
# trtS            -0.36202597 0.0943276315 0.2051472
# trtC:mean_SLAbv -0.02101844 0.8548829039 0.2051472
# trtS:mean_SLAbv -0.03843478 0.7463009786 0.2051472

tmod4 <- lm(perPlantCanopy  ~ trt + trt:FT1001_mean, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

# > tmp4
#                       Estimate    Pr(>|t|)      r2adj
# (Intercept)       0.6149299899 1.864680e-06 0.2015996
# trtS             -0.3722129523 3.617364e-02 0.2015996
# trtC:FT1001_mean -0.0007538586 6.661990e-01 0.2015996
# trtS:FT1001_mean -0.0002590184 8.782208e-01 0.2015996

tmod5 <- lm(perPlantCanopy  ~ trt + trt:meanGenD, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

# > tmp5
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)    0.56733594 2.105302e-62 0.201971
# trtS          -0.33734048 1.809903e-16 0.201971
# trtC:meanGenD -0.02716877 6.574161e-01 0.201971
# trtS:meanGenD -0.02649864 6.648309e-01 0.201971

tmod6 <- lm(perPlantCanopy  ~ trt + trt:treeDiv, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6

# > tmp6
#                   Estimate     Pr(>|t|)      r2adj
# (Intercept)   0.5664224169 1.989844e-65 0.2016296
# trtS         -0.3404750875 9.559905e-18 0.2016296
# trtC:treeDiv -0.0009694545 6.554810e-01 0.2016296
# trtS:treeDiv -0.0003465502 8.752998e-01 0.2016296
