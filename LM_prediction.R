#testing prediciton plots
#2/21/2020

#using lm() in {stats}
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA")) #see construction in REML_biomass.R. Made from:
# fieldData <- read.csv("FieldDataWithDiversityInfo_2Aug19.csv", na.strings=c(""," ","NA"))
# lineData <- read.csv("chosenLines.csv", na.strings=c(""," ","NA"))
# # damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
# prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt", na.strings=c(""," ","NA")) #census data based on photos from 6/12/2018
# SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))
predtype <- read.delim ("plots_withPredictTypes.csv", sep = ",", na.strings=c(""," ","NA"))

#plotType: div, mono, pred
#predType???

####testing prediciton plots - biomass####
#compare only to non-mono plots (since there aren't any mono pred plots)

# #the results tables with parameter estimate (slope), pvalue, and R2adjusted,
# anova(lm(DivEffect ~ typePlot * trt, data = fieldD[fieldD$MaxPlantNum > 10,]))
# 
# a1 <- aov(DivEffect ~ typePlot * trt, data = fieldD[fieldD$MaxPlantNum > 10,])
# posthoc <- TukeyHSD(x=a1, 'typePlot', conf.level=0.95)
 
modeldata_p <- merge.data.frame(modeldata, predtype[,c(1,8)], by="PotID")
modeldata_p<-modeldata_p[!is.na(modeldata$FH_Wt),]
modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
modeldata_p <- modeldata_p[modeldata_p$plotType %!in% "mono",]
levels(modeldata_p$typePlot)<-c(levels(modeldata_p$typePlot),"div")  #Add the extra level to your factor
modeldata_p$typePlot[is.na(modeldata_p$typePlot)] <- "div"           #Change NA to "div"
summary(modeldata_p)

# model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
#
#covariates to test:
# + trt
# divLevel +PC5dist_mean +mean_SLAbv +FT1001_mean

#table to save results
tmp <- c() #plotType, grouping all pred plots together

tmod <- lm(perPlantFH_Wt  ~ trt + trt:plotType, data = modeldata_p[modeldata_p$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

# > tmp
#                       Estimate     Pr(>|t|)      r2adj
# (Intercept)        1.192806e-02 7.299935e-08 -0.0012946
# trtS              -3.907101e-03 1.856965e-01 -0.0012946
# trtC:plotTypepred  9.435373e-05 9.743941e-01 -0.0012946
# trtS:plotTypepred  9.247774e-04 7.623485e-01 -0.0012946

tmp2 <- c() #typePlot, div vs each type of pred plot

tmod2 <- lm(perPlantFH_Wt  ~ trt + trt:typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,] )
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

# > tmp2
#                             Estimate     Pr(>|t|)      r2adj
#(Intercept)               0.023479046 5.111623e-07 0.03712244
# trtS                     -0.017937041 6.593999e-03 0.03712244
# trtC:typePlotGdiv        -0.015517788 1.338298e-02 0.03712244
# trtS:typePlotGdiv         0.003437446 6.113838e-01 0.03712244
# trtC:typePlotLF          -0.016100356 1.437801e-02 0.03712244
# trtS:typePlotLF           0.003002931 6.428893e-01 0.03712244
# trtC:typePlotSimClimFdiv -0.015998941 1.498510e-02 0.03712244
# trtS:typePlotSimClimFdiv  0.010401778 1.491529e-01 0.03712244
# trtC:typePlotSimClimGdiv -0.011145199 8.755508e-02 0.03712244
# trtS:typePlotSimClimGdiv  0.001219378 8.649819e-01 0.03712244
# trtC:typePlotdiv         -0.011550991 1.797245e-02 0.03712244
# trtS:typePlotdiv          0.002478949 6.340496e-01 0.03712244

anova(lm(perPlantFH_Wt ~ trt*typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,]))
# Analysis of Variance Table
# 
# Response: perPlantFH_Wt
# Df    Sum Sq    Mean Sq F value  Pr(>F)  
# trt           1 0.0003275 0.00032755  2.8818 0.09289 .
# typePlot      5 0.0003696 0.00007392  0.6503 0.66192  
# trt:typePlot  5 0.0010132 0.00020265  1.7829 0.12383  
# Residuals    94 0.0106840 0.00011366                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
a1 <- aov(perPlantFH_Wt ~ trt *typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,])
a1
# Call:
#   aov(formula = perPlantFH_Wt ~ trt * typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 
#                                                                      10, ])
# 
# Terms:
#   trt    typePlot trt:typePlot   Residuals
# Sum of Squares  0.000327549 0.000369588  0.001013226 0.010683991
# Deg. of Freedom           1           5            5          94
# 
# Residual standard error: 0.01066112
# Estimated effects may be unbalanced
posthoc <- TukeyHSD(x=a1, 'typePlot', conf.level=0.95)
posthoc
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = perPlantFH_Wt ~ trt * typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10, ])
# 
# $typePlot
# diff          lwr         upr     p adj
# Gdiv-EF                 -6.901773e-03 -0.020127419 0.006323874 0.6534992
# LF-EF                   -6.991182e-03 -0.020216828 0.006234464 0.6408857
# SimClimFdiv-EF          -4.119643e-03 -0.018060699 0.009821412 0.9550048
# SimClimGdiv-EF          -5.504187e-03 -0.019445242 0.008436868 0.8595443
# div-EF                  -5.187886e-03 -0.015432428 0.005056655 0.6819496
# LF-Gdiv                 -8.940963e-05 -0.013315056 0.013136237 1.0000000
# SimClimFdiv-Gdiv         2.782129e-03 -0.011158926 0.016723184 0.9920887
# SimClimGdiv-Gdiv         1.397586e-03 -0.012543470 0.015338641 0.9997075
# div-Gdiv                 1.713886e-03 -0.008530655 0.011958428 0.9965316
# SimClimFdiv-LF           2.871539e-03 -0.011069516 0.016812594 0.9908481
# SimClimGdiv-LF           1.486995e-03 -0.012454060 0.015428050 0.9996038
# div-LF                   1.803296e-03 -0.008441245 0.012047837 0.9955920
# SimClimGdiv-SimClimFdiv -1.384543e-03 -0.016006045 0.013236958 0.9997789
# div-SimClimFdiv         -1.068243e-03 -0.012221087 0.010084601 0.9997661
# div-SimClimGdiv          3.163008e-04 -0.010836543 0.011469145 0.9999994