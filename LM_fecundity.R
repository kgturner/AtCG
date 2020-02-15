#LM models of fecundity
#1/22/2020

#using lm() in {stats}
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
fieldData <- read.csv("FieldDataWithDiversityInfo_14May19.csv", na.strings=c(""," ","NA"))#using data from excel file 3_21_19
lineData <- read.csv("chosenLines.csv", na.strings=c(""," ","NA"))
damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt", na.strings=c(""," ","NA")) #census data based on photos from 6/12/2018
SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))

fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)

#created in REML_biomass.R
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA"))

####modeling dataset for fecundity####
#dataset for modeling
library(plyr)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))
modeldata_f <- merge.data.frame(modeldata, seed_mean, by.x="PotID", by.y="Pot.Number", all.x=TRUE)

modeldata_f<-modeldata_f[!is.na(modeldata_f$seedEstMean),] #lose 18 damaged pots
summary(modeldata_f)

summary(seed_mean$Pot.Number %!in% modeldata_f$PotID)

summary(modeldata_f[modeldata_f$MaxPlantNum > 10,]) #196 pots

####LM models - seedEstMean####
# #the results tables with parameter estimate (slope), pvalue, and R2adjusted, 
# for sla and spad, both singularly (tmp) and with flowering time (tmp2):

###model1.1<-lmer(seedEstMean ~ divLevel*trt+numFecPlants + PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_f)

#covariates to test:
# + trt
# divLevel +PC5dist_mean +mean_SLAbv +FT1001_mean +numFecPlants

#table to save results
tmp <- c() #divLevel
tmp2 <- c() #PC5dist_mean
tmp3 <- c()#mean_SLAbv
tmp4 <- c()#FT1001_mean
tmp5 <- c() # meanGenD
tmp6 <- c() # treeDiv
tmp7 <- c() #numFecPlants

tmod <- lm(seedEstMean  ~ trt + trt:divLevel, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

#                 Estimate     Pr(>|t|)      r2adj
# (Intercept)    76.6748606 3.655971e-15 0.04649648
# trtS          -19.3120327 1.370221e-01 0.04649648
# trtC:divLevel   2.2076234 2.889518e-02 0.04649648
# trtS:divLevel   0.6310146 5.733510e-01 0.04649648

tmod2 <- lm(seedEstMean  ~ trt + trt:PC5dist_mean, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

#                     Estimate     Pr(>|t|)      r2adj
# (Intercept)        76.854140 0.0002848846 0.02272497
# trtS              -17.682826 0.5488922756 0.02272497
# trtC:PC5dist_mean   1.529303 0.5467125990 0.02272497
# trtS:PC5dist_mean   0.160760 0.9483687927 0.02272497

tmod3 <- lm(seedEstMean  ~ trt + trt:mean_SLAbv, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

#                   Estimate  Pr(>|t|)    r2adj
# (Intercept)      49.876803 0.1989018 0.017132
# trtS              8.528132 0.8786549 0.017132
# trtC:mean_SLAbv -30.273886 0.3107652 0.017132
# trtS:mean_SLAbv  -3.203369 0.9215010 0.017132

tmod4 <- lm(seedEstMean  ~ trt + trt:FT1001_mean, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

#                       Estimate   Pr(>|t|)      r2adj
# (Intercept)        91.79598593 0.05427476 0.03845188
# trtS             -119.85593358 0.07670664 0.03845188
# trtC:FT1001_mean   -0.05192143 0.94613130 0.03845188
# trtS:FT1001_mean    1.43961111 0.06250012 0.03845188

tmod5 <- lm(seedEstMean  ~ trt + trt:meanGenD, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

#                 Estimate     Pr(>|t|)      r2adj
# (Intercept)    79.38139 8.771310e-16 0.03511763
# trtS          -18.28833 1.614102e-01 0.03511763
# trtC:meanGenD  26.37127 9.446353e-02 0.03511763
# trtS:meanGenD  -2.10719 9.026868e-01 0.03511763

tmod6 <- lm(seedEstMean  ~ trt + trt:treeDiv, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6

#                 Estimate     Pr(>|t|)      r2adj
# (Intercept)   79.84983673 3.364080e-17 0.03798237
# trtS         -19.86294657 1.108638e-01 0.03798237
# trtC:treeDiv   0.96702741 6.631072e-02 0.03798237
# trtS:treeDiv   0.05532629 9.250191e-01 0.03798237

tmod7 <- lm(seedEstMean  ~ trt + trt:numFecPlants, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp7 <- rbind(tmp7, cbind(summary(tmod7)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod7)[[9]]))
tmp7

#                     Estimate     Pr(>|t|)      r2adj
# (Intercept)       61.7328608 1.001495e-05 0.04796539
# trtS              -4.9397835 8.043652e-01 0.04796539
# trtC:numFecPlants  2.5968987 2.138386e-02 0.04796539
# trtS:numFecPlants  0.3827009 7.699797e-01 0.04796539
