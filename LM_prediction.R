#testing prediciton plots
#2/21/2020

#using lm() in {stats}
'%!in%' <- function(x,y)!('%in%'(x,y))

#data
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA")) #see construction in REML_biomass.R. Made from:
fieldData <- read.csv("FieldDataWithDiversityInfo_2Aug19.csv", na.strings=c(""," ","NA"))
lineData <- read.csv("chosenLines.csv", na.strings=c(""," ","NA"))
# damaged_pot <- c(2,9,100,110,111,120,121,126,128,130,138,166,186,200,201,202,209,213,214,217,230,232,233,246,247,263,264,266,331,362,363,365,367,368,371,376,378,383,384,390,392,396,406,410,436,438,439,440,444,450)
prelimSurv <- read.delim("PrelimSurvivalData_20190321.txt", na.strings=c(""," ","NA")) #census data based on photos from 6/12/2018
SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))

#plotType: div, mono, pred
#predType???

####testing prediciton plots - biomass####
#compare only to non-mono plots (since there aren't any mono pred plots)

# anova(lm(DivEffect ~ typePlot * trt, data = fieldD[fieldD$MaxPlantNum > 10,]))
# 
# a1 <- aov(DivEffect ~ typePlot * trt, data = fieldD[fieldD$MaxPlantNum > 10,])
# posthoc <- TukeyHSD(x=a1, 'typePlot', conf.level=0.95)



# #the results tables with parameter estimate (slope), pvalue, and R2adjusted,
# for sla and spad, both singularly (tmp) and with flowering time (tmp2):

modeldata_p<-modeldata[!is.na(modeldata$FH_Wt),]
summary(modeldata_p)
modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
modeldata_p <- modeldata_p[modeldata_p$plotType %!in% "mono",]
# model1<-lmer(perPlantFH_Wt ~ divLevel*trt+PC5dist_mean+FT1001_mean+mean_SLAbv+(1|stripNo), data=modeldata_m)
#
#covariates to test:
# + trt
# divLevel +PC5dist_mean +mean_SLAbv +FT1001_mean

#table to save results
tmp <- c() #plotType, grouping all pred plots together
#divLevel
# tmp2 <- c() #PC5dist_mean
# tmp3 <- c()#mean_SLAbv
# tmp4 <- c()#FT1001_mean
# tmp5 <- c() # meanGenD
# tmp6 <- c() # treeDiv

tmod <- lm(perPlantFH_Wt  ~ trt + trt:plotType, data = modeldata_p[modeldata_p$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

# > tmp
#                       Estimate     Pr(>|t|)      r2adj
# (Intercept)        1.192806e-02 7.299935e-08 -0.0012946
# trtS              -3.907101e-03 1.856965e-01 -0.0012946
# trtC:plotTypepred  9.435373e-05 9.743941e-01 -0.0012946
# trtS:plotTypepred  9.247774e-04 7.623485e-01 -0.0012946


