#clean biomass, canopy, fecundity, prediction analysis
#9/27/2020
#KGTurner

####libraries####
library(plyr)
#using lm() in {stats}

####useful functions####
'%!in%' <- function(x,y)!('%in%'(x,y))

####load data####
fieldD <- read.csv('CombinedFieldData_27Sep2020.csv', na.strings=c(""," ","NA"))

####clean dataset####
#480 obvs of 89 var
#remove non-NA plots that were trampled/dug up/washed out
modeldata <- subset(fieldD, Damaged_pot %!in% "damaged")
#429 obvs of 89 var
modeldata <- modeldata[,2:89]
# #429 obvs of 88 var

####analysis####
####LM models - biomass####
modeldata_m<-modeldata[!is.na(modeldata$FH_Wt),]
summary(modeldata_m)
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum

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

tmod2 <- lm(perPlantFH_Wt  ~ trt + trt:PC5dist_mean, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

tmod3 <- lm(perPlantFH_Wt  ~ trt + trt:mean_SLAbv, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

tmod4 <- lm(perPlantFH_Wt  ~ trt + trt:FT1001_mean, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

tmod5 <- lm(perPlantFH_Wt  ~ trt + trt:meanGenD, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

tmod6 <- lm(perPlantFH_Wt  ~ trt + trt:treeDiv, data = modeldata_m[modeldata_m$MaxPlantNum > 10,])
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6

####LM models - canopy area####
#405 obs. 88 var
modeldata_c<-modeldata[!is.na(modeldata$CanopyArea),]
modeldata_c$perPlantCanopy <- modeldata_c$CanopyArea/modeldata_c$MaxPlantNum

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

tmod2 <- lm(perPlantCanopy  ~ trt + trt:PC5dist_mean, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

tmod3 <- lm(perPlantCanopy  ~ trt + trt:mean_SLAbv, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

tmod4 <- lm(perPlantCanopy  ~ trt + trt:FT1001_mean, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

tmod5 <- lm(perPlantCanopy  ~ trt + trt:meanGenD, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

tmod6 <- lm(perPlantCanopy  ~ trt + trt:treeDiv, data = modeldata_c[modeldata_c$MaxPlantNum > 10,])
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6

####fecundity####
modeldata_f<-modeldata[!is.na(modeldata$potF2),]
#202 obs. 88 var

#table to save results
tmp <- c() #divLevel
tmp2 <- c() #PC5dist_mean
tmp3 <- c()#mean_SLAbv
tmp4 <- c()#FT1001_mean
tmp5 <- c() # meanGenD
tmp6 <- c() # treeDiv

tmod <- lm(potF2  ~ trt + trt:divLevel, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

tmod2 <- lm(potF2  ~ trt + trt:PC5dist_mean, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

tmod3 <- lm(potF2  ~ trt + trt:mean_SLAbv, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp3 <- rbind(tmp3, cbind(summary(tmod3)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod3)[[9]]))
tmp3

tmod4 <- lm(potF2  ~ trt + trt:FT1001_mean, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp4 <- rbind(tmp4, cbind(summary(tmod4)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod4)[[9]]))
tmp4

tmod5 <- lm(potF2  ~ trt + trt:meanGenD, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp5 <- rbind(tmp5, cbind(summary(tmod5)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod5)[[9]]))
tmp5

tmod6 <- lm(potF2  ~ trt + trt:treeDiv, data = modeldata_f[modeldata_f$MaxPlantNum > 10,] )
tmp6 <- rbind(tmp6, cbind(summary(tmod6)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod6)[[9]]))
tmp6


####prediction####
modeldata_p<-modeldata[!is.na(modeldata$FH_Wt),]
modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
modeldata_p <- modeldata_p[modeldata_p$plotType %!in% "mono",]
levels(modeldata_p$typePlot)<-c(levels(modeldata_p$typePlot),"div")  #Add the extra level to your factor
modeldata_p$typePlot[is.na(modeldata_p$typePlot)] <- "div"           #Change NA to "div"

tmp <- c() #plotType, grouping all pred plots together

tmod <- lm(perPlantFH_Wt  ~ trt + trt:plotType, data = modeldata_p[modeldata_p$MaxPlantNum > 10,] )
tmp <- rbind(tmp, cbind(summary(tmod)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod)[[9]]))
tmp

tmp2 <- c() #typePlot, div vs each type of pred plot

tmod2 <- lm(perPlantFH_Wt  ~ trt + trt:typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,] )
tmp2 <- rbind(tmp2, cbind(summary(tmod2)[[4]][,c('Estimate', 'Pr(>|t|)')], r2adj = summary(tmod2)[[9]]))
tmp2

anova(lm(perPlantFH_Wt ~ trt*typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,]))

a1 <- aov(perPlantFH_Wt ~ trt *typePlot, data = modeldata_p[modeldata_p$MaxPlantNum > 10,])
a1

posthoc <- TukeyHSD(x=a1, 'typePlot', conf.level=0.95)
posthoc
