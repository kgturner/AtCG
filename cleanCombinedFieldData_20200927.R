#clean CombinedFieldData_24Sep2020
#9/27/2020
#KGTurner


####libraries####
library(plyr)
#using lm() in {stats}


####useful functions####

'%!in%' <- function(x,y)!('%in%'(x,y))

####load data####
fieldD <- read.csv('CombinedFieldData_24Sep2020.csv', na.strings=c(""," ","NA"))


####paste in SLAbv####
modeldataSLABV <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA"))
# includes 'mean_SLAbv' variable made from:
# 
# # SLAbv <- read.csv("logSLA_breedingvalues_6June2019.csv", na.strings=c(""," ","NA"))
# # #add SLA breeding values to model data set
# # #for control
# # md_c <- subset(modeldata, trt %in% "C" & plotType %!in% "mono", select = c(1, 3:25))
# # md_c3 <- data.frame(PotID=md_c$PotID)
# # md_c3$temp <- NA
# # for(i in seq_along(md_c)[5:24]) {
# #   md_c3$temp <- SLAbv[match(md_c[,i], SLAbv$lines),2] #col 2 is trtC
# #   names(md_c3)[names(md_c3)=="temp"] <- paste0("SLAcol",i)
# # }
# # md_c3$mean_SLAbv <- rowMeans(md_c3[,2:21], na.rm=TRUE)
# # 
# # #for stress
# # md_s <- subset(modeldata, trt %in% "S" & plotType %!in% "mono", select = c(1, 3:25))
# # md_s3 <- data.frame(PotID=md_s$PotID)
# # md_s3$temp <- NA
# # for(i in seq_along(md_s)[5:24]) {
# #   md_s3$temp <- SLAbv[match(md_s[,i], SLAbv$lines),3] #col 2 is trtC
# #   names(md_s3)[names(md_s3)=="temp"] <- paste0("SLAcol",i)
# # }
# # md_s3$mean_SLAbv <- rowMeans(md_s3[,2:21], na.rm=TRUE)
# # 
# # #for mono
# # library("tidyr")
# # SLAbv2 <- SLAbv %>% gather(trtC, trtS, key = "trt", value = "mean_SLAbv")
# # SLAbv2[SLAbv2$trt %in% "trtC",]$trt <- "C"
# # SLAbv2[SLAbv2$trt %in% "trtS",]$trt <- "S"
# # SLAbv2$trt <- as.factor(SLAbv2$trt)
# # mono <- subset(modeldata, plotType %in% "mono")
# # mono <- merge.data.frame(mono, SLAbv2, by.x=c("lines1", "trt"), by.y=c("lines","trt"), all = TRUE)
# # #319 obvs of 45 var
# # 
# # #merge back!
# # modeldata <- merge.data.frame(modeldata, mono, all.x=TRUE)
# # #429 obs of 45 var

fieldD <- merge.data.frame(fieldD, modeldataSLABV[,c("PotID", "mean_SLAbv")], by="PotID", all.x=TRUE)
# fieldD <- fieldD <- fieldD[order("PotID"),]

###paste in fecundity data####
fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))

fieldD <- merge.data.frame(fieldD, seed_mean[,c("Pot.Number", "seedEstMean")], by.x="PotID", by.y="Pot.Number", all.x=TRUE)

####get rid of extra column####
fieldD <- fieldD[,c(1,3:89)]

####save####
write.csv(fieldD, "CombinedFieldData_27Sep2020.csv")
