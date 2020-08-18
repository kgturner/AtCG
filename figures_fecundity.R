#AtCG fecundity figures
#7/23/2019

library(ggplot2)
library(plyr)

#data
# fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
# lineData <- read.csv("chosenLines.csv")

modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA"))
fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))
modeldata_f <- merge.data.frame(modeldata, seed_mean, by.x="PotID", by.y="Pot.Number", all.x=TRUE)

modeldata_f<-modeldata_f[!is.na(modeldata_f$seedEstMean),] #lose 18 damaged pots

#divLevel
#trt

names(modeldata_f)[5] <- "Treatment"
modeldata_f$Treatment <- revalue(modeldata_f$Treatment, c("C"="High Resource", "S"="Low Resource"))
modeldata_f$Treatment <- revalue(modeldata_f$Treatment, c("High Resource"="High", "Low Resource"="Low"))


####seedEstMean####
pseedEst_div <- ggplot(modeldata_f[modeldata_f$MaxPlantNum > 10,],aes(divLevel,seedEstMean, color=Treatment))+
  # geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_jitter(aes(shape=Treatment, color=Treatment), size=3) +
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  scale_colour_manual(values=jesse)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title ="(c)" , x = "no. of genotypes in stand", y = "estimated seed no. per fecund plant")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 24) +
  theme(legend.position=c(.45,.70)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
    strip.background = element_blank())+
  labs(color="Resource treatment", shape="Resource treatment")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pseedEst_div


# png("AtCG_fecundity.png",width=665, height = 400, pointsize = 12)
# # svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
# # multiplot(pbiomass_div, pbiomass_kin,pbiomass_ft, cols=3) 
# pseedEst_div
# dev.off()

####fecundity - prediction plots####
modeldata_p <- merge.data.frame(modeldata, predtype[,c(1,8)], by="PotID")
# modeldata_p<-modeldata_p[!is.na(modeldata$FH_Wt),]
# modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
modeldata_p <- modeldata_p[modeldata_p$plotType %!in% "mono",]
levels(modeldata_p$typePlot)<-c(levels(modeldata_p$typePlot),"div")  #Add the extra level to your factor
modeldata_p$typePlot[is.na(modeldata_p$typePlot)] <- "div"           #Change NA to "div"
summary(modeldata_p)

library(plyr)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))
modeldata_pf <- merge.data.frame(modeldata_p, seed_mean, by.x="PotID", by.y="Pot.Number", all.x=TRUE)

modeldata_pf<-modeldata_pf[!is.na(modeldata_pf$seedEstMean),]
modeldata_pf <- modeldata_pf[modeldata_pf$typePlot%!in%"LF",] #lose 2 pots - Not enough of this plot type represented in fecundity data

names(modeldata_pf)[5] <- "Treatment"
modeldata_pf$Treatment <- revalue(modeldata_pf$Treatment, c("C"="High Resource", "S"="Low Resource"))


pseedEst_pred <- ggplot(modeldata_pf[modeldata_pf$MaxPlantNum > 10,],aes(typePlot, seedEstMean))+
  stat_boxplot(geom = 'errorbar', width = 0.2)+ #, linetype="dotted"
  geom_boxplot()+
  facet_grid(. ~ Treatment) +
  # geom_point(aes(shape=typePlot, color=typePlot), size=3) 
  # geom_jitter(aes(shape=typePlot, color=typePlot), size=3) +
  # geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title ="(b)" , x = "Prediction type", y = "estimate seed per fecund plant in each stand")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank())  #panel.border = element_blank(),
  # theme(legend.position=c(.9,.8))
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pseedEst_pred


png("AtCG_fecundity_pred.png",width=665, height = 400, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
# multiplot(pbiomass_div, pbiomass_kin,pbiomass_ft, cols=3)
pseedEst_pred
dev.off()
