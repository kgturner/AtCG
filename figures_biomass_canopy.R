#AtCG biomass (and canopy figures)
#10/24/2018

library(ggplot2)
library(plyr)

#data
# fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
# lineData <- read.csv("chosenLines.csv")
modeldata <- read.delim("modeldata_20190828.txt", na.strings=c(""," ","NA")) #see construction in REML_biomass.R. Made from:
modeldata_m<-modeldata[!is.na(modeldata$FH_Wt),]
summary(modeldata_m)
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum


names(modeldata_m)[5] <- "Treatment"
modeldata_m$Treatment <- revalue(modeldata_m$Treatment, c("C"="High Resource", "S"="Low Resource"))

#colors
library(RColorBrewer)
jesse <- brewer.pal(9, 'RdBu')[c(1,9)] #this will give low then high colors
# # The palette with grey:
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # The palette with black:
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# # tmod <- lm(perPlantFH_Wt  ~ trt + trt:divLevel, data = modeldata_m[modeldata_m$MaxPlantNum > 10,] )
# tmp <- c() #divLevel
# tmp3 <- c()#mean_SLAbv
# tmp4 <- c()#FT1001_mean
# 


#####biomass - scatterplots####
# pbiomass_div <- ggplot(modeldata_m,aes(divLevel,perPlantFH_Wt, color=trt))+ #geom_point() + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   geom_jitter(aes(shape=trt, color=trt), size=3) +
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot diversity level")+ylab("per plant biomass at harvest")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() +
#   theme(legend.position=c(.9,.9))
#   # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pbiomass_div

# pPerPlantMass_div <- ggplot(modeldata_m,aes(divLevel,perPlantFH_Wt, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot diversity")+ylab("per plant biomass at harvest")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() +
#   theme(legend.position=c(.9,.9))
# # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pPerPlantMass_div

# pbiomass_kin <- ggplot(biomass,aes(meanKin,FH_Wt, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot mean kinship")+ylab("plot biomass at harvest")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() +
#   theme(legend.position="none")
# # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pbiomass_kin
# 
pbiomass_ft <- ggplot(modeldata_m[modeldata_m$MaxPlantNum > 10,],aes(FT1001_mean,perPlantFH_Wt, color=Treatment))+ 
  geom_point(aes(shape=Treatment, color=Treatment), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  scale_colour_manual(values=jesse)+
  # geom_jitter(aes(shape=trt, color=trt), size=3) +
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title = "(a)", x = "flowering time mean (days)", y = "stand biomass at harvest (g)")+
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 24) +
  theme(legend.position="none") +  #c(.85,.85)
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank())
pbiomass_ft

pbiomass_SLA <- ggplot(modeldata_m[modeldata_m$MaxPlantNum > 10,],aes(mean_SLAbv,perPlantFH_Wt, color=Treatment))+ 
  geom_point(aes(shape=Treatment, color=Treatment), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  scale_colour_manual(values=jesse)+
  # geom_jitter(aes(shape=trt, color=trt), size=3) +
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title ="(b)", x = bquote('') , y = "stand biomass at harvest (g)")+
  xlab(expression(SLA~BV~(ln~(cm^{"2"}/mg)))) + 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 24) +
  theme(legend.position="none") +   #c(.85,.85)
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank())
pbiomass_SLA


####canopy####
pcanopy_div <- ggplot(modeldata_c,aes(divLevel,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pcanopy_div

pPerPlantcanopy_div <- ggplot(modeldata_c,aes(divLevel,perPlantCanopy, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("per plant canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pPerPlantcanopy_div

# pcanopy_kin <- ggplot(canopy,aes(meanKin,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot mean kinship")+ylab("plot canopy area")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() +
# theme(legend.position="none")
# # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pcanopy_kin
# 
# pcanopy_ft <- ggplot(canopy,aes(FT1001_mean,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("FT1001_mean")+ylab("plot canopy area")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() +
#   theme(legend.position=c(.9,.9))
# pcanopy_ft

####avg by div level####
# #for plots of div level means
# biomass_mean <- ddply(biomass, .(divLevel, trt), summarize, 
#                divBiomass=mean(FH_Wt,na.rm = TRUE))
# 
# pbiomass.2 <- ggplot(biomass_mean,aes(divLevel,divBiomass,color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot diversity")+ylab("biomass")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() 
#   # theme(legend.position="none")
#   # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pbiomass.2
# 
# pbiomass.3 <- ggplot(biomass_mean,aes(trt,divBiomass,color=divLevel, group=divLevel))+
#   # geom_errorbar(aes(ymin=lCL, ymax=uCL),color="black", width=.1, position=position_dodge(0.1))+
#   geom_line()+geom_point(size=3) + #facet_grid(. ~ Trt)
#   # geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("treatment")+ylab("biomass")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() 
# # theme(legend.position="none")
# # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pbiomass.3
# 
# #predicted and kin
# pbiomass_kin_pred <- ggplot(biomass,aes(meanKin,FH_Wt, color=trt))+geom_point(aes(shape=plotType, color=trt), size=3) + #facet_grid(. ~ Trt)
#   geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
#   #   coord_cartesian(ylim = c(0, 1.02)) +
#   xlab("plot mean kinship")+ylab("plot biomass at harvest")+ 
#   #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
#   theme_bw() 
#   # theme(legend.position="none")
# # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
# pbiomass_kin_pred



####biomass - prediction plots####

modeldata_p <- merge.data.frame(modeldata, predtype[,c(1,8)], by="PotID")
# modeldata_p<-modeldata_p[!is.na(modeldata$FH_Wt),]
# modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
modeldata_p <- modeldata_p[modeldata_p$plotType %!in% "mono",]
levels(modeldata_p$typePlot)<-c(levels(modeldata_p$typePlot),"div")  #Add the extra level to your factor
modeldata_p$typePlot[is.na(modeldata_p$typePlot)] <- "div"           #Change NA to "div"
modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
summary(modeldata_p)

library(plyr)

names(modeldata_p)[5] <- "Treatment"
# modeldata_p$Treatment <- revalue(modeldata_p$Treatment, c("C" = "High", "S" = "Low"))
modeldata_p$Treatment <- revalue(modeldata_p$Treatment, c("High" = "High Resource", "Low" = "Low Resource"))



pbiomass_pred <- ggplot(modeldata_p[modeldata_p$MaxPlantNum > 10,],aes(typePlot, perPlantFH_Wt))+
  stat_boxplot(geom = 'errorbar', width = 0.2)+ #, linetype="dotted"
  geom_boxplot()+
  facet_grid(. ~ Treatment) +
  # geom_point(aes(shape=typePlot, color=typePlot), size=3) 
  # geom_jitter(aes(shape=typePlot, color=typePlot), size=3) +
  # geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title = "(a)", x = "Prediction type", y = "stand biomass at harvest")+   #title ="Prediction stands" , 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 16) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank())  #panel.border = element_blank(),
# theme(legend.position=c(.9,.8))
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass_pred


png("AtCG_biomass_pred.png",width=665, height = 400, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
# multiplot(pbiomass_div, pbiomass_kin,pbiomass_ft, cols=3)
pbiomass_pred
dev.off()


# ####make multi figs####

# pdf("AtCG_biomass_fec.pdf", useDingbats=FALSE,width=12.65, height=5, pointsize = 4)
png("AtCG_biomass_fec.png",width=1265, height = 500, pointsize = 40)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pbiomass_ft, pbiomass_SLA,pseedEst_div, cols=3) 
dev.off()

# pdf("AtCG_Pred_biomass_fec.pdf", useDingbats=FALSE,width=18.65, height=6, pointsize = 12)
png("AtCG_Pred_biomass_fec.png",width=865, height = 1000, pointsize = 40)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pbiomass_pred, pseedEst_pred, cols=1) 
dev.off()


# pdf("AtCG_canopy.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
png("AtCG_canopy.png",width=665, height = 400, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pcanopy_div, pcanopy_kin,pcanopy_ft, cols=3) 
dev.off()

# pdf("AtCG_biomass_mean.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
png("AtCG_biomass_mean.png",width=665, height = 300, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pbiomass.2, pbiomass.3, pbiomass_kin_pred, cols=3) 
dev.off()


####multiplot func####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}