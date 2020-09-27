#clean biomass, canopy, fecundity, prediction figures
#9/27/2020
#KGTurner

#libraries

library(RColorBrewer)
library(ggplot2)
library(plyr)

####useful functions####
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

####load data####
fieldD <- read.csv('CombinedFieldData_27Sep2020.csv', na.strings=c(""," ","NA"))

#colors
jesse <- brewer.pal(9, 'RdBu')[c(1,9)] #this will give low then high colors


####model data####
modeldata_m<-fieldD[!is.na(fieldD$FH_Wt),]
modeldata_m$perPlantFH_Wt <- modeldata_m$FH_Wt/modeldata_m$MaxPlantNum
names(modeldata_m)[5] <- "Treatment"
modeldata_m$Treatment <- revalue(modeldata_m$Treatment, c("C"="High Resource", "S"="Low Resource"))


modeldata_f<-fieldD[!is.na(fieldD$seedEstMean),]
names(modeldata_f)[5] <- "Treatment"
modeldata_f$Treatment <- revalue(modeldata_f$Treatment, c("C"="High Resource", "S"="Low Resource"))
modeldata_f$Treatment <- revalue(modeldata_f$Treatment, c("High Resource"="High", "Low Resource"="Low"))


modeldata_p <- fieldD[fieldD$plotType %!in% "mono",]
levels(modeldata_p$typePlot)<-c(levels(modeldata_p$typePlot),"div")  #Add the extra level to your factor
modeldata_p$typePlot[is.na(modeldata_p$typePlot)] <- "div"           #Change NA to "div"
modeldata_p$perPlantFH_Wt <- modeldata_p$FH_Wt/modeldata_p$MaxPlantNum
names(modeldata_p)[5] <- "Treatment"
modeldata_p$Treatment <- revalue(modeldata_p$Treatment, c("High" = "High Resource", "Low" = "Low Resource"))


modeldata_pf<-modeldata_p[!is.na(modeldata_p$seedEstMean),]
modeldata_pf <- modeldata_pf[modeldata_pf$typePlot%!in%"LF",] #lose 2 pots - Not enough of this plot type represented in fecundity data


####plots####

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


pseedEst_div <- ggplot(modeldata_f[modeldata_f$MaxPlantNum > 10,],aes(divLevel,seedEstMean, color=Treatment))+
  # geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_jitter(aes(shape=Treatment, color=Treatment), size=3) +
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  scale_colour_manual(values=jesse)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  labs(title ="(c)" , x = "no. of genotypes in stand", y = "mean total fruit length (mm)")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw(base_size = 24) +
  theme(legend.position=c(.45,.70)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        strip.background = element_blank())+
  labs(color="Resource treatment", shape="Resource treatment")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pseedEst_div


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


####figures####

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
