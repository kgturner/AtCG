#AtCG biomass and canopy figures
#10/24/2018

library(ggplot2)
library(plyr)

#data
fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
# lineData <- read.csv("chosenLines.csv")

#dataset for graphing
biomass<-fieldData[!is.na(fieldData$FH_Wt),]
canopy <- fieldData[!is.na(fieldData$CanopyArea),]

####scatterplots####
#biomass

pbiomass_div <- ggplot(biomass,aes(divLevel,FH_Wt, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("plot biomass at harvest")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position=c(.9,.9))
  # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass_div

pbiomass_kin <- ggplot(biomass,aes(meanKin,FH_Wt, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot mean kinship")+ylab("plot biomass at harvest")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass_kin

pbiomass_ft <- ggplot(biomass,aes(FT1001_mean,FH_Wt, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("FT1001_mean")+ylab("plot biomass at harvest")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position=c(.9,.9))
pbiomass_ft


#canopy
pcanopy_div <- ggplot(canopy,aes(divLevel,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pcanopy_div

pcanopy_kin <- ggplot(canopy,aes(meanKin,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot mean kinship")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pcanopy_kin

pcanopy_ft <- ggplot(canopy,aes(FT1001_mean,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("FT1001_mean")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position=c(.9,.9))
pcanopy_ft

#avg by div level
#for plots of div level means
biomass_mean <- ddply(biomass, .(divLevel, trt), summarize, 
               divBiomass=mean(FH_Wt,na.rm = TRUE))

pbiomass.2 <- ggplot(biomass_mean,aes(divLevel,divBiomass,color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("biomass")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() 
  # theme(legend.position="none")
  # ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass.2

pbiomass.3 <- ggplot(biomass_mean,aes(trt,divBiomass,color=divLevel, group=divLevel))+
  # geom_errorbar(aes(ymin=lCL, ymax=uCL),color="black", width=.1, position=position_dodge(0.1))+
  geom_line()+geom_point(size=3) + #facet_grid(. ~ Trt)
  # geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("treatment")+ylab("biomass")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() 
# theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass.3

#predicted and kin
pbiomass_kin_pred <- ggplot(biomass,aes(meanKin,FH_Wt, color=trt))+geom_point(aes(shape=plotType, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot mean kinship")+ylab("plot biomass at harvest")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() 
  # theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass_kin_pred

# ####make multi figs####

# pdf("AtCG_biomass.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
png("AtCG_biomass.png",width=665, height = 400, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pbiomass_div, pbiomass_kin,pbiomass_ft, cols=3) 
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