#AtCG biomass and canopy figures
#10/24/2018

library(ggplot2)
library(plyr)

#data
fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
lineData <- read.csv("chosenLines.csv")

#dataset for graphing
biomass<-fieldData[!is.na(fieldData$FH_Wt),]
canopy <- fieldData[!is.na(fieldData$CanopyArea),]

#scatterplots
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

pcanopy_div <- ggplot(canopy,aes(divLevel,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pcanopy_div

pcanopy_kin <- ggplot(biomass,aes(meanKin,CanopyArea, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot mean kinship")+ylab("plot canopy area")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pcanopy_kin

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

pbiomass.3 <- ggplot(biomass_mean,aes(trt,divBiomass,color=divLevel))+geom_point(aes(shape=trt, color=divLevel), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("treatment")+ylab("biomass")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() 
# theme(legend.position="none")
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pbiomass.3

# ####make multi figs####

pdf("AtCG_biomass_canopy.pdf", useDingbats=FALSE,width=6.65, height=9, pointsize = 12)
# png("KTurnerFig3.png",width=665, height = 900, pointsize = 12)
# svg("KTurnerFig3.svg", width=6.65, height=9, pointsize = 12)
multiplot(pbiomass_div, pbiomass_kin,pcanopy_div, pcanopy_kin, cols=2) 
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