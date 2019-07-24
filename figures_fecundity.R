#AtCG fecundity figures
#7/23/2019

library(ggplot2)
library(plyr)

#data
# fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
# lineData <- read.csv("chosenLines.csv")
modeldata <- read.delim("modeldata_20190711.txt", na.strings=c(""," ","NA"))
fecundityData <- read.delim("Field-Fecundity-DATA-2018.txt", header = T)

seed_mean <- ddply(fecundityData, .(Pot.Number), summarize, 
                   seedEstMean=mean(Seed_Num_estimate),potTotSiliqueNum=sum(Total.Slique.Number),numFecPlants=length(Pot.Number))
modeldata_f <- merge.data.frame(modeldata, seed_mean, by.x="PotID", by.y="Pot.Number", all.x=TRUE)
modeldata_f<-modeldata_f[!is.na(modeldata_f$seedEstMean),] #lose 18 damaged pots


####scatterplots####
#seedEstMean

pseedEst_div <- ggplot(modeldata_f,aes(divLevel,seedEstMean, color=trt))+geom_point(aes(shape=trt, color=trt), size=3) + #facet_grid(. ~ Trt)
  geom_smooth(method=glm, se=TRUE)+ #ylim(0,1)+
  #   coord_cartesian(ylim = c(0, 1.02)) +
  xlab("plot diversity")+ylab("seedEstMean")+ 
  #   annotate(geom="text", x=-4, y=3.5, label="(a)",fontface="bold", size=5)+
  theme_bw() +
  theme(legend.position=c(.9,.9))
# ggtitle("(f)")+theme(plot.title = element_text(lineheight=2, face="bold",hjust = 0))
pseedEst_div
