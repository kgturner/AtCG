#REML models of biomass
#8/27/2018

#using lme4 v1.1-18-1
#needs R 3.2+, using R v3.4.0
#needs Matrix 1.2-1, using Matrix v1.2-10
# update.packages("Matrix")
#need to reinstall RccpEigen and minqua to avoid seg fault
install.packages("minqa", "RcppEigen")
#new version lme4
install.packages("lme4")

library("lme4")

#data
fieldData <- read.csv("FieldDataWithDiversityInfo.csv")
lineData <- read.csv("chosenLines.csv")

#full model
modeldata<-fieldData[!is.na(fieldData$FH_Wt),]
# model1<-lmer(FH_Wt ~ divLevel*trt+PC1+(1|stripNo), family=gaussian,data=modeldata)
# model2<-lmer(FH_Wt ~ divLevel+trt+PC1+(1|stripNo), family=gaussian,data=modeldata)

model3<-lmer(FH_Wt ~ divLevel+trt+(1|stripNo), data=modeldata)
model3.1<-lmer(FH_Wt ~ trt+(1|stripNo), data=modeldata)
model3.2<-lmer(FH_Wt ~ divLevel*trt+(1|stripNo), data=modeldata)
model3.3<-lmer(FH_Wt ~ divLevel+(1|stripNo), data=modeldata)

# model4 <- lmer(FH_Wt ~ trt+PC1+(1|stripNo), family=gaussian,data=modeldata)
# model5<-lmer(FH_Wt ~ divLevel+PC1+(1|stripNo), family=gaussian,data=modeldata)


# (a1 <- anova(model2,model1)) # is interaction sig?
# (a2 <- anova(model3,model2)) # is PC1 covariate sig?
# (a3 <- anova(model4, model2)) #is divLevel sig?
# (a4 <- anova(modeltrt, model2)) #is trt sig?

(a_3 <- anova(model3.1,model3)) # is divLevel sig?
(a_1 <- anova(model3,model3.2)) # is interaction sig?
(a_2 <- anova(model3.3,model3)) # is trt sig?

########example using lme4.0#####
exprs.LR<- function(trait,df,cov, family=gaussian){
  modeldata<-df[!is.na(df[[trait]]),]
  
  #browser()
  
  model1<-lmer(modeldata[[trait]]  ~ Origin*Trt+modeldata[[cov]]+ (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model2<-lmer(modeldata[[trait]]  ~ Origin+Trt+ modeldata[[cov]] + (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model3<-lmer(modeldata[[trait]]  ~ Origin+Trt + (Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  model4 <- lmer(modeldata[[trait]]  ~ Trt+ modeldata[[cov]]+(Tmpt|PopTrtPool)+(1|Pop), family,data=modeldata)
  
  a1 <- anova(model2,model1) # is interaction sig?
  a2 <- anova(model3,model2) # is covariate sig?
  a3 <- anova(model4, model2) #is origin sig?
  
  pval <- as.data.frame(cbind(Contig=trait,intLRT=a1[[7]][2],covLRT=a2[[7]][2],originLRT=a3[[7]][2]))
  
  return(pval)
}

#test!
test <- read.table("test_lme4dat.txt", header=T, sep="\t") 

test <- exprs.df[, c(1:20)]
test.LRT <- do.call(rbind,lapply(names(test)[15:20],function(n) exprs.LR(n,df=test,cov="Latitude")))#apply func to all things in list
