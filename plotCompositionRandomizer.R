#generate random number sets with limited overlap
#3/28/2018

####generate random integers from a range####
sample.int(60, 10)
#[1] 34 42 45 54  3  5 20 25 53 40
#such as ranks of populations from a list

plot71 <- sample.int(60,10)
# plot72 <- sample.int(60,10)
# plot73 <- sample.int(60,10)
# 
# plist <- list(plot71=plot71, plot72=plot72, plot73=plot73)
# names(plist)


###pairwise comparisons
plot71 %in% plot73
#[1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

####function to check matching/add to list####
#first make list with starter plot
# plot71 <- sample.int(60,10)
plist <- list(plot71=plot71)

generate_plot <- function(plotList, pool=60, size=10, overlap=4) {
  testplot <- sample.int(pool,size)
  for(i in 1:length(plotList)) {
    n <- testplot %in% plotList[[i]]
    if (sum(n, na.rm=TRUE)>overlap) {
      fail <- TRUE
    } 
  }
  if (fail==FALSE) {return(testplot)}
}

# #function to make, check, add sngle new plot to list
# generate_plot <- function(plotList, pool=60, size=10, vs) {
#   testplot <- sample.int(pool,size)
#   n <- testplot %in% plotList[[vs]]
#   if (sum(n, na.rm=TRUE)>4) {
#     return("no good")
#   } else {
#     newPlotList <- c(plotList, list(testplot)) #plotNo.advancing=testplot)
#   } 
# }

####while list to run enough times to generate plot assignments####
while (length(plist) < 20) {
  fail <- FALSE
  plist <- c(plist, list(generate_plot(plist)))
}


####import predicted plots and make list of vectors####
pool60 <- read.delim("chosenLines.csv", header=TRUE, sep=",")
pool60$index <- rownames(pool60)

predplots <- read.delim("PredictedCombs.csv", header=TRUE, sep=",")
pred10 <- predplots[1:10,11:20]
# pred10indexv11 <- subset(pool60, CS_number %in% pred10[,1], select=c(index, CS_number))
# pred10v11 <- as.integer(pred10indexv11$index)
# plist <- list(pred10v11=pred10v11)

pred10index <- NULL
for (i in names(pred10)){
  indexv <- subset(pool60, CS_number %in% pred10[,i], select=c(index, CS_number))
  pred10index <- c(pred10index, list(as.integer(indexv$index)))
}
preplot_byindex <- pred10index #this list contains vectors that are the index values for the prediction plots only

####run function to generate random diversity plots####
#generate plot defaults pool=60, size=10, overlap=4

while (length(pred10index) < 20) {
  fail <- FALSE
  pred10index <- c(pred10index, list(generate_plot(pred10index)))
}
#pred10index is now a list of both the prediction and diversity plots of 10 genotypes

####convert index numbers to CS numbers in df####
pred10df <- data.frame(matrix(unlist(pred10index),nrow=20, byrow=T))

pred10df_cs <- pred10df
pred10df_cs[] <- pool60$CS_number[match(unlist(pred10df),pool60$index)]

pred10df_cs$plotType <- "pred"
pred10df_cs[11:20,]$plotType <- "div"
pred10df_cs$plotNo <- c(11:20,71:80)

####save prediction and diversity 10 plot composition####
write.table(pred10df_cs, "pred_div_10composition.txt", sep="\t")

####repeat all steps for 20 plot####
# pool60 <- read.delim("chosenLines.csv", header=TRUE, sep=",")
# pool60$index <- rownames(pool60)
# 
# predplots <- read.delim("PredictedCombs.csv", header=TRUE, sep=",")
pred20 <- predplots[1:20,21:30]

pred20index <- NULL
for (i in names(pred20)){
  indexv <- subset(pool60, CS_number %in% pred20[,i], select=c(index, CS_number))
  pred20index <- c(pred20index, list(as.integer(indexv$index)))
}
preplot_byindex20 <- pred20index #this list contains vectors that are the index values for the prediction plots only

#generate plot defaults pool=60, size=10, overlap=4
#length of pred20index should be 20 (because 10 predicition plots and 10 diversity plots)

#for some reason this generates some null vectors... so overshoot for 30

while (length(pred20index) < 30) {
  fail <- FALSE
  pred20index <- c(pred20index, list(generate_plot(pred20index, size=20, overlap=9)))
}
#pred20index is now a list of both the prediction and diversity plots of 20 genotypes

#remove nulls
pred20index <- pred20index[-which(sapply(pred20index, is.null))]

pred20df <- data.frame(matrix(unlist(pred20index),nrow=20, byrow=T))

pred20df_cs <- pred20df
pred20df_cs[] <- pool60$CS_number[match(unlist(pred20df),pool60$index)]

pred20df_cs$plotType <- "pred"
pred20df_cs[11:20,]$plotType <- "div"
pred20df_cs$plotNo <- c(21:30,81:90)

write.table(pred20df_cs, "pred_div_20composition.txt", sep="\t")

####calculate average overlap among plots in a diversity level####
tengeno <- read.delim("pred_div_10composition.txt", header =T)
tengenoM <- as.matrix(tengeno[,(1:10)])
tengenoM_diff <- outer(1:nrow(tengenoM), 1:nrow(tengenoM), FUN=Vectorize(function(x,y)
  sum(tengenoM[x,]!=tengenoM[y,])))
# tengenoM_diff[lower.tri(tengenoM_diff)] <- 0
tengenoM_diff
sum(tengenoM_diff)
# [1] 3682
dim(tengenoM_diff)
# [1] 20 20
# sum(tengenoM_diff)/200
sum(tengenoM_diff)/400
# [1] 9.205
10-sum(tengenoM_diff)/400
# [1] 0.795


twengeno <- read.delim("pred_div_20composition.txt", header =T)
twengenoM <- as.matrix(twengeno[,(1:20)])
twengenoM_diff <- outer(1:nrow(twengenoM), 1:nrow(twengenoM), FUN=Vectorize(function(x,y)
  sum(twengenoM[x,]!=twengenoM[y,])))
twengenoM_diff
sum(twengenoM_diff)
# [1] 7310
dim(twengenoM_diff)
# [1] 20 20
sum(twengenoM_diff)/400
# [1] 18.275
20-sum(twengenoM_diff)/400
# [1] 1.725

twogeno <- read.delim("pred_div_02composition.txt", header =T)
twogenoM <- as.matrix(twogeno[,(2:3)])
twogenoM_diff <- outer(1:nrow(twogenoM), 1:nrow(twogenoM), FUN=Vectorize(function(x,y)
  sum(twogenoM[x,]!=twogenoM[y,])))
# twogenoM_diff[lower.tri(twogenoM_diff)] <- 0
twogenoM_diff
sum(twogenoM_diff)
# [1] 750
dim(twogenoM_diff)
# [1] 20 20
# sum(twogenoM_diff)/200
sum(twogenoM_diff)/400
# [1] 1.875
2-sum(twogenoM_diff)/400
# [1] 0.125


####calc avg number of plots per genotype####
twengeno_freq <- as.data.frame(table(unlist(twengenoM)))
# sum(twengeno_freq$Freq)/60
mean(twengeno_freq$Freq)
# [1] 6.666667
sd(twengeno_freq$Freq)
# [1] 2.259944

tengeno_freq <- as.data.frame(table(unlist(tengenoM)))
mean(tengeno_freq$Freq)
# [1] 3.508772
sum(tengeno_freq$Freq)/60 #include 3 genotypes not included in 10-div level plots
# [1] 3.333333
tenfreq <- as.vector(c(tengeno_freq$Freq, 0,0,0))
sd(tenfreq)
# [1] 2.080309

twogeno_freq <- as.data.frame(table(unlist(twogenoM)))
mean(twogeno_freq$Freq)
# [1] 1.142857
sum(twogeno_freq$Freq)/60 #include 3 genotypes not included in 10-div level plots
# [1] 0.6666667
twofreq <- as.vector(c(twogeno_freq$Freq, 0,0,0))
sd(twofreq)
# [1] 0.4619213
