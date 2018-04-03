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
pred10df$plotType <- "pred"
pred10df[11:20,]$plotType <- "div"
pred10df$plotNo <- c(11:20,71:80)

for (i in names(pred10df)[1:10]){
  csx <- subset(pool60, index %in% pred10df[,i], select=c(index, CS_number))
  pred10df_cs <-dataframe(csx$CS_number)
}



for (i in pred10index){
  
  indexv <- subset(pool60, index %in% pred10index[,i], select=c(index, CS_number))
  pred10df <- c(pred10index, list(as.integer(indexv$index)))
}


