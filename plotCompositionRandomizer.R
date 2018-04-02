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

####while list to run enough times to generate plot assignments####
while (length(plist) < 20) {
  fail <- FALSE
  plist <- c(plist, list(generate_plot(plist)))
}

#function to make, check, add sngle new plot to list
generate_plot <- function(plotList, pool=60, size=10, vs) {
  testplot <- sample.int(pool,size)
  n <- testplot %in% plotList[[vs]]
  if (sum(n, na.rm=TRUE)>4) {
    return("no good")
  } else {
    newPlotList <- c(plotList, list(testplot)) #plotNo.advancing=testplot)
  } 
}

####import predicted plots####
pool60 <- read.delim("chosenLines.csv", header=TRUE, sep=",")
predplots <- read.delim("PredictedCombs.csv", header=TRUE, sep=",")
pred10 <- predplots[,11-20]
