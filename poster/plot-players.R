rm(list=ls())
setwd('~/github/australian-football')

# set up workspace
library(evtree)
library(gplots)
library(mclust)
library(nnet)
library(party)
library(RColorBrewer)
library(randomForest)


# load data
players = read.csv('data/players.csv', as.is=TRUE)
players$league.factor = as.factor(players$league)
nrow(players)
odds = seq(1, nrow(players), by=2)
evens = seq(2, nrow(players), by=2)
features = c("age", "height", "weight")
target = "league.factor"
trnData = players[odds,  features]
tstData = players[evens, features]
trnClass = players[odds,  target]
tstClass = players[evens, target]
formula = league.factor ~ age + height + weight
data = cbind(trnData, trnClass)
names(data)[4] = target


# load matrices
models = c("playersMclust", "playersCIT", "playersEV", "playersRF", "playersANN")


################################
# plotting function
source("~/github/cs590ml/poster/heatmapNew.R")
colfunc = colorRampPalette(c("white", "steelblue"))
myHeatmap = function(matrix){

  heatmapNew(matrix[nrow(matrix):1,], Rowv=NA, Colv=NA,
    col=colfunc(100), 
    cexRow = 0.3 + 1/log10(nrow(matrix)),
    cexCol = 0.3 + 1/log10(ncol(matrix)),
    scale="row",
    xlab="Predicted League",
    ylab="Actual League",
    margins=c(10,1,10)
  )
}
# playersANNmatrix
myHeatmap(playersANNmatrix)



################################
# get accuracy and plot

modelCheck = function(mname){
  cat("Model:", mname, "\n")
  mpath = paste("rcode/", mname, ".rda", sep="")
  cat(mpath, "\n")
  model = get(load(mpath))

  if(mname=="playersANN"){
    type="class"
  } else {
    type="response"
  }

  if(mname=="playersMclust"){
    trnPred = predict(model, trnData, type=type)$classification
    tstPred = predict(model, tstData, type=type)$classification
  } else {
    trnPred = predict(model, trnData, type=type)
    tstPred = predict(model, tstData, type=type)
  }

  # training data
  mtrx = table(trnClass, trnPred)
  filename = paste("graphics/", mname, "-trn.pdf", sep="")
  pdf(filename)
    myHeatmap(mtrx)
  dev.off()
  acc1 = mean(trnPred==trnClass)
  cat("\tTrain accuracy:", acc1, "\n")

  # test data
  mtrx = table(tstClass, tstPred)
  filename = paste("graphics/", mname, "-tst.pdf", sep="")
  pdf(filename)
    myHeatmap(mtrx)
  dev.off()
  acc2 = mean(tstPred==tstClass)
  cat("\tTest accuracy:", acc2, "\n")

  rat = acc2/acc1
  cat("\tRatio:", rat, "\n")
}

for(m in models){
  modelCheck(m)
}

dev.off()

# todo: fix league labels in plots

