rm(list=ls())
setwd('~/github/olympifier')

# set up workspace
library(evtree)
library(gplots)
library(mclust)
library(nnet)
library(party)
library(RColorBrewer)
library(randomForest)


# load data
athletes = read.csv('data/athletes-clean.csv', header=TRUE, as.is=TRUE)
athletes = athletes[-which(athletes$Sport=="Athletics"),]
athletes$sport.factor = as.factor(athletes$Sport)
athletes$event.factor = as.factor(athletes$FirstEvent)

# set up training and test sets
features = c("Age", "Height", "Weight", "Female")
target = "sport.factor"
trnData = athletes[athletes$kfold <= 5, features]
tstData = athletes[athletes$kfold >  5, features]
trnClass = athletes[athletes$kfold <= 5, target]
tstClass = athletes[athletes$kfold >  5, target]


# load matrices
models = c("sportMclust", "sportCIT", "sportEV", "sportRF", "sportANN")


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
    xlab="Predicted Sport",
    ylab="Actual Sport",
    margins=c(10,1,10)
  )
}
# sportANNmatrix
myHeatmap(sportANNmatrix)

?heatmap


scale = function(){
  data = matrix(c(rep(0,11), seq(0, 1, by=0.1), rep(0,11)), nrow=11, ncol=3)
  heatmapNew(data, Rowv=NA, Colv=NA,
    col=colfunc(100), 
    # main="Legend",
    # col=rev(heat.colors(256)),
    # scale="row",
    # labRow=NA,
    # labCol=NA,
    scale="col",
    # ylab="Actual Sport",
    # xlab="Predicted Sport",
    margins=c(0,0,0),
    add.expr=text(x=2, y=1:11, labels=as.character(seq(0, 1, by=0.1)), cex=2)
  )
}

pdf("graphics/scale.pdf")
scale()
dev.off()



################################
# get accuracy and plot

modelCheck = function(mname){
  cat("Model:", mname, "\n")
  mpath = paste("rcode/", mname, ".rda", sep="")
  cat(mpath, "\n")
  model = get(load(mpath))

  if(mname=="sportANN"){
    type="class"
  } else {
    type="response"
  }

  if(mname=="sportMclust"){
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


