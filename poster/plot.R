rm(list=ls())
setwd('~/github/olympifier')

# set up workspace
library(gplots)


# load matrices

################################
# plotting function
myHeatmap = function(matrix){
  heatmap(matrix, Rowv=NA, Colv=NA,
    col=cm.colors(256), 
    # col=rev(heat.colors(256)),
    # scale="none",
    scale="col",
    margins=c(5,10))
}

################################
# neural net
# sportANN = nnet(trnData, ideal,
#   size=30,
#   MaxNWts=1500,
#   softmax=TRUE,
#   # censored=TRUE,
#   skip=TRUE,
#   maxit=1000)
# save(sportANN, file="rcode/sportANN.rda")
load("rcode/sportANN.rda")

trnPred = predict(sportANN, trnData, type="class")
table(trnPred, trnClass)
1-mean(trnPred == trnClass)

tstPred = predict(sportANN, tstData, type="class")
table(tstPred, tstClass)
1-mean(tstPred == tstClass)

sportANNmatrix = table(tstClass, tstPred)

# plot
myHeatmap(sportANNmatrix)
# todo: rotate

heatmap.2(sportANNmatrix,
  Rowv=NA, Colv=NA,
  col=cm.colors(256),
  scale="col",
  dendrogram="none",
  key=FALSE)
# todo: remove margins (or move labels to opposite sides)