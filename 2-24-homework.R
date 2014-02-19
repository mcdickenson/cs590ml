# Code up the generative model for the Indian Buffet Process. 
library(ggplot2)

# generative model for ibp
ibp = function(alpha, N){
  assignments = matrix(NA, nrow=N, ncol=N*alpha) 
    # extreme upper bound on expected number of columns
  
  dishes = rpois(1, alpha)
  zeroes = ncol(assignments) - dishes
  
  assignments[1,] = c(rep(1, dishes), rep(0, zeroes))
  
  for(i in 2:N){
    last_previously_sampled_dish = 
      max(which(assignments==1, arr.ind=T)[, 'col'])
    
    dishes_from_previously_sampled = 
      matrix(NA, nrow=1, ncol=last_previously_sampled_dish)
    for(k in 1:last_previously_sampled_dish){
      m_k = sum(assignments[1:i-1,k])
      prob = m_k / i
      dishes_from_previously_sampled[1,k] = rbinom(1,1, prob)
    }
    
    new_dishes = rpois(1, alpha/i)
    zeroes = ncol(assignments)-(last_previously_sampled_dish + new_dishes)
    assignments[i,] = c(dishes_from_previously_sampled, 
      rep(1,new_dishes), rep(0, zeroes))
  }
  
  last_sampled_dish = max(which(assignments==1, arr.ind=T)[, 'col'])
  assignments = assignments[1:N, 1:last_sampled_dish]
  
  return(assignments)
}

# convert a matrix to its left-ordered form
lof = function(matrix){
  binary_digits = rep(NA, ncol(matrix))
  for(i in 1:ncol(matrix)){
    col = matrix[,i]
    val = binary(rev(col))
    binary_digits[i] = val
  }
  return(matrix[,rev(order(binary_digits))])
}

# turn a binary vector into a base-10 number
binary = function(vec){ 
  sum = 0
  for(i in 1:length(vec)){
    sum = sum + (2^(i-1) * vec[i]) 
  }
  return(sum)
}

# What happens as you vary hyperparameter alpha and N? 
# Submit plots (generated binary matrices) to back up your claims.
makePlot = function(alpha, n){
  x = lof(ibp(alpha, n))

  
  # reshape data for use by ggplot
  binary = as.data.frame(as.table(t(x[n:1,] )))
  
  # generate plot
  plot = ggplot(binary)
  plot = plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + 
    scale_x_discrete(name="Dishes", labels=c(1:ncol(x))) + 
    scale_y_discrete(name="Customers", labels=c(nrow(x):1)) + 
    scale_fill_gradient2(breaks=seq(from=0, to=1, by=.2), guide=FALSE)
  print(plot)
}

setwd('~/github/cs590ml')
pdf("alpha5n5.pdf", height=3, width=3)
  makePlot(5, 5)
dev.off()

pdf("alpha5n10.pdf", height=3, width=3)
  makePlot(5, 10)
dev.off()

pdf("alpha10n5.pdf", height=3, width=3)
  makePlot(10, 5)
dev.off()

pdf("alpha10n10.pdf", height=3, width=3)
  makePlot(10, 10)
dev.off()


