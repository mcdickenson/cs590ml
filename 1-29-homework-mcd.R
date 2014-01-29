# CS 590.01
# Homework 1
# 29 January, 2014
# Matt Dickenson 
# mcd31@duke.edu

# What happens as you vary the DP concentration parameter (alpha)? 
# Higher values of alpha lead to more classes present (higher values of K). 

# What happens as you vary the Normal-Inverse Wishart hyperparameters? 
# S affects the correlation between features. In the second half of the plots, the correlation between features is much more apparent.
# mu_0 shifts the means of the features.
# lambda impacts the scale (magnitude) of the features.
# v affects the variance of the features.

library(MCMCpack) # for Dirichlet and Wishart

gmm_sampler = function(n, alpha, mu_0, lambda, S, v, legend=FALSE){
  # n: number of samples
  # alpha: scalar parameter for Dirichlet process
  # mu_0: p-length vector of hyperparams for mu
  # lambda: scalar for mu 
  # v: scalar for Inverse Wishart
  # S: pxp inverse scale matrix

  z = restaurant(alpha, n) # Dirichlet process table assignments
  K = length(unique(z))
  p = ncol(S)  

  # sample mu and Sigma from Normal-Inverse Wishart
  Sigma = array(NA, dim=c(p,p,K))
  mu = matrix(NA, nrow=K,ncol=p)
  for(k in 1:K){
    Sigma[,,k] = solve(rwish(v, solve(S)))
    mu[k,] = mvrnorm(1, mu_0, Sigma[,,k])
  }

  # sample each x from the Gaussian dist for its class 
  x = matrix(NA, nrow=n, ncol=p)
  for(i in 1:n){
    x[i,] = mvrnorm(1, mu[z[i],], Sigma[,,z[i]])
  }

  plot(x[,1], x[,2], col=z,
    xlab=paste("mu_0=", liststr(mu_0), ", lambda=", lambda, sep=""),
    ylab=paste("alpha=", alpha, ", v=", v, sep=""),
    main=paste("S=", liststr(as.vector(S)), sep="")
  )

  if(legend){
    legend("topright", 
      legend=paste("Class ", seq(1:K), sep=""), 
      pch=16, 
      col=c(1:K))
  }
}


restaurant = function(alpha, n){
  table_counts = c(1) # number of 'customers' at each 'table'
                      # first customer at first table
  table_assignments = c(1, rep(NA, n-1))

  for(m in 2:n){
    tmp = c(table_counts, alpha)
    table_props = tmp/sum(tmp)

    table_m = sample(c(1:length(tmp)), 1, prob=table_props)
    if(table_m==length(tmp)){ table_counts[table_m] = 1}
    else{ table_counts[table_m] = table_counts[table_m] + 1}
    table_assignments[m] = table_m 
  }
  return(table_assignments) 
}

liststr=function(mylist){
  str = paste("(", mylist[1], sep="")
  for(i in mylist[2:length(mylist)]){
    str = paste(str, ", ", i, sep="")
  }
  str = paste(str, ")", sep="")
  return(str)
}

# n = 1000
# alpha = 1
# mu_0 = c(0,0)
# lambda = 1
# S = matrix(c(1,0,0,1), nrow=2, ncol=2)
# v = 5 
# solve(S)
# solve(rwish(v, solve(S)))

# gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=TRUE)

# output figures: 
setwd("~/desktop")
pdf("1-29-plots-mcd.pdf", width=20, height=40)
par(mfrow=c(12,6))
n = 1000
mus = matrix(c(0,0,-5,5,5,10), ncol=2, byrow=TRUE)
# alphas = matrix(c(c(0.9, 0.9, 0.9), c(1,2,2), c(10, 10, 1)), ncol=3, byrow=TRUE)

S = matrix(c(1,0,0,1), nrow=2, ncol=2)

for(alpha in c(1,5,10)){
  for(v in c(5, 10)){
    for(m in 1:nrow(mus)){
      for(lambda in c(1, 5)){
        mu_0 = mus[m,]
        gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
      }
    }
  }
}

S = matrix(c(1,.5,.5,1), nrow=2, ncol=2)

for(alpha in c(1,5,10)){
  for(v in c(5, 10)){
    for(m in 1:nrow(mus)){
      for(lambda in c(1, 5)){
        mu_0 = mus[m,]
        gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
      }
    }
  }
}
dev.off()


