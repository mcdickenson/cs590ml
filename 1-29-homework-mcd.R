# CS 590.01
# Homework 1
# 29 January, 2014
# Matt Dickenson 
# mcd31@duke.edu

# What happens as you vary the DP concentration parameter (alpha)? 
# As alpha varies, the balance between classes shifts. In the final two rows of the plots, the third class is almost undetectable.

# What happens as you vary the Normal-Inverse Wishart hyperparameters? 
# S affects the correlation between features. In the second half of the plots, the correlation between features is much more apparent.
# mu_0 shifts the means of the features.
# lambda impacts the scale (magnitude) of the features.
# v affects the variance of the features.

library(MCMCpack) # for Dirichlet and Wishart

set.seed(8675309)

gmm_sampler = function(n, alpha, mu_0, lambda, S, v, legend=FALSE){
  # n: number of samples
  # alpha: parameter for Dirichlet
  # mu_0: k-length vector of hyperparams for mu
  # lambda: scalar for mu 
  # v: k-length vector of scalars for Inverse Wishart
  # S: k pxp inverse scale matrices

  K=length(alpha)
  p = ncol(S)

  Sigma = array(NA, dim=c(p,p,K))
  mu = matrix(NA, nrow=K,ncol=p)

  # sample mu and Sigma from Normal-Inverse Wishart
  for(k in 1:K){
    Sigma[,,k] = solve(rwish(v, solve(S)))
    mu[k,] = mvrnorm(1, mu_0, Sigma[,,k])
  }

  # sample probability of each class from Dirichlet
  pis = rdirichlet(1, alpha) 

  # sample latent indicators of class 
  z = rep(NA, n)
  x = matrix(NA, nrow=n, ncol=p)

  # sample each x from the Gaussian dist for its class 
  for(i in 1:n){
    z[i] = sample(seq(1:K), size=1, prob=pis)
    x[i,] = mvrnorm(1, mu[z[i],], Sigma[,,z[i]])
  }

  plot(x[,1], x[,2], col=z,
    xlab=paste("mu_0=", liststr(mu_0), ", lambda=", lambda, sep=""),
    ylab=paste("alpha=", liststr(alpha), ", v=", v, sep=""),
    main=paste("S=", liststr(as.vector(S)), sep="")
  )

  if(legend){
    legend("topright", 
      legend=c("Class 1", "Class 2", "Class 3"), 
      pch=16, 
      col=c(1:K))
  }
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
# alpha = c(0.9, 0.9, 0.9)
# mu_0 = c(0,0)
# lambda = 1
# S = matrix(c(1,0,0,1), nrow=2, ncol=2)
# solve(S)
# solve(rwish(v, solve(S)))
# v = 5 
# gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=TRUE)

# output figures: 
setwd("~/desktop")
pdf("plots-mcd.pdf", width=20, height=40)
par(mfrow=c(12,6))
n = 1000
mus = matrix(c(0,0,-5,5,5,10), ncol=2, byrow=TRUE)
alphas = matrix(c(c(0.9, 0.9, 0.9), c(1,2,2), c(10, 10, 1)), ncol=3, byrow=TRUE)

S = matrix(c(1,0,0,1), nrow=2, ncol=2)

for(a in 1:nrow(alphas)){
  for(v in c(5, 10)){
    for(m in 1:nrow(mus)){
      for(lambda in c(1, 5)){
        alpha = alphas[a,]
        mu_0 = mus[m,]
        gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
      }
    }
  }
}

S = matrix(c(1,.5,.5,1), nrow=2, ncol=2)

for(a in 1:nrow(alphas)){
  for(v in c(5, 10)){
    for(m in 1:nrow(mus)){
      for(lambda in c(1, 5)){
        alpha = alphas[a,]
        mu_0 = mus[m,]
        gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
      }
    }
  }
}
dev.off()


