# CS 590.01
# Homework 1
# 29 January, 2014
# Matt Dickenson 
# mcd31@duke.edu

# Code up the generative model (i.e. generate data from) 
# for a Dirichlet process mixture of Gaussians (preferably in R or Matlab), 
# using a Normal-Inverse Wishart prior on the Gaussian parameters.

# What happens as you vary the DP concentration parameter (alpha)? 
# What happens as you vary the Normal-Inverse Wishart hyperparameters? 
# Along with your code, submit plots of your generated data to back up your answers.

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
  # print(Sigma)
  mu = matrix(NA, nrow=K,ncol=p)

  # sample mu and Sigma from Normal-Inverse Wishart
  for(k in 1:K){
    Sigma[,,k] = solve(rwish(v, solve(S)))
    # print("val:")
    # print(Sigma[,,k])
    mu[k,] = mvrnorm(1, mu_0, Sigma[,,k])
    # mu[k] = rnorm(1, mu_0, Sigma[,,k])
    # print(mu[k,])
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



n = 1000
alpha = c(0.9, 0.9, 0.9)
mu_0 = c(0,0)
lambda = 1
S = matrix(c(1,0,0,1), nrow=2, ncol=2)
solve(S)
solve(rwish(v, solve(S)))
v = 5 
gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=TRUE)


dev.off()
par(mfrow=c(12,6))

# set up hyper params 
S1 = matrix(c(1,0,0,1), nrow=2, ncol=2)
S2 = matrix(c(.9, .4, .4, .9), nrow=2, ncol=2)
Ss = list(S1, S2)

n = 1000
S = matrix(c(1,0,0,1), nrow=2, ncol=2)
dim(S)
for(alpha in c(c(0.9, 0.9, 0.9), c(1,2,2), c(10, 10, 1))){
  for(v in c(5,10)){
    # for(mu_0 in c(c(0,0), c(-5, 5), c(1,10))){
      for(lambda in c(1, 5)){
        # print(S)
        # print("v is:")
        # print(v)
        print("mu0")
        print(mu_0)
        gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
      }
    }
  }
}
for(mu_0 in c(c(0,0), c(-5, 5), c(1,10))){
  print(mu_0)
}

for(v in c(1,5)){
  print(v)
}

# for(S in c(matrix(c(1,0,0,1), nrow=2, ncol=2), matrix(c(1,0,0,1), nrow=2, ncol=2)))


mus = matrix(c(0,0,-5,5,5,10), ncol=2, byrow=TRUE)
mus

S = matrix(c(1,0,0,1), nrow=2, ncol=2)
mu_0 = c(0,0)
dim(S)
for(alpha in c(c(0.9, 0.9, 0.9), c(1,2,2), c(10, 10, 1))){
  for(v in c(5,10)){
    for(lambda in c(1, 5)){
      for
      # print(S)
      # print("v is:")
      # print(v)
      print("mu0")
      print(mu_0)
      gmm_sampler(n, alpha, mu_0, lambda, S, v, legend=FALSE)
    }  
  }
}

dev.off()

setwd("~/desktop")
pdf("tmp.pdf", width=20, height=40)
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



