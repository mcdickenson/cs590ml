# CS 590.01
# Homework 17
# 21 March, 2014
# Matt Dickenson 
# mcd31@duke.edu

# For the squared exponential and 
# two other covariance functions (your choice) 
# plot (function) samples from a Gaussian process. 
# Several plots should be included for each covariance function, 
# varying the number of draws, and the hyperparameters 
# associated with each.

# set up workspace
rm(list=ls())
library(MASS)
set.seed(8675309)

# covariance_functions

# squared exponential covariance function
# try l=1,2,...
squared_expo = function(x1, x2, l=1){
  sigma = matrix(0, nrow=length(x1), ncol=length(x1))
  for (i in 1:nrow(sigma)) {
    for (j in 1:ncol(sigma)) {
      sigma[i,j] = exp( -( ( ( x1[i]-x2[j] )^2 )/ (2 *l^2) ) )
    }
  }
  return(sigma)
}

# rational quadratic covariance function
rational_quad = function(x1, x2, l=1, alpha=0.5) {
  sigma = matrix(0, nrow=length(x1), ncol=length(x1))
  for (i in 1:nrow(sigma)) {
    for (j in 1:ncol(sigma)) {
      r = abs(x1[i]-x2[j])
      sigma[i,j] = ( 1 + (r^2)/(2 * alpha * l^2) )^(-alpha)
    }
  }
  return(sigma)
}

# gamma exponential covariance function
gamma_expo = function(x1, x2, l, gamma=1){
  sigma = matrix(0, nrow=length(x1), ncol=length(x1))
  for (i in 1:nrow(sigma)) {
    for (j in 1:ncol(sigma)) {
      r = abs(x1[i]-x2[j])
      sigma[i,j] = exp( -(r/l)^gamma )
    }
  }
  return(sigma) 
}

plot_gp = function(nsamps=3, mu=0, l=1, alpha=0.5, gamma=1, cvfun="squared exponential"){
  xs = seq(-10,10,length.out=200)
  xlab = paste("x, n=", nsamps, ", mu=", mu, ", l=", l, sep="")

  if(cvfun=="squared exponential"){
    sigma = squared_expo(xs, xs, l)
  } else if(cvfun=="rational quadratic"){
    sigma = rational_quad(xs, xs, l, alpha)
    xlab = paste(xlab, ", alpha=", alpha, sep="")
  } else if(cvfun=="gamma exponential"){
    sigma = gamma_expo(xs, xs, l, gamma)
    xlab = paste(xlab, ", gamma=", gamma, sep="")
  }
  

  # draw samples 
  samps = matrix(rep(0,length(xs)*nsamps), ncol=nsamps)
  for (i in 1:nsamps) {
    samps[,i] <- mvrnorm(1, rep(mu, length(xs)), sigma)
  }
  samps <- cbind(x=xs,as.data.frame(samps))

  # plot function draws
  plot(samps$x, samps$V1, 
    lwd=2,
    ylim=c(-3, 3),
    type="l",
    col="grey60",
    xlab=xlab,
    ylab="f(x)")
  for(i in 3:(nsamps+1)) {
    lines(samps[,1], samps[,i], col=i, lwd=2)
  }
}


for(n in c(3, 5, 10)){
  for(m in c(-1, 0, 1)){
    for(l in c(1, 2, 3)){
      for(cv in c("squared exponential", "rational quadratic", "gamma exponential")){
        if(cv=="squared exponential"){
          filename = paste("n", n, "-m", m, "-l", l, sep="")
          filename = paste(filename, ".pdf", sep="")
          pdf(filename)
            plot_gp(nsamps=n, mu=m, l=l, cvfun=cv)
          dev.off()
        } else if(cv=="rational quadratic"){
          for(a in c(0.5, 2, 3)){
            filename = paste("n", n, "-m", m, "-l", l, sep="")
            filename = paste(filename, "-a", a*2, ".pdf", sep="")
            pdf(filename)
              plot_gp(nsamps=n, mu=m, l=l, alpha=a, cvfun=cv)
            dev.off()
          }
        } else if(cv=="gamma exponential"){
          for(g in c(0.5, 1, 2)){
            filename = paste("n", n, "-m", m, "-l", l, sep="")
            filename = paste(filename, "-g", g*2, ".pdf", sep="")
            pdf(filename)
              plot_gp(nsamps=n, mu=m, l=l, gamma=g, cvfun=cv)
            dev.off()
          }
        }
      }
    }
  }
}


# examples
plot_gp(nsamps=5, mu=0)

plot_gp(nsamps=5, mu=0, cvfun="rational quadratic")

plot_gp(nsamps=5, mu=0, cvfun="gamma exponential")



# cv = "squared exponential"
# cv = "rational quadratic"
cv = "gamma exponential"

for(n in c(3, 5, 10)){
  for(m in c(-1, 0, 1)){
    cat("\\begin{figure}\n")
    cat("\\begin{center}\n")
    for(l in c(1, 2, 3)){
      
      if(cv=="squared exponential"){
        filename = paste("n", n, "-m", m, "-l", l, sep="")
        filename = paste(filename, ".pdf", sep="")
        
        cat(paste("\\includegraphics[scale=0.2]{hw321/", filename, "}\n", sep=""))
        
      } else if(cv=="rational quadratic"){
        for(a in c(0.5, 2, 3)){
          filename = paste("n", n, "-m", m, "-l", l, sep="")
          filename = paste(filename, "-a", a*2, ".pdf", sep="")
          cat(paste("\\includegraphics[scale=0.2]{hw321/", filename, "}\n", sep=""))
        }
      } else if(cv=="gamma exponential"){
        for(g in c(0.5, 1, 2)){
          filename = paste("n", n, "-m", m, "-l", l, sep="")
          filename = paste(filename, "-g", g*2, ".pdf", sep="")
          cat(paste("\\includegraphics[scale=0.2]{hw321/", filename, "}\n", sep=""))
        }
      }
    }
  cat("\\end{center}\n")
  cat("\\end{figure}\n")
}
}


