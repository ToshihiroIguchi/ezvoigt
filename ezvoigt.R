library(RcppFaddeeva)
library(ggplot2)

library(DEoptim)
library(pso)
library(GA)

source("pboptim.R")
source("randomsearch.R")


as.vec <- function(df){as.vector(as.matrix(df))}

Voigt2 <- function(x, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2){
  #2つのフォークト関数を作る
  v1.r <- Voigt(x, p1, sigma1, gamma1)
  v2.r <- Voigt(x, p2, sigma2, gamma2)
  
  #面積をかける
  v1 <- a1 * v1.r
  v2 <- a2 * v2.r
  
  #足してデータフレームを返す
  ret <- data.frame(x = x, v1 = v1, v2 = v2, v = v1 + v2)
  return(ret)
}

Voigt.opt <- function(x, I, maxit = 100, s = 16){
  Voigt.rmse <- function(x, I, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2){
    calc <- Voigt2(x, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2)$v
    rmse <- sqrt(sum((I - calc)^2)/length(I))
    return(rmse)
  }
  min.fn <- function(y){
    (Voigt.rmse(x = x, I = I, a1 = y[1], a2 = y[2], p1 = y[3], p2 = y[4],
                 sigma1 = y[5], sigma2 = y[6], gamma1 = y[7], gamma2 = y[8]))
  }
  lower <- c(rep(0, 2), rep(min(x), 2), rep(1e-5, 4))
  upper <- c(rep(sum(I), 2), rep(max(x), 2), rep(max(x) - min(x), 4))
  
  #The swarm size. Defaults to floor(10+2*sqrt(length(par))) unless type is “SPSO2011” in which case the default is 40.
  #The maximum number of iterations. Defaults to 1000.
  
  result.pbo <- pboptim(fn = min.fn,
                        method = c("DEO","PSO","GA","RS"),
                        population = 20, generation = 100,
                        lower = lower, upper = upper, trace = FALSE)

  result.opt <- optim(par = as.vec(result.pbo$bestpar),
                      fn = min.fn)

  result <- list(pso = result.pbo, optim = result.opt)

  
  if(result.pbo$bestvalue < result.opt$value){
    result$par <- result.pbo$bestpar
    result$value <- result.pbo$bestvalue
  }else{
    result$par <- result.opt$par
    result$value <- result.opt$value
  }
  
  print(result)
  
  return(result)
}

Voigt.df <- function(x, I, par){
  a1 <- par[1]
  a2 <- par[2]
  p1 <- par[3]
  p2 <- par[4]
  sigma1 <- par[5]
  sigma2 <- par[6]
  gamma1 <- par[7]
  gamma2 <- par[8]
  ret.df <- Voigt2(x, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2)

  ret.df <- data.frame(ret.df, I = I)
  return(ret.df)
}

Voigt.plot <- function(data){
  #https://qiita.com/kazutan/items/3b982eb589dcc12cee54
  ggplot(data, aes(x)) + 
    xlab(NULL) +
    ylab("Intensity") +
    geom_point(aes(y = I), color = "black", size = 1) +
    geom_line(aes(y = v1), color = "blue") +
    geom_line(aes(y = v2), color = "green") +
    geom_line(aes(y = v), color = "red")
}




