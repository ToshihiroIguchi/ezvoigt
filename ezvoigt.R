library(RcppFaddeeva)
library(pso)
library(ggplot2)


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

is.ascending.order <- function(vec){
  true.n <- sum(sort(vec) != vec)
  if(true.n == 0){return(TRUE)}else{return(FALSE)}
}

integral.strength <- function(x, I){
  if(!is.ascending.order(x)){return(NULL)}
  if(length(x) != length(I)){return(NULL)}
  
  num <- length(x)
  
  #Integral Strength
  ret <- sum(diff(x) * (I[-1] + I[-num])/2)
  return(ret)
}

Voigt.opt <- function(x, I, maxit = 300, s = 20, peak.range =NULL,
                      method = "Nelder-Mead", times = 3){
  
  if(is.null(peak.range)){peak.range <- c(min(x), max(x))}
  
  Voigt.rmse <- function(x, I, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2){
    calc <- Voigt2(x, a1, a2, p1, p2, sigma1, sigma2, gamma1, gamma2)$v
    rmse <- sqrt(sum((I - calc)^2)/length(I))
    return(rmse)
  }
  min.fn <- function(y){
    log(Voigt.rmse(x = x, I = I, a1 = y[1], a2 = y[2], p1 = y[3], p2 = y[4],
                 sigma1 = y[5], sigma2 = y[6], gamma1 = y[7], gamma2 = y[8]))
  }
  
  is <- integral.strength(x, I)
  
  lower <- c(rep(0, 2), rep(peak.range[1], 2), rep(1e-10, 4))
  upper <- c(rep(is*2, 2), rep(peak.range[2], 2), rep((max(x) - min(x))/2, 4))
  
  ret <- list()
  ret$value <- Inf
  
  for(i in 1:times){
    
    #The swarm size. Defaults to floor(10+2*sqrt(length(par))) unless type is “SPSO2011” in which case the default is 40.
    #The maximum number of iterations. Defaults to 1000.
    result.pso <- psoptim(par = rep(NA,8), 
                          fn = min.fn,
                          lower = lower, upper = upper,
                          control = list(
                            #trace = 1,trace.stats = TRUE,REPORT = 1,
                            maxit = maxit, s = s
                          ))
    
    result.opt <- try(
      optim(par = result.pso$par, fn = min.fn, method = method), 
      silent = TRUE)
    
    if(class(result.opt) == "try-error"){
      result.opt <- optim(par = result.pso$par, fn = min.fn, method = "Nelder-Mead")
    }
    
    result <- list(integral.strength = is, pso = result.pso, optim = result.opt)
    
    
    if(result.pso$value < result.opt$value || min(result.opt$par[-c(3,4)]) < 0){
      result$par <- result.pso$par
      result$value <- result.pso$value
    }else{
      result$par <- result.opt$par
      result$value <- result.opt$value
    }
    
    if(result$value < ret$value){ret <- result}

  }
  
  
  
  
  class(ret) <- "Voigt.opt"
  return(ret)
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

Voigt.plot <- function(data, text.size = 15){
  #https://qiita.com/kazutan/items/3b982eb589dcc12cee54
  
  gp.common <-  theme(axis.title.x = element_text(size = text.size),
                      axis.title.y = element_text(size = text.size),
                      text = element_text(size = text.size))
  
  ggplot(data, aes(x)) + 
    xlab(NULL) +
    ylab("Intensity") +
    geom_point(aes(y = I), color = "black", size = 1) +
    geom_line(aes(y = v1), color = "blue") +
    geom_line(aes(y = v2), color = "green") +
    geom_line(aes(y = v), color = "red") +
    gp.common
}

Voigt.FWHM <- function(sigma, gamma, method = "BFGS"){
  
  #peak
  peak.I <- Voigt(0, 0, sigma, gamma)
  
  #width to height
  width.height <- function(w){Voigt(abs(w/2), 0, sigma, gamma)}
  
  #minimize fn
  fn <- function(w){
    e2 <- 1e10*(peak.I/2 - width.height(w))^2
    return(e2)
  }
  
  #optimize
  result <- optim(par = sigma+gamma, fn, method = method)

  #return
  ret <- abs(result$par)
  return(ret)
}

table.Voigt.opt <- function(result){
  par <- result$par
  rmse <- exp(result$value)
  
  a1 <- par[1]
  a2 <- par[2]
  p1 <- par[3]
  p2 <- par[4]
  sigma1 <- par[5]
  sigma2 <- par[6]
  gamma1 <- par[7]
  gamma2 <- par[8]
  
  df <- data.frame(Voigt1 = par[c(1,3,5,7)], Voigt2 = par[c(2,4,6,8)])
  df <- rbind(df, 
              data.frame(
                Voigt1 = Voigt.FWHM(sigma1, gamma1), 
                Voigt2 = Voigt.FWHM(sigma2, gamma2)
                )
              )
  df <- rbind(df,
              data.frame(
                Voigt1 = a1*Voigt(0, 0, sigma1, gamma1),
                Voigt2 = a2*Voigt(0, 0 ,sigma2, gamma2)
              ))
  
  
  rownames(df) <- c("Integral_strength", "Peak_position", 
                    "sigma", "gamma", "FWHM", "Peak_intensity")
  return(df)
  
}

summary.Voigt.opt <- function(result){
  
  print(table.Voigt.opt(result))
  
}

read.profile <- function(file){
  ret <- NULL
  if(length(grep("\\.csv$", tolower(file)))){
    ret <- (read.csv(file))
  }
  if(length(grep("\\.asc$", tolower(file)))){
    ret <- (read.table(file, skip = 0))
  }
  if(length(grep("\\.int$", tolower(file)))){
    ret <- (read.table(file, skip = 2))
  }
  
  if(length(ret[1, ]) < 2){return(NULL)}
  return(ret)
}





