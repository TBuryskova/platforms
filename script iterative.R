library(tidyverse)

br_user<- function(w, lambda) {
  threshold <- lambda * log(cosh(1 / lambda))
  
  q_H=  ifelse(
    w < threshold,
    {
      numerator <- 2 - (exp((-1 + w)/lambda) + exp((1 + w)/lambda))
      denominator <- 2*exp(2*w/lambda) - (exp((-1 + w)/lambda) + exp((1 + w)/lambda))
      val <- 1 / (1 + (numerator / denominator) * exp((-1 + w)/lambda))
      pmin(pmax(val, 0), 1)  # clamp between 0 and 1
    },
    0
  )
q_L =  ifelse(
    w < threshold,
    {
      numerator <- 2 - (exp((-1 + w)/lambda) + exp((1 + w)/lambda))
      denominator <- 2*exp(2*w/lambda) - (exp((-1 + w)/lambda) + exp((1 + w)/lambda))
      val <- 1 / (1 + (numerator / denominator) * exp((1 + w)/lambda))
      pmin(pmax(val, 0), 1)  # clamp between 0 and 1
    },
    0
  )
return(c(q_H,q_L))

}


br_platform <- function(q_H,q_L) {
  if(q_H+q_L==0) {
    w=1
    lambda=0
  } else if(q_H+q_L==0.5) {
    w=1
    lambda=1
  } else{
  
  w_grid <- seq(0, 1, length.out = 300)
  lambda_grid <- seq(0.01, 1, length.out = 300)
  
  w_lambda <- expand.grid(w = w_grid, lambda = lambda_grid) %>%
  
    mutate(profit=w*0.5*(q_H+q_L)+lambda*(
      q_H * log(2*q_H / (q_H+q_L)) + (1 - q_H) * log(2*(1 - q_H) / (1 - (q_H+q_L))) +
        q_L * log(2*q_L / (q_H+q_L)) + (1 - q_L) * log(2*(1 - q_L) / (1 - (q_H+q_L))))
  
    )
  opt_row <-  w_lambda[which.max(w_lambda$profit), ]
  w <- opt_row$w
  lambda <- opt_row$lambda
  }
  return(c(w, lambda))
  
  
}