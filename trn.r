###########
# Script for estimating Mean and SD of a normal distribution with one side censoring or truncation
# 
# Reference: 
# Cohen, A. C. (1959). Simplified estimators for the normal distribution when samples are singly censored or truncated. Technometrics, 1(3), 217-237.
###########

#######
# For singly truncated data
Z <- function(x){
  y <- dnorm(x)/(1 - pnorm(x))
  return(y)
}

theta <- function(x){
  y <- Z(x)/(Z(x) - x)
  return(y)
}

check <- function(xbar, x0, ss){
  check <- ss/(xbar - x0)^2
  return(check)
}

fitfunction_gen  <- function(checkout){
  fitfunction <- function(x){
    abs((1 - Z(x) * (Z(x) - x))/(Z(x) - x)^2 - checkout)}
}


thetaEst <- function(x){
  f <- fitfunction_gen(x)
  t <- theta(optimize(f, c(-200, 20), tol = .0001)$minimum)
  return(t)
}

########
# For singly censored data
lamda <- function(h, x){
  Y <- Ygen(h)
  qq <- Y(x)/(Y(x) - x)
  return(qq)
}


Ygen <- function(h){
  Y <- function(x){
    (h/(1 - h)) * Z(-x)}
}

censorfit_gen <- function(checkout, h){
  Y <- Ygen(h)
  fitfunction <- function(x){
    abs((1 - Y(x) * (Y(x) - x))/(Y(x) - x)^2 - checkout)}
}

lambdaEst <- function(checkout, h){
  fc <- censorfit_gen(checkout, h)
  lam <- lamda(h, optimize(fc, c(-3000, 10), tol = .0001)$minimum)
  return(lam)
}


# Example from Alliger et al. (1986)
JK <- c(rep(2, 2), rep(3, 8), rep(4, 51), rep(5, 86), rep(6, 90), rep(7, 56))
JKc <- c(rep(2, 2), rep(3, 8), rep(4, 51), rep(5, 86), rep(6, 90))
check(mean(JKc), 7, var(JKc))

l <- lambdaEst(check(mean(JKc), 7, var(JKc)), length(JK[JK == 7])/length(JK))

mean(JKc) - l * (mean(JKc) - 7)
sqrt(var(JKc) + l * (mean(JKc) - 7)^2)
