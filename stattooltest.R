leverage<-function(xi, dataset, display="both"){
    n<-length(dataset)
    devisum<-sum((dataset-mean(dataset))^2)
    hii<-(1/n)+(xi-mean(dataset))^2/devisum
    hiia<-hii-(1/n)
    if(display=="both") c(hii=hii, hii_ast=hiia)
    else if(display=="hii")  hii
    else if(display=="hiia")  hiia          
}



## The leverage function could be accompanied by lapply and sapply function.

partial.etas<-function(F, df1, df2){
         eta.squared<-df1*F/(df1*F+df2)
         eta.squared
}

centering<-function(x){
      x-mean(x)
}

# These are used to calculate sample size for Anova

ptemp<-function(alpha, power, df1,f,k){
  myp<-function(ncp){
    N<-ncp/(f^2)
    df2<-N-k
    abs(qf(1-power, df1, df2, ncp)-qf(1-alpha, df1, df2))
  }
}

Ndetermine<-function(alpha, power, df1, f, k){
ptemp2<-ptemp(alpha,power,df1,f,k)
#lam<-optim(2,ptemp2)$par
lam<-optimize(ptemp2, c(0,1000),tol=0.00001)$minimum
N<-lam/f^2
return(N)
}


cohen.f<-function(F, df1, N){
  cohen.f<-sqrt((df1*F)/N)
  return(cohen.f)
}

#This one is used to calculate observed power through literature

Ob.power<-function(alpha,df1, df2, f,N){
  lam<-f^2*N
  power<-1-pf(qf(1-alpha, df1, df2), df1, df2, ncp=lam)
  return(power)
}


# Fisher transformation for ICC
# the k here refers to the number of raters
# the r here refers to the ICC value
FisherICCz<-function(k, r){ 
  .5*log((1+(k-1)*r)/(1-r))
}
FisherICCr<-function(k, z){
  (exp(2*z)-1)/(k+exp(2*z)-1)
}

## Calculation of COhen's D using the safest way, despite the homogeneity 
## or heterogeneity of variances

sp<-function(n1,s1, n2, s2){  ## the function for calculating Pooled SD
  spool<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
  return(spool)
}

cohenD <- function(mu1, mu2, sp){
  d <- (mu1-mu2)/sp
  return(d)
}
