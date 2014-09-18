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
