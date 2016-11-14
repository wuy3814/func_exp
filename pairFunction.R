
# These functions are for converting ranking data to Thurstonian pairwise comparison data. 
# Out of n items there will be [n(n-1)]/2 pairs. Data should be the ranking of n items, each response corresponds to the item numbering.
# Data requirement: 
# 1. No missing cases. 
# 2. No out-of-bounds cases. 

rankToPair <- function(x){
    n <- length(x)
    m <- n - 1
    designMatrix <- matrix(nrow = n, ncol = n)
    for(i in 1:m) {
      for(j in (i+1):n){
        if(x[i] < x[j]) {designMatrix[x[i], x[j]] <- 1}
        else {designMatrix[x[j], x[i]] <- 0}
      }
    } 
  return(designMatrix)
}

rankToPairOut <- function(x){
  x <- rankToPair(as.numeric(x)) # Use as.numeric() to force data.frame rows into a vector. Useful as in apply() function.
  x <- x[upper.tri(x)]
  return(x)
}

