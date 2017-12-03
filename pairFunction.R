
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


# RankShift
# These two functions transform raw ranking data into a shifted format, in which each column corresponds to one item, and the element represents
# the rank given to the item
rankshift <- function(x){
  xlength <- length(x)
  xname <- 1:xlength
  names(xname) <- x
  ranking <- as.character(1:xlength)
  out <- as.numeric(xname[ranking])
  return(out)
}

rankshiftOut <- function(x){
  x <- apply(x, 1, rankshift)
  x <- t(x)
  x <- as.data.frame(x)
  return(x)
}
###########

# Simplification
# A simplification function to choose a subset of ranking data, starting from 1 to "lower" (with a default of ending with 1).
# For example, if I want to subset the ranking of 1:4 items from a 1:12 ranking data, I should run simplification(data, 4)
simplification <- function(x, lower = NULL){
  x <- x[x < lower + 1]
  return(x)
}
