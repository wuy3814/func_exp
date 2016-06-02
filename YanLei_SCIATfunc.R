### Function Definition

# Correction of mistyped subject number
subjectNoCorrection <- function(x){
  x[x$subject == 1 & x$group != 1,]$subject <- x[x$subject == 1 & x$group != 1,]$group
  return(x)
}
# maleAbility[maleAbility$subject == 1 & maleAbility$group != 1,]$subject <- maleAbility[maleAbility$subject == 1 & maleAbility$group != 1,]$group

# Removal of reminder
removeReminder <- function(x){
  x <- x[x$trialcode != "reminder", ]
}
# Removal of practice trials
removePractice <- function(x){
  x <- x[x$blockcode != "practice_incompatibletest" & x$blockcode != "practice_compatibletest",]
}
# Removal of outliers
excludeOutliers <- function(x, lower = 350){
  x[x$latency <= lower, "latency"] <- -999
  No.Ouliers <- nrow(x[x$latency == -999, ])
  print(No.Ouliers)
  x <- x[x$latency != -999, ]
}
# Error Penalty
errorPenalty <- function(x, penalty = 400){
  correctMeanIncon <- mean(x[x$correct == 1 & x$blockcode == "incompatibletest", "latency"])
  x[x$correct == 0 & x$blockcode == "incompatibletest", "latency"] <- correctMeanIncon + penalty
  correctMeancon <- mean(x[x$correct == 1 & x$blockcode == "compatibletest", "latency"])
  x[x$correct == 0 & x$blockcode == "compatibletest", "latency"] <- correctMeancon + penalty
  return(x)
}
# means and sd
preDStatistic <- function(x){
  compatibleMean <- tapply(x[x$blockcode == "compatibletest", "latency"], x[x$blockcode == "compatibletest", "subject"], mean)
  incompatibleMean <- tapply(x[x$blockcode == "incompatibletest", "latency"], x[x$blockcode == "incompatibletest", "subject"], mean)
  allSd <- tapply(x$latency, x$subject, sd)
  combine <- cbind(compatibleMean, incompatibleMean, allSd)
  return(combine)
}

Dscore <- function(x){
  data <- preDStatistic(x)
  d <- (data[, 2] - data[, 1])/data[, 3]
  return(d)
}

fullScheme <- function(x){
  x <- preparation(x)
  Dscore(x)
}

preparation <- function(x){
  x <- subjectNoCorrection(x)
  x <- removeReminder(x)
  x <- removePractice(x)
  x <- excludeOutliers(x)
  x <- errorPenalty(x)
}

shootCount <- function(x){
  library(plyr)
  library(reshape2)
  x <- preparation(x)
  x$correct <- as.numeric(x$correct)
  x <- ddply(x[, c("subject", "blockcode", "trialcode", "correct")], .(subject, blockcode, trialcode, correct), .fun = summarize, shoot = length(correct))
  x_molten <- melt(x, id = c("subject", "blockcode", "trialcode", "correct"))
  x <- dcast(x_molten, subject~blockcode + trialcode + correct)
  x[is.na(x)] <- 0
  return(x)
}
