### Function Definition
## Used to calculate D score and counts of correct response for the data generated in SC-IAT program in INQUISIT

# Correction of mistyped subject number
subjectNoCorrection <- function(x){
  x[x$subject == 1 & x$group != 1,]$subject <- x[x$subject == 1 & x$group != 1,]$group
  return(x)
}

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


### Used to exclude subjects with abnormally little trials or too many trials
## Here I applied Mean plus/minus 3 SD standards. Might consider using MAD statistics in the future.
cleanErrorSubject <- function(x){
  xtable <- table(x$subject)
  meantable <- mean(xtable)
  sdtable <- sd(xtable)
  upp <- meantable + 3 * sdtable
  low <- meantable - 3 * sdtable
  lowSubject <- as.numeric(names(xtable[xtable < low]))
  uppSubject <- as.numeric(names(xtable[xtable > upp]))
  x <- x[!(x$subject %in% lowSubject | x$subject %in% uppSubject), ]
  return(x)
}

###
## A function to transform named vector to a matrix with names as a column
matricize <- function(x){
  matrix(c(as.numeric(names(x)), x), nrow = length(x))
}

framize <- function(x){
  x <- matricize(x)
  ## Transform matricized data into data.frame, and assaign column names
  x <- data.frame(x)
  colnames(x) <- c("subject", "value")
  return(x)
}

###
# This function is used to combine Yanlei's 2*2 Mixed design data
mixedMerge <- function(O, OI, OO, factorize = FALSE){
  O_OI <- merge(framize(O), framize(OI), by = "subject")
  O_OI$IO <- 1
  O_OO <- merge(framize(O), framize(OO), by = "subject")
  O_OO$IO <- 0
  mixed <- rbind(O_OI, O_OO)
  colnames(mixed)[2:3] <- c("Pre", "Crossed")
  if(factorize == TRUE){
    mixed$IO <- factor(mixed$IO, levels = c(1, 0), labels = c("I", "O"))
  }
  return(mixed)
}

