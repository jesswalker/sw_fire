replaceIt <- function(pointID, rowNum){
  # focus on point x
  sub <- subset(mins, pointid == pointID)
  
  # set val to NA
  sub$mean[rowNum] <- NA

  # get an approx #
  sub$mean <- na.approx(sub$mean)
    
  # get the date for id'ing other rows
  naDate <- sub$date[rowNum]

  # replace value in mins, for checking in tsOutliers
  #assign(
  #"mins[which(mins$pointid == pointID & mins$date == naDate), 5]", sub$mean[rowNum],
  #envir = .GlobalEnv)
  mins[which(mins$pointid == pointID & mins$date == naDate), 5] <<- sub$mean[rowNum]
  
  # replace value in overall df
  #assign(
  #"df.melt[which(df.melt$pointid == pointID & df.melt$date == naDate), 5]", sub$mean[rowNum],
  #envir = .GlobalEnv())
  
  df.melt[which(df.melt$pointid == pointID & df.melt$date == naDate), 5] <<- sub$mean[rowNum]
}