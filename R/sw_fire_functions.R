#    -------------- savePlot -------------------

#  save plots

savePlot <- function(myPlot) {
  pdf(myPlot)
  print(myPlot)
  dev.off()
}



# ----------- getStats ----------------------

# summarized stats for a df and column
# https://stackoverflow.com/questions/40811562/dplyr-object-not-found-median-only

getStats <- function(df, col) {
  #col <- deparse(substitute(col))  # to pass a string argument, comment out this line
  funs <- c("n", "mean", "stats::median", "sd", "se")
  dots <- c(sprintf("sum(!is.na(%s))", col),
            sprintf("%s(%s, na.rm = TRUE)", funs[2:4], col),
            "sd/sqrt(n)")
  names(dots) <- gsub("^.*::", "", funs)
  summarize_(df, .dots = dots)
}



# --------- function convert to dataframe
convert_to_df <- function(df) {
  df <- ldply(df)
  colnames(df)[1] <- 'ptid'
}



#  --------- replaceIt
#
# crappy function to replace outlier.  Rewrite

replaceIt <- function(pointID, rowNums){
  
  # focus on given point ID
  sub <- subset(minVals, pointid == pointID)
  
  # set val to NA
  sub$mean[rowNums] <- NA
  
  # get an approx #
  sub$mean <- na.approx(sub$mean)
  
  # get the date for id'ing rows in df.melt
  naDate <- sub$date[rowNums]
  
  # replace value in mins, for checking in tsOutliers
  minVals$mean[which(minVals$pointid == pointID)] <<- replace(minVals$mean[which(minVals$pointid == pointID)], rowNums, sub$mean[rowNums]) 
   
  # replace value in df.melt
  df.melt$mean[which(df.melt$pointid == pointID & df.melt$date %in% naDate)] <<- replace(df.melt$mean[which(df.melt$pointid == pointID & df.melt$date %in% naDate)], seq(1:length(rowNums)), sub$mean[rowNums])

  # note subtlety of "<<-" assignment option. That assigns a global environment to the minVals and df.melt files.
  
}


#  --------- replaceIt2
#
# replace outliers

replaceIt2 <- function(pointID, rowNums, rowVals){
  
  # focus on given point ID
  sub <- subset(minVals, pointid == pointID)
  
  # set val to NA
  sub$mean[rowNums] <- rowVals[rowNums]
  
  # get the date for id'ing rows in df.melt
  rowDates <- sub$date[rowNums]
  
  # replace value in mins, for checking in tsOutliers
  minVals$mean[which(minVals$pointid == pointID)] <<- replace(minVals$mean[which(minVals$pointid == pointID)], rowNums, sub$mean[rowNums]) 
  
  # replace value in df.melt
  df.melt$mean[which(df.melt$pointid == pointID & df.melt$date %in% rowDates)] <<- replace(df.melt$mean[which(df.melt$pointid == pointID & df.melt$date %in% rowDates)], seq(1:length(rowNums)), sub$mean[rowNums])
  
  # note subtlety of "<<-" assignment option. That assigns a global environment to the minVals and df.melt files.
  
}








# -------------------- tsOutliers -------------------------

# Find time series outliers (plot options)
# http://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series
# Rob Hyndman

tsOutliers <- function(x, plot=FALSE)
{
  x <- as.ts(x)
  if (frequency(x) > 1) {
    resid <- stl(x, s.window = "periodic", robust=TRUE)$time.series[, 3]
  } else {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid, prob=c(0.25, 0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid - limits[1])/iqr, 0) + pmax((resid - limits[2])/iqr, 0))
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA, length(x)))
    x2[score > 0] <- x[score > 0]
    tsp(x2) <- tsp(x)
    points(x2, pch=19, col="red")
    return(invisible(score))
  } else {
    return(score)
  }  
}






# ------------ Extract max values to ASCII tables --------------

# This subroutine extracts the max VI value per point per year, and
# writes the results to an ASCII table for future analysis

getMaxVals <- function(df_vals) {
  
  message("Extracting max vals...\n", "\r", appendLF = FALSE)
  
  # Get the max value and corresponding date for each point in each year
  max.vals <- do.call(rbind, lapply(split(df_vals, list(df_vals$pointid, df_vals$year)), 
                                    function(x) x[which.max(x[ , 'mean']), ]))
  
  #> head(max.vals)
  #               date pointid year      mean
  #pt0.1984  1984-07-08  pt0 1984 0.5229930
  #pt1.1984  1984-07-08  pt1 1984 0.2851064
  #pt11.1984 1984-07-08 pt11 1984 0.5669947
  #pt13.1984 1984-06-29 pt13 1984 0.2712892
  #pt14.1984 1984-07-08 pt14 1984 0.3606695
  
  # Summarize max value stats for all points in each year: range of values, dates, and stddev
  stats.maxVals <- ddply(max.vals, .(year), summarize,
                         freq = length(mean),
                         rangeVal = max(mean) - min(mean), 
                         rangeDate = as.numeric(max(date) - min(date)),
                         stdVal = sd(mean))
  
  #> head(stats.maxVals)
  #  year  rangeVal rangeDate    stdVal
  #1 1984 0.4136916       128 0.1418780
  #2 1985 0.3754448       103 0.1213463
  #3 1986 0.3573114        87 0.1183681
  #4 1987 0.4356716        96 0.1376528
  #5 1988 0.3973650        87 0.1338641
  
  # Set the column name back to the original vi choice
  colnames(max.vals)[which(colnames(max.vals) == 'mean')] <- vi
  
  #> head(max.vals)
  #               ndvi ptid       date year
  #pt0.1984  0.4913237  pt0 1984-11-13 1984
  #pt1.1984  0.4415022  pt1 1984-11-04 1984
  #pt10.1984 0.3444302 pt10 1984-11-13 1984
  #pt11.1984 0.4108262 pt11 1984-11-20 1984
  #pt12.1984 0.4299363 pt12 1984-11-04 1984
  #pt13.1984 0.5169591 pt13 1984-11-04 1984
  
  # export files: 
  # max.values: maximum value for each sampled pixel in each year
  # stats.maxVals: aggregate stats for all pixels in each group
  
  outfileMaxVals <- paste0(vi, "Vals_", file.names$longName, "_maxVals", add_on, ".csv")
  outfileMaxStats <- paste0(vi, "Vals_", file.names$longName, "_maxStats", add_on, ".csv")
  return(max.vals)
}


# ------------ QC min values --------------

# This subroutine extracts the min VI value per point per year, and
# evaluates whether it's an outlier or not.

getMinVals <- function(df_vals) {
  
  message("Extracting min vals...\n", "\r", appendLF = FALSE)
  
  # Get the min value and corresponding date for each point in each year
  min.vals <- do.call(rbind, lapply(split(df_vals, list(df_vals$pointid, df_vals$year)), 
                                    function(x) x[which.min(x[ , 'mean']), ]))
  
#  > head(min.vals)
#                 date pointid year firename      mean
#  pt1.1986 1986-07-19     pt1 1986     dude 0.6385241
#  pt2.1986 1986-12-10     pt2 1986     dude 0.4274510
#  pt3.1986 1986-11-24     pt3 1986     dude 0.6414634
#  pt4.1986 1986-08-04     pt4 1986     dude 0.4416300
#  pt5.1986 1986-08-20     pt5 1986     dude 0.4417671
#  pt6.1986 1986-05-16     pt6 1986     dude 0.5335024

  
return(min.vals)
}
# Write dataframe to a file. Name = input filename + "_seasonal_output.csv"




# ------------ readDateLabels --------------

# Read in either 8- or 16-day file of date labels
# format date and add column representing period

readDateLabels <- function(df, timeInt) {
  
  df$newdate <- strptime(as.character(df$date), "%m/%d/%Y") #excel changed the format to m/d/y
  df$date <- as.Date(format(df$newdate, "%Y-%m-%d"))
  df$newdate <- NULL
  df$period <- seq(1, nrow(df)) # associate each 8- or 16-day period with a sequential number
  return(df)
}


# -------------- convertDates ---------------------------

#Function: Convert indexed column to DOY

convertDates <- function(df.in, col, t.int, dates.in) {
  temp <- df.in[col]  # focus on that one column
  temp$period <- floor(temp[[col]])  # get the floor of the period (i.e., floor(9.33) = 9)
  temp.merged <- plyr::join(temp, dates.in,    # get the equivalent date for each time period from the 'dates.in' file                        
                            by = "period",     # plyr magically preserves the row order
                            type = 'left')     # keep only the dates listed in temp
  temp.merged$doydate <- yday(temp.merged$date)  # get the DOY
  temp.merged$year <- year(temp.merged$date)
  
  # DOY = conversion of date to DOY PLUS extra days (fraction above the floor x time interval)
  # Rounding is necessary bc otherwise the value is truncated
  temp.merged$xtrdays <- (temp.merged[[col]] %% temp.merged$period) * t.int
  temp.merged$doy <- yday(round(temp.merged$date + temp.merged$xtrdays))
  
  # the above columns are just for reassuring myself that it's working as it should.  This is the money column.
  colnames(temp.merged)[length(colnames(temp.merged))] <- paste0(col, ".doy")  #awkward way to assign name but works
  return(temp.merged)
}



# ------------ exportList --------------
# 
# Import a dataframe, split by category type, export to individual csv files

exportList <- function(df, cat) {
  
  # Set naming convention prefix
  prefix <- "samplePts_"
  
  # Split file into constituent elements, by category
  file.list <- split(df, df[[cat]])
  
  # Write list elements to individual files
  lapply(1:length(file.list), function(i) write.csv(file.list[[i]],
                                                    file = file.path(path.out, 
                                                                     paste0(prefix, names(file.list[i]), ".csv")), 
                                                    row.names = FALSE)
  )
}


# ------------------- setFireDates -----------------------------


fire.dates <- list(Dome = as.Date("1996-04-25", '%Y-%m-%d'), 
                   Lummins = as.Date("1997-06-29", '%Y-%m-%d'),
                   Unit29 = as.Date("1998-06-16", '%Y-%m-%d'),
                   Unit38 = as.Date("1999-03-04", '%Y-%m-%d'),
                   CerroGrande = as.Date("2000-05-04", '%Y-%m-%d'),
                   LaMesa = as.Date("1977-06-16", '%Y-%m-%d'),
                   LasConchas = as.Date("2011-06-27", '%Y-%m-%d'))


# ------------------- lm_eqn -------------------------------------

# Compute and format regression equation for output on plot

lm_eqn <- function(df, y, x){
  f <- paste(y, "~", x) #, collapse=" + ")
  m <- do.call("lm", list(as.formula(f), data=df))
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}



# ----------------- getNames ------------------------------


# This function splits up a file name into components
# To make sense, the name should be in the format
# "ndvi_evtGrass_espALL_cerroG_30pts_1984.csv"
# which will return evt, esp, and fire area

getNames <- function(fileName.in) {
  strParts <- unlist(strsplit(fileName.in, "_"))
  plot.name <- paste(strParts[1:4], collapse = "_")
  outfile.name <- paste(strParts[2:4], collapse = "_")
  evt <- gsub("evt", "", strParts[2])
  esp <- gsub("esp", "", strParts[3])
  area <- gsub("area", "", strParts[4])
  long.name <- paste(strParts[2:length(strParts)], collapse = "_")
  file.names <- list("plot" = plot.name, "evt" = evt, "esp" = esp, "area" = area, 
                     "outfile" = outfile.name, "longName" = long.name)
  return(file.names)
}


# ---------------- readInFile --------------------

# Read in file, check for particular column


readInFile <- function(fileIn) {
  
  # read the file
  z <- read.csv(fileIn, header = T, check.names = FALSE)
  
  # check.names keeps R from changing the column name from system:index to system.index
  
  # check for presence of key value column
  if (!("ndvi" %in% colnames(z))) {
    
    warning('\n\n****************************\n no value column present!\n****************************')
  } 
  
  if (!("system:index" %in% colnames(z))) {
    warning('\n\n****************************\n no sys:index present!\n****************************')
  }
  
  return(z)
}




##  ------------------ processLandsatGEEFile ---------------------------

# Formats a GEE table of points sampled from a Landsat time series.
# Columns have the format 'system:index', 'pointid', 'data1', 'data2', 'data3', 'data4', '.geo'.
# system:index is (e.g.) LT50680141999284XXX03_0, where *_ is the Landsat ID and date,
# and _* is the internally assigned point ID.  The function drops the '.geo' column and extracts the 
# date and ID.
# 
# File starts in this format:
# 

#> head(file.in)
#               system:index pointid         evi       msavi           ndvi        satvi .geo
#1  1_1_1_LT40330351982352_4     pt3 0.212980315 0.157719314 NA 0.414122999 -0.057125863   NA
#2 1_1_1_LT40330351982352_12    pt11 0.169329077 0.113681175 NA 0.376331359 -0.076161020   NA
#3 1_1_1_LT40330351983003_12    pt11 0.159979150 0.106413729 NA 0.308542728 -0.099384382   NA
#4  1_1_2_LT50330351984126_8     pt7 0.353798509 0.286248595 NA 0.459088683  0.116970964   NA
#5 1_1_2_LT50330351984126_14    pt13 0.153756738 0.122013733 NA 0.187459737  0.131762624   NA
#6 1_1_2_LT50330351984126_13    pt12 0.189355552 0.148945928 NA 0.250434339  0.176715955   NA
 

# and ends in this format:

#> head(df)
#        ndvi      evi     msavi      satvi ptid             lsid       date
#1 0.3091971 0.1875000 0.1650349 0.11022376    0 LT50330351984142 1984-05-21
#2 0.2221314 0.1426921 0.1229799 0.12969960    1 LT50330351984142 1984-05-21
#3 0.2615648 0.1663700 0.1408165 0.15166470    2 LT50330351984142 1984-05-21
#4 0.3287943 0.2086033 0.1798990 0.10017336    3 LT50330351984142 1984-05-21
#5 0.2891471 0.1883774 0.1655805 0.12423935    4 LT50330351984142 1984-05-21
#6 0.2885832 0.1857953 0.1603972 0.08511823    5 LT50330351984142 1984-05-21



# processLandsatGEEFile <- function(fileIn, fileName) {
# 
# ## Took this out because essentially I was trying to get rid of ".geo", and
# ## since sometimes "system:index" was "system.index", it seemed easier to just
# ## remove the .geo column
#   
#   # look at the file name to determine what the column names are.
#   # Anything with "std" in it means the standard deviation was calculated.
# 
#   #  if (grepl('std', fileName)) {
# #     if ("firename" %in% colnames(fileIn)) {
# #       cols <- c('system:index', 'ndvi_sd', 'evi_sd', 'msavi_sd', 'satvi_sd', 'pointid', 'firename')
# #     } else {
# #       cols <- c('system:index', 'ndvi_sd', 'evi_sd', 'msavi_sd', 'satvi_sd')
# #     }
# #  } else {
# #     if ("firename" %in% colnames(fileIn)) {
# #       cols <- c('system:index', 'ndvi', 'evi', 'msavi', 'satvi', 'pointid', 'firename')
# #     } else {
# #       cols <- c('system:index', 'ndvi', 'evi', 'msavi', 'satvi')
# #     }
# #  }
#   
#   # drop any columns with only NAs. 
#   z <- fileIn[ , colSums(is.na(fileIn)) != nrow(fileIn)]
#   
#   # rename first column for convenience
#   colnames(z)[1] <- "ids"
#   
#   # convert the date/ID tag from factor
#   z['ids'] <- lapply(z['ids'], as.character) 
#   
#   # split the date/ID tag by "_" 
#   z$temp <- lapply(strsplit(z$ids, "_"), tail, n=2)
#   
#   # last element = internal ID
#   z$intid <- sapply(z$temp, "[[", 2)
# 
#   # function to pad single-digit point name  
#   rename_points = function(y) {
#     
#     # if 1st character of 1st row is a digit,
#     # assume they all are
#     if (grepl("^[[:digit:]]", y[1])) {
#       y <- ifelse (as.numeric(y) < 10,
#             paste0("pt0", as.numeric(y)),
#             paste0("pt", y))
#       }
#     return(y)
#   }
#   
#   # If the point# is a number less than 10, add a leading 0
#   z$intid <- rename_points(z$intid)
#   z$intid <- as.factor(unlist(z$intid))
#   
#   # If the point# is a number less than 10, add a leading 0
#   z$pointid <- rename_points(z$pointid)
#   z$pointid <- as.factor(unlist(z$pointid))
#   
#   # 2nd to last element = date in YYYYMMDD
#   z$sceneid <- sapply(z$temp, "[[", 1)
#   z$sceneid <- as.factor(unlist(z$sceneid))
#   
#   # get the satellite #
#   z$lsid <- as.factor(substr(z$sceneid, 1, 3))
#   
#   # extract the date in format yyyydoy
#   z$yrdoy <- substr(z$sceneid, 10, 16)
#   z$date <- as.Date(z$yrdoy, format = "%Y %j")
#   
#   # clean up
#   z$ids <- NULL
#   z$yrdoy <- NULL
#   z$temp <- NULL
#   
#   return(z)
# }



processLandsatGEEFile_coll1 <- function(fileIn, fileName) {
  
  # the file info construct is different than the pre-coll1 data
  
  # drop any columns with only NAs. 
  z <- fileIn[ , colSums(is.na(fileIn)) != nrow(fileIn)]
  
  # rename first column for convenience
  colnames(z)[1] <- "ids"
  
  # convert the date/ID tag from factor
  z['ids'] <- lapply(z['ids'], as.character) 
  
  # split the date/ID tag by "_". Get the last 4 elements
  z$temp <- lapply(strsplit(z$ids, "_"), tail, n=4)
  
  # last element = internal ID
  z$intid <- sapply(z$temp, "[[", 4)
  

  # If the point# is a number less than 10, add a leading 0
  z$intid <- rename_points(z$intid)
  z$intid <- as.factor(unlist(z$intid))
  
  # 2nd to last element = acquisition data
  #z$sceneid <- sapply(z$temp, "[[", 1)
  #z$sceneid <- as.factor(unlist(z$sceneid))
  
  # get the satellite #
  #z$lsid <- as.factor(substr(z$sceneid, 1, 3))
  
  # extract the acquisition date in format yyyymmdd
  z$date_temp <- sapply(z$temp, "[[", 3)
  z$date <- as.Date(z$date_temp, format = "%Y %m %d")
  
  # If the point# is a number less than 10, add a leading 0
  #z$pointid <- rename_points(z$pointid)
  #z$pointid <- as.factor(unlist(z$pointid))
  
  # clean up
  z$ids <- NULL
  z$date_temp <- NULL
  z$temp <- NULL
  
  return(z)
}


# -------------- rename_points ----------------------
# function to pad single-digit point name  
rename_points = function(pts) {
  
  # if 1st character of 1st row is a digit,
  # assume they all are
  if (grepl("^[[:digit:]]", pts[1])) {
    pts <- ifelse (as.numeric(pts) < 10,
                 paste0("pt0", as.numeric(pts)),
                 paste0("pt", pts))
  }
  return(pts)
}



# -------------- moveCols ----------------------
# Moves columns to either the beginning or the end of the file
# swiped as is from http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe

# use as moveCols(df, c("b", "c"), "where")  #where = c("first", "last", "before", "after)
# ba = before/after column
moveCols <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

#  ---------- convertToTS -----------------
# 
# Average all values in any given month
# Convert to a time series by filling in missing values
# with NAs
# 
convertToTS2 <- function(df.in, evt.type, esp.type, metric.type) {
  
  df.in$my <- floor_date(df.in$date, "month")
  df.in$wk <- floor_date(df.in$date, "week")
  
  start.year <- year(df.in$date[1])
  start.mo <- month(df.in$date[1])
  end.year <- year(df.in$date[nrow(df.in)])
  end.mo <- month(df.in$date[nrow(df.in)])

  vi = "ndvi"
  df.melt <- ddply(df.in, .(my), summarize,
                   n = length(id), 
                   mean = mean(vi, na.rm = T),
                   med = median(vi, na.rm = T),
                   max = max(vi),
                   min = min(vi),
                   sd = sd(vi),
                   se = sd/sqrt(n))
  
  df.zoo <- zoo(df.melt[, 2:ncol(df.melt)], order.by = df.melt$my)
  
  df.zoo.nas <- merge(df.zoo, zoo(, seq(start(df.zoo), end(df.zoo), by = "mo")), all = T)
  df.zoo.filled <- na.spline(df.zoo.nas)
  df.zoo.df <- data.frame(date = time(df.zoo.filled), 
                          df.zoo.filled, 
                          check.names = F, 
                          row.names = NULL)
  df.zoo.df$evt <- evt.type
  df.zoo.df$esp <- esp.type
# convert to xts
  df.xts <- xts(df.zoo.filled) # changed from df.zoo.df
  
  # choose 1 metric to convert to ts
  df.zoo.filled.metric <- df.zoo.filled[, metric.type]

  df.ts.metric <- ts(df.zoo.filled.metric, start = c(start.year, start.mo), end = c(end.year, end.mo), frequency = 12)  
  df.all <- list('ts' = df.ts.metric, 'df' = df.zoo.df, 'xts' = df.xts)
  return(df.all)
}


#  ---------- convertToTS -----------------

convertToTS <- function(df.in, evt.type, esp.type, metric.type) {
  
  df.in$my <- floor_date(df.in$date, "month")
  
  start.year <- year(df.in$date[1])
  start.mo <- month(df.in$date[1])
  end.year <- year(df.in$date[nrow(df.in)])
  end.mo <- month(df.in$date[nrow(df.in)])
  
  df.melt <- ddply(df.in, .(my), summarize,
                   n = length(id), 
                   mean = mean(ndvi, na.rm = T),
                   med = median(ndvi, na.rm = T),
                   max = max(ndvi),
                   min = min(ndvi),
                   sd = sd(ndvi),
                   se = sd(ndvi)/sqrt(length(ndvi)))
  
  # turn into a zoo time series class.  Don't need the date column
  zoo.data <- zoo(df.melt[, 2:ncol(df.melt)], order.by = df.melt$my)
  
  zoo.nas <- merge(zoo.data, zoo(, seq(start(zoo.data), end(zoo.data), by = "mo")), all = T)
  zoo.filled <- na.spline(zoo.nas)
  df.all <- data.frame(date = time(zoo.filled), 
                          zoo.filled, 
                          check.names = F, 
                          row.names = NULL)
  df.all$evt <- evt.type
  df.all$esp <- esp.type
  df.metric <- df.all[, metric.type]
  ts.metric <- ts(df.metric, start = c(start.year, start.mo), end = c(end.year, end.mo), frequency = 12)  
  df.both <- list('ts' = ts.metric, 'df' = df.all)
  return(df.both)
}


###

# -------------- Plot 

plotGG <- function(df, colToPlot) {
  if(colToPlot == "max") {
    yMax = 1.0
  } else if(colToPlot == "sd") {
    yMax = 0.25
  } else {
    yMax = 0.7
  }
  ggplot(df, aes_string(x = "date", y = colToPlot)) +
    geom_line(size = 0.8, 
              aes(group = evt, color = evt)) + # linetype = veg)) +
  #  scale_linetype_manual(values = c("dotdash", "solid")) +
    theme_bw() +
    xlab("Date") +
    ylab(paste0(colToPlot, " NDVI")) +
    ylim(0, yMax) +
    scale_color_discrete(name = "EVT Class\n") +
    ggtitle(paste0("Site Potential: ", df$esp, "\n Existing Vegetation: ",
                   levels(df$evt)[1], "/", levels(df$evt)[2]))
}



## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



# --------------




# ---------- fillMissingDates




#  Sometimes there are multiple entries for each date (why would that be?). 
#  Average values collected on each date, by id.

# df.sum <- ddply(df, .(date, id), summarize, value = mean(ndvi))
 
# for each id level, create a zoo object from x (after taking out the date)
# df.zoo <- by(df.sum, df.sum$id, function(x) zoo(x[,-1], order.by=x$date))
 
# Now there's a list of zoo objects.  Merge them:
 
# df.zoo.all <- do.call("merge", df.zoo)
 
 # split out for each factor, calculate zoo object, then combine
 
 #df.zoo <- zoo(df.sum[, -1], order.by = df.sum$date)

# Assuming there's a 16-day interval, create an empty zoo object with the
# sequence of every 16d from the start of the first df to the end

 #df.dates <- zoo(, seq(start(df.zoo), end(df.zoo), by = 16))
 
 #df.all <- merge(df.zoo, df.dates, all = TRUE)
 
 # convert the id back to a single value
# df.all$id <- df.all$id[1]
  


  
# summarize data by date
# z_median <- ddply(z_sub, .(date), summarize, median = median(ndvi))
#z_mean <- ddply(z_sub, .(date), summarize, mean = mean(ndvi))