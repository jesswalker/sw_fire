# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

#  process_landsat_GEE_sampled_data_to_output_tables_1fire.R

# This script takes all individual data series tables that result from
# sampling all surface reflectance Landsat images available
# in GEE (4,5,7,8) in a burn area at a number of points in a SINGLE fire.
# Each table was (originally) sampled at 30 points. 
#
# The script inputs raw, irregularly spaced data.  After an optional correction of OLI NDVI data 
# (and only NDVI data) to match Landsat 5/7 values, the routine extracts the maximum value of each 
# point per year, writing it out to an output file.  Max value stats are computed for all
# pixels (i.e., range of max values, range of max value dates, and stddev of values) and
# also written to file.
#
# The irregular data series is then extended to daily intervals, and non-data days are filled
# with NaNs and aggregated to 8- and 16-day intervals.  Missing values are linearly
# approximated to yield a regular data series consisting of a value for each time interval.  
# This interpolation results in a regularly spaced time series that can be ingested by any 
# subsequent process that requires equal interval time steps.
#
# Files are reformatted to comply with Timesat format requirements, and exported to txt files. 
#
# Since Timesat only produces seasonal data for n-1 years, the last year of data is replicated
# for all points to yield data for the full span of years (this action is recommended as SOP by
# Timesat folks.)
#
#
# Data QA/QC: 
#  - duplicate points (i.e., different points w identical values) are reduced to one
#    representative point;
#  - multiple values collected at a single point on the same day (due to image overlap, etc.)
#    are averaged.
#
#   Input files have multiple columns:
#   
#   1.  system:index:  Landsat and point ID in format "LT50330351995012XXX01_1014"
#   2.  evi, msavi, ndvi, satvi:  VI values for the particular image
#
#   Output files written to D:/projects/Mendenhall/tables/ :
#
#   1. Maximum VI values per sampled pixel, per year
#   2. Stats (range, range of dates, and stddev) of maximum values of all sampled pixels over time
#   3. File in timesat format of padded 8-day data
#   4. File in timesat format of padded 16-day data

#  There is an option to correct NDVI data using the values in Huntington et al., 2016, 
#  "Assessing the role of climate and resource management on groundwater dependent ecosystem changes..."
#   RSE.
#   
# Load libraries
#  library(reshape2)
#  library(ggplot2)
#  library(TTR)
#  library(plyr)
#  library(tidyr)
#  library(lubridate)
#  library(zoo)


#
#  ===========  START MAIN PROGRAM =================================
#

rm(list=ls())

## Load libraries
library(reshape2)
library(ggplot2)
library(TTR)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

# Set up directory paths
folder = "Coop" #Mendenhall"
source("D:/projects/Mendenhall/R/gee_functions.R")  # this usually returns a TRUE/FALSE error--ignore.
#path.in <- paste0("D:/projects/", folder, "/tables/*.*")  uncomment to run individually
path.in <- paste0("D:/projects/", folder, "/tables")
path.out <- paste0("D:/projects/", folder, "/tables")

# list all files in the directory that begin with "viVals".  The symbol ^ ensures that
# the start of the string is "viVals", and not, e.g., "ndviVals". 

filenames.in <- list.files(path.in, pattern = "^viVals*", full.names=F)

# correct for OLI?
correct_opt <- TRUE # FALSE
add_on <- ""  # tag for whether or not the file is OLI-corrected

# begin loop through all files

#for (filename.in in filenames.in) {
 # to choose individual file to ingest rather than looping: 
  filename.in <- choose.files(default = path.in, caption = "Select file", # uncomment to select single files
                         multi = FALSE, filters = c("^viVals*"))
  
  # Choose a VI to extract: ndvi, evi, satvi, or msavi
  vi <- "ndvi"

  # Parse names  
  baseFilename <- basename(filename.in)
  baseFilename <- substr(baseFilename, 1, nchar(baseFilename) - 4)
  file.names <- getNames(baseFilename)

# Read file -> df and check for valid data columns
  file.in <- readInFile(file.path(path.in, filename.in))
#  file.in <- readInFile(file.path(filename.in))  ## uncomment for single file
  
  print(paste0("Processing ", file.path(path.in, filename.in)))

  #> head(file.in)
  #             system:index       evi      msavi      ndvi       satvi .geo
  #1  1_1_LT50330351984126_1 0.1364291 0.11029096 0.1608447 0.105349543   NA
  #2  1_1_LT50330351984126_2 0.1944078 0.15399020 0.2491195 0.147205466   NA
  #3 1_1_LT50330351984126_13 0.1597042 0.09859476 0.1279297 0.008608672   NA
  #4 1_1_LT50330351984126_14 0.1551420 0.12163408 0.1903710 0.160990934   NA
  #5 1_1_LT50330351984126_15 0.1464890 0.11600250 0.1881833 0.147418476   NA

  # Drop the '.geo' column, extract the date and ID from Landsat tag
  df <- processLandsatGEEFile(file.in, baseFilename)

  #> head(df)
  #       ndvi       evi      msavi       satvi ptid          sceneid lsid       date
  #1 0.1608447 0.1364291 0.11029096 0.105349543  pt1 LT50330351984126  LT5 1984-05-05
  #2 0.2491195 0.1944078 0.15399020 0.147205466  pt2 LT50330351984126  LT5 1984-05-05
  #3 0.1279297 0.1597042 0.09859476 0.008608672 pt13 LT50330351984126  LT5 1984-05-05
  #4 0.1903710 0.1551420 0.12163408 0.160990934 pt14 LT50330351984126  LT5 1984-05-05
  #5 0.1881833 0.1464890 0.11600250 0.147418476 pt15 LT50330351984126  LT5 1984-05-05

  # Make sure the data are nicely ordered
  df <- df[order(df$date), ]

  # Get rid of some dates. 1985 is the first full year
  df$year <- year(df$date)  # lubridate
  startyear <- 1984  #year(df$date[1])
  endyear <- year(df$date[nrow(df)])

  df <- df[which(df$year >= startyear), ]

  # Make sure no one pixel is sampled twice; take out any duplicated rows (i.e., different 
  # rows that contain the same vi values and dates. That would be highly unlikely for different pixels). 
  # One representative point remains for each set.
  # Using only one of the vi's--ndvi, evi, etc--to check for duplicates should be enough.
  # Still doesn't account for spatial autocorrelation! Yikes.
  
  df <- ddply(df, .(date), function(x) x[!duplicated(x[[vi]]), ])
  
#  df2 <- df[!(duplicated(df[, c("value", "date")]) | duplicated(df[, c("value", "date")], fromLast = T)),]

  # Remove all other VIs until someone learns how to program better.
  df.sub <- df[ , c(vi, 'ptid', 'date', 'year')]

  #> head(df.sub)
  #           ndvi ptid       date year
  #  129 0.1991603    4 1985-03-21 1985
  #  131 0.1990817   11 1985-03-21 1985
  #  133 0.1969757   28 1985-03-21 1985
  #  134 0.1627010    0 1985-04-06 1985

  # per Huntington et al. 2016, adjust the OLI NDVI to match the ETM+ (and TM) NDVI

 
# to correct OLI values (Huntington et al 2016) uncomment 
 if (correct_opt & vi == 'ndvi') {
       df[which(df$lsid == "LC8"), 'ndvi'] <- (df[which(df$lsid == "LC8"), 'ndvi'] *0.8744 + 0.0074)
       add_on <- "_OLI_correction"
 }
  
  # Also need to handle POINTS that are sampled more than once on any given date.  This can 
  # happen during times when Landsats 5,7,8 are operating together OR if there's overlap from 
  # different path/row.  Take the mean of any values that are associated with the
  # same date and point ID.  "V1" is the value column name.  "Year" is retained because 
  # it is needed in subsequent analysis steps, and including it does not affect the command.
  
  #df.melt <- ddply(df.sub, .(date, ptid, year), summarize, 
  #               mean = eval(substitute(mean(col), list(col = as.name(vi)))))
  
  df.melt <- ddply(df.sub, .(date, ptid, year), 
                   function(d) mean = mean(d[[vi]]))
  
  # switch Colname back to "mean"
  colnames(df.melt)[which(colnames(df.melt) == "V1")] <- "mean"

  # ---------------
  # This subroutine extracts the max VI value per point per year, and
  # writes the results to an ASCII table for future analysis

  # Get the max value and corresponding date for each point in each year
  max.vals <- do.call(rbind, lapply(split(df.melt, list(df.melt$ptid, df.melt$year)), 
                                  function(x) x[which.max(x[ , 'mean']), ]))

  #> head(max.vals)
  #                date ptid year      mean
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
#  write.csv(max.vals, file = file.path(path.out, outfileMaxVals), row.names = F)
#  write.csv(stats.maxVals, file = file.path(path.out, outfileMaxStats), row.names = F)

#  print(paste0("Table of max values written to ", file.path(path.out, outfileMaxVals)))
#  print(paste0("Table of max value stats written to ", file.path(path.out, outfileMaxStats)))

  ggplot(max.vals, aes_string('date', vi, color = 'ptid')) + geom_point() + geom_line() + ylim(0, 1)
  ggplot(stats.maxVals, aes(year, rangeVal)) + geom_point() + geom_line()

  # ----------  Subroutine over -----------------
  
  # Make sure the time series starts on Jan 1 and ends on Dec 31
  # Adding a Jan 1 and a Dec 31 date ensures that the daily sequence will
  # encompass the entire span of years.  Not sure this is the best way to go.
  
  # If the first day isn't Jan 1, add an entry consisting of all NAs
  if (df.melt$date[1] > paste0(startyear, '-01-01')) { 
    firstrow <- c(rep(NA, ncol(df)))
    df.melt <- rbind(firstrow, df.melt)
    df.melt[1, "date"]  <- as.Date(paste0(startyear, '-01-01'))
    df.melt[1, "year"] <- startyear
    df.melt[1, "ptid"] <- df.melt[2, "ptid"]  # duplicate point id sacrificial point
  }

  # If the last day isn't Dec 31, add an entry
  if (df.melt$date[nrow(df.melt)] < paste0(endyear, '-12-31')) {
    lastrow <- c(rep(NA, ncol(df.melt)))
    df.melt <- rbind(df.melt, lastrow)
    df.melt[nrow(df.melt), "date"] <- as.Date(paste0(endyear, '-12-31'))
    df.melt[nrow(df.melt), "year"] <- endyear
    df.melt[nrow(df.melt), "ptid"] <- df.melt[nrow(df.melt)-1, "ptid"]
  }
  
  
  # Extract the max VI value per point per year, and
  # write the results to ASCII tables
  maxs <- getMaxVals(df.melt)
  
  # Extract the min VI value per point per year
  mins <- getMinVals(df.melt)
  #  mins.list <- split(mins, mins$pointid)
  #  out.list <- by(mins$mean, mins$pointid, function(x) tsOutliers(x, T))
  
  ##########################################
  ####################################################
  # stop here to run replaceIt on outliers
  ####################################################
  # don't know how to do this programmatically
  
  # calc outliers for each group
  # id where outliers > 1.5 and data < .1
  # interpolate id'd value from surrounding mins
  # replace value with interpolated one in original data file (df.melt)
  
  
  # tsOutliers(subset(mins, pointid == "pt2")$mean, T)
  # replaceIt('pt0', 23)
  
  
  

  # Spread out the data to associate each point with each date
  # Note the use of "spread_" instead of "spread", which allows the evaluation of variable "vi"
  #df.wide <- tidyr::spread_(df.melt, "ptid", vi)  # no need any longer
  df.wide <- tidyr::spread(df.melt, "ptid", "mean") 

  #> head(df.wide)
  #         date year       pt0       pt1      pt11      pt13      pt14      pt15      pt16      pt17     
  # 1 1984-01-01 1984        NA        NA        NA        NA        NA        NA        NA        NA       
  # 2 1984-05-05 1984        NA 0.1608447        NA 0.1279297 0.1903710 0.1881833 0.4454846        NA   
  # 3 1984-05-21 1984 0.4974560 0.2064470 0.5175009 0.2168142 0.2492776 0.2459297 0.5701208 0.2215569 
  # 4 1984-06-06 1984        NA        NA        NA        NA        NA        NA        NA        NA        
  # 5 1984-06-29 1984 0.4810364 0.2661510        NA 0.2712892        NA 0.3282843 0.6022018 0.2668738 
  # 6 1984-07-08 1984 0.5229930 0.2851064 0.5669947 0.2545983 0.3606695 0.2996047 0.6322106        NA 

  # Get column names (point names) for later use
  col_names <- colnames(df.wide[, 3:ncol(df.wide)])

  # Convert the df to a zoo object
  # Starting w col #3 skips the date and year columns. Date is kept, year falls away.
  zoo.wide <- zoo(df.wide[, 3:ncol(df.wide)], order.by = df.wide$date)

  # Create a sequence of daily values
  seq_daily <- zoo(, seq(start(zoo.wide), end(zoo.wide), by = 1))

  # Pad the original data with the daily dates
  zoo.nas <- merge(zoo.wide, seq_daily, all = T)

  # Convert the zoo object BACK to a df for ease of na.approx computation
  df.nas <- data.frame(date = time(zoo.nas), 
                   data = zoo.nas, 
                   check.names = F, 
                   row.names = NULL)

  # Bring in the original column names
  colnames(df.nas) <- c("date", col_names)  


  # Drop levels since it's possible fewer than 30 points were sampled
  df$ptid <- droplevels(df$ptid)

  # Reference relative column names
  firstPt <- col_names[1]
  lastPt <- col_names[length(col_names)]

  # Reshape into long format
  df.nas.long <- df.nas %>% gather_('ptid', 'value', col_names)  # tidyr 

  #> head(df.nas.long)
  #         date ptid value
  # 1 1984-01-01  pt0    NA
  # 2 1984-01-02  pt0    NA
  # 3 1984-01-03  pt0    NA
  # 4 1984-01-04  pt0    NA
  # 5 1984-01-05  pt0    NA

  # Cut the data into multi-day chunks
  # 8-day
  list.8day <- dlply(df.nas.long, .(format(date, "%Y"), ptid), # chunk for each year, ptid
             function(x) {split(x$value, ceiling(seq_along(x$value)/8))})

  # 16-day
  list.16day <- dlply(df.nas.long, .(format(date, "%Y"), ptid), # chunk for each year, ptid
              function(x) {split(x$value, ceiling(seq_along(x$value)/16))})

  # Get the associated names (dates) for each chunk, by year and point ID
  list.8day.names <- dlply(df.nas.long, .(format(date, "%Y"), ptid), function(x) x$date[seq_along(x$date) %% 8 == 1])
  list.16day.names <- dlply(df.nas.long, .(format(date, "%Y"), ptid), function(x) x$date[seq_along(x$date) %% 16 == 1])

  # > head(list.16day.names)
  # $`1984.pt0`
  # [1] "1984-01-01" "1984-01-17" "1984-02-02" "1984-02-18" "1984-03-05" "1984-03-21" "1984-04-06" "1984-04-22" "1984-05-08" "1984-05-24" "1984-06-09"
  # [12] "1984-06-25" "1984-07-11" "1984-07-27" "1984-08-12" "1984-08-28" "1984-09-13" "1984-09-29" "1984-10-15" "1984-10-31" "1984-11-16" "1984-12-02"
  # [23] "1984-12-18"

  # $`1984.pt1`
  # [1] "1984-01-01" "1984-01-17" "1984-02-02" "1984-02-18" "1984-03-05" "1984-03-21" "1984-04-06" "1984-04-22" "1984-05-08" "1984-05-24" "1984-06-09"
  # [12] "1984-06-25" "1984-07-11" "1984-07-27" "1984-08-12" "1984-08-28" "1984-09-13" "1984-09-29" "1984-10-15" "1984-10-31" "1984-11-16" "1984-12-02"
  # [23] "1984-12-18"

  # Turn the names into dataframes, because lists frustrate me
  df.8day.names <- reshape2::melt(list.8day.names)
  df.16day.names <- reshape2::melt(list.16day.names)

  # head(df.16day.names)
  #        value       L1
  # 1 1984-01-01 1984.pt0
  # 2 1984-01-17 1984.pt0
  # 3 1984-02-02 1984.pt0
  # 4 1984-02-18 1984.pt0
  # 5 1984-03-05 1984.pt0

  # remove the leading date and rename columns
  df.8day.names$ptid <- lapply(strsplit(df.8day.names$L1, "\\."), tail, n=1) #this extracts the last element of the values in col L1
  df.8day.names$ptid <- as.factor(unlist(df.8day.names$ptid))
  df.8day.names$L1 <- NULL
  colnames(df.8day.names)[1] <- "date"

  df.16day.names$ptid <- lapply(strsplit(df.16day.names$L1, "\\."), tail, n=1)
  df.16day.names$ptid <- as.factor(unlist(df.16day.names$ptid))
  df.16day.names$L1 <- NULL
  colnames(df.16day.names)[1] <- "date"
  
  # save them for future use!
#  write.csv(subset(df.8day.names, ptid == 'pt0'), file = "D:/projects/Mendenhall/tables/names_8day_1984_2015.csv", 
#                   row.names = FALSE)
#  write.csv(subset(df.16day.names, ptid == 'pt0'), file = "D:/projects/Mendenhall/tables/names_16day_1984_2015.csv", 
#                   row.names = FALSE)

  # Take the mean of each 8- or 16-day chunk
  num.8day.mean <- rapply(list.8day, mean, na.rm = T)
  num.16day.mean <- rapply(list.16day, mean, na.rm = T)

  # Attach the correct names, just for the heck of it
  names(num.8day.mean) <- do.call(c, list.8day.names)
  names(num.16day.mean) <- do.call(c, list.16day.names)

  #> head(num.16day.mean, 24)
  # 1984-01-01 1984-01-17 1984-02-02 1984-02-18 1984-03-05 1984-03-21 1984-04-06 1984-04-22 1984-05-08 1984-05-24 1984-06-09 1984-06-25 1984-07-11 
  #      NaN        NaN        NaN        NaN        NaN        NaN        NaN        NaN    0.4974560        NaN        NaN  0.5020147        NaN 
  #
  # 1984-07-27 1984-08-12 1984-08-28 1984-09-13 1984-09-29 1984-10-15 1984-10-31 1984-11-16 1984-12-02 1984-12-18 1984-01-01 
  #      NaN        NaN        NaN        NaN        NaN        NaN  0.5100365        NaN        NaN        NaN        NaN 

  # Back to a df so that it can be combined with the column names
  df.8day.mean <- melt(num.8day.mean)
  df.16day.mean <- melt(num.16day.mean)

  # Attach column names
  df.8day.mean <- cbind(df.8day.names, df.8day.mean)
  df.16day.mean <- cbind(df.16day.names, df.16day.mean)

  #> head(df.16day.mean)
  #         date ptid value
  # 1 1984-01-01  pt0   NaN
  # 2 1984-01-17  pt0   NaN
  # 3 1984-02-02  pt0   NaN 
  # 4 1984-02-18  pt0   NaN
  # 5 1984-03-05  pt0   NaN
  # 6 1984-03-21  pt0   NaN
  # 7 1984-04-06  pt0   NaN
  # 8 1984-04-22  pt0   NaN
  # 9 1984-05-08  pt0   0.497456

  # Expand to wide format b/c I couldn't figure out how to apply a function to each grouping 
  # in the df in this format without returning a godawful mess.  Kludgy and circuitous carries the day.
  df.8day.wide <- tidyr::spread(df.8day.mean, ptid, value)
  df.16day.wide <- tidyr::spread(df.16day.mean, ptid, value)

  # na.approx each column! rule = 2 extends the first/last valid value to leading/trailing NaNs
  mat.8day.approx <- sapply(df.8day.wide[, 2:ncol(df.8day.wide)], function(x) na.approx(x, rule = 2))
  mat.16day.approx <- sapply(df.16day.wide[, 2:ncol(df.16day.wide)], function(x) na.approx(x, rule = 2))

  # Back to df it goes, hi ho
  df.8day.approx <- as.data.frame(mat.8day.approx)
  df.16day.approx <- as.data.frame(mat.16day.approx)

  # Add the date back in
  df.8day.approx <- cbind('date' = df.8day.wide[, 1], df.8day.approx)
  df.16day.approx <- cbind('date' = df.16day.wide[, 1], df.16day.approx)

  #> head(df.16day.approx)
  #
  #         date      pt0       pt1      pt11      pt13     pt14      pt15      pt16      pt17      pt18      pt19       pt2 
  # 1 1984-01-01 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195
  # 2 1984-01-17 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195 
  # 3 1984-02-02 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195 
  # 4 1984-02-18 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195 
  # 5 1984-03-05 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195 
  # 6 1984-03-21 0.497456 0.1608447 0.5175009 0.1279297 0.190371 0.1881833 0.4454846 0.2215569 0.2578683 0.2075472 0.2491195 


  # Reshape so that each point has its own row of data (Timesat format)
  # First associate each point with a date and value.  Not sure this is a necessary
  # step but I"m keeping it for now.
  df.8day.approx.long <- df.8day.approx %>% tidyr::gather_('ptid', 'value', col_names)  # tidyr
  df.16day.approx.long <- df.16day.approx %>% tidyr::gather_('ptid', 'value', col_names)

  # Format those crazy long numbers to 4 digits
  df.8day.approx.long$value <- as.numeric(format(round(df.8day.approx.long$value, digits = 4)))
  df.16day.approx.long$value <- as.numeric(format(round(df.16day.approx.long$value, digits = 4)))

  # > head(df.16day.approx.long)
  #         date ptid    value
  # 1 1984-01-01  pt0 0.497456
  # 2 1984-01-17  pt0 0.497456
  # 3 1984-02-02  pt0 0.497456
  # 4 1984-02-18  pt0 0.497456
  # 5 1984-03-05  pt0 0.497456
  # 6 1984-03-21  pt0 0.497456

  # Reshape such that each row contains the data for an individual data point
  df.8day.approx.wide <- reshape(df.8day.approx.long, timevar = "date", idvar = c('ptid'), direction = "wide")
  df.16day.approx.wide <- reshape(df.16day.approx.long, timevar = "date", idvar = c('ptid'), direction = "wide")

  # > head(df.16day.approx.wide, 2)
  #       ptid value.1984-01-01 value.1984-01-17 value.1984-02-02 value.1984-02-18 value.1984-03-05 value.1984-03-21 value.1984-04-06 value.1984-04-22 value.1984-05-08
  # 1     pt0        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560
  # 737   pt1        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.2064470


  # This is ready for timesat! Almost. Need header elements:
  
  # number of years in the time series
  nyrs <- (endyear - startyear) + 2 #actually is + 1 years of data, but pad the timesat files with duplicated last year
  
  # number of data values per year
  nptsperyear_8day <- round(365/8) 
  nptsperyear_16day <- round(365/16) 

  # of time series in the file
  nts <- length(col_names)

  # 8 day
  # Write out the results to a table - 8day. Strip off 1st column, which has ptid info.
  # Write the header - 8-day data
  outfile8day <- paste0(vi, "Vals_", file.names$longName, "_8day_timesat", add_on, ".txt")
  write.table(t(c(nyrs, nptsperyear_8day, nts)), 
            file = file.path(path.out, outfile8day), 
            row.names = FALSE, col.names = FALSE, 
            sep = " ")
  
  # Duplicate last year's worth of data to pad for timesat
  start.col <- ncol(df.8day.approx.wide) - nptsperyear_8day + 1
  end.col <- ncol(df.8day.approx.wide)
  out.table <- cbind(df.8day.approx.wide, df.8day.approx.wide[, start.col:end.col])
  
  # Append data to 8-day table
  write.table(out.table[, 2:ncol(out.table)], 
            file = file.path(path.out, outfile8day), 
            row.names = FALSE, col.names = FALSE,
            sep = " ", append = TRUE)
  
  print(paste0("Table written to ", file.path(path.out, outfile8day)))


  # 16 day
  # Write out the results to a table - 16day.  Strip off 1st column, which has ptid info
  outfile16day <- paste0(vi, "Vals_", file.names$longName, "_16day_timesat", add_on, ".txt")
  write.table(t(c(nyrs, nptsperyear_16day, nts)), 
            file = file.path(path.out, outfile16day), 
            row.names = FALSE, col.names = FALSE, 
            sep = " ")
  
  # Duplicate last year's worth of data for Timesat padding
  start.col <- ncol(df.16day.approx.wide) - nptsperyear_16day + 1
  end.col <- ncol(df.16day.approx.wide)
  out.table <- cbind(df.16day.approx.wide, df.16day.approx.wide[, start.col:end.col])
  
  # Write output table
  write.table(out.table[, 2:ncol(out.table)], 
            file = file.path(path.out, outfile16day), 
            row.names = FALSE, col.names = FALSE,
            sep = " ", append = TRUE)

  print(paste0("Table written to ", file.path(path.out, outfile16day)))

}

###############################################################################
# Fini

