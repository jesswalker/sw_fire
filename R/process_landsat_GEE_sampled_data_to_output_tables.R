# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #

#  process_landsat_GEE_sampled_data_to_output_tables.R

# This script inputs a data series that results from sampling all surface reflectance Landsat images available
# in GEE (4,5,7,8) in a burn area at a number of points. 

#  'ptid' = id derived from GEE-supplied tag on each point
#  'pointid' = user-defined point id (and it must be labeled as such in the file).
#              This one is more useful, since it can be used to relate to a
#              file that contains metadata for each point.
#
# The script takes in raw, temporally irregular data.  After an optional correction of 
# OLI NDVI data (and only NDVI data) to match Landsat 5/7 values, the routine extracts the maximum 
# value of each point per year and writes it to an output file.  Max value stats are computed for all
# pixels (i.e., range of max values, range of max value dates, and stddev of values) and
# also written to file.
#
# The irregular data series is padded to a daily interval time series; non-data days are filled
# with NaNs.  The series is then aggregated to 8- and 16-day intervals by averaging all values
# within each period.  Missing values are linearly approximated to yield a regular data series 
# consisting of a value for each time interval (i.e., no NAs).  
#
# Files are reformatted to comply with Timesat format requirements, and exported to txt files. 
#
# Since Timesat only produces seasonal data for n-1 years, the last year of data is replicated
# for all points to yield data for the full span of years (this action is recommended by
# Timesat folks.)  The first year is generally too wacky to be useful.
#
#
# Data QA/QC: 
#  - duplicate points (i.e., different points w identical values, the result of sampling within
#    the same pixel) are reduced to one representative point;
#  - multiple values collected at a single point on the same day (due to image overlap, etc.)
#    are averaged;
#  - leading years with fewer then 5 sample dates are excluded.  The first "valid" year has
#    more than 5 points. 
#
# -------------------- #
#  INPUT
# -------------------- #
#   Input files have multiple columns:
#   
#   1.  system:index:  Landsat and point ID in format "LT50330351995012XXX01_1014"
#   2.  evi, msavi, ndvi, satvi:  VI values for the particular image
#   3.  firename (optional)
#   4.  pointid (user-defined; distinct from the subsequent point ID that GEE assigns)
#
#   - Note that the directory structure is hardcoded
#
# -------------------- #
#  oUTPUT
# -------------------- #
#   Output files written to D:/projects/<folder>/tables/max_vals:
#
#   1. Maximum VI values per sampled pixel, per year
#   2. Stats (range, range of dates, and stddev) of maximum values of all sampled pixels over time
#
#   Output files written to D:/projects/<folder>/timesat/input:
#
#   3. File in timesat format of padded 8-day data
#   4. File in timesat format of padded 16-day data
#
#   Output files written to D:/projects/<folder>/tables/r_format:
#   5. File in r format of padded 8-day data
#   6. File in r format of padded 16-day data
#
#   Output files written to D:/projects/<folder>/tables/
#   7. Files of dates at 8/16-day intervals: "dates_8day.csv"
#   8. File of point names: "point_names.csv"
#
#   There is an option to correct NDVI data using the values in Huntington et al., 2016, 
#  "Assessing the role of climate and resource management on groundwater dependent ecosystem changes..."
#   RSE.
#   
#
#  ===========  START MAIN PROGRAM =================================
#

rm(list=ls())

## Load libraries
library(reshape2)
library(TTR)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

# load functions
source("D:/projects/sw_fire/R/sw_fire_functions.R")  

# Set up directory paths
folder = "sw_fire" 
path.in <- file.path("D:/projects", folder, "data/tables/gee_values")
path.out.max <- file.path("D:/projects", folder, "output/tables/max_values")
path.out.rformat <- file.path("D:/projects", folder, "data/tables/r_format")
path.out.time <- file.path("D:/projects/", folder, "data/timesat/")
path.out.names <- file.path("D:/projects", folder, "output/tables")

# list all files in the directory that begin with "tag".  The symbol ^ ensures that
# the start of the string is searched for only; e.g., "^viVals" will not match "ndviVals". 

#tag <- "^sample*"
#tag <- "*.csv"

#filenames.in <- list.files(path.in, pattern = tag, full.names = TRUE)

# correct for OLI?
#correct_opt <- TRUE # FALSE
add_on <- ""  # tag for whether or not the file is OLI-corrected. Assigned later.

# --- Loop through files ----

#for (filename.in in filenames.in) {  ###   Uncomment to process multiple files
 
  all.files <- file.path(path.in, "*.*") 
  filename.in <- choose.files(default = all.files, caption = "Select file", 
                         multi = FALSE, filters = c("^viVals*"))  # this doesn't seem to work

  # Choose a VI to extract: ndvi, evi, satvi, or msavi
  vi <- "ndvi"

  # Parse names  
  baseFilename <- basename(filename.in)
  baseFilename <- substr(baseFilename, 1, nchar(baseFilename) - 4)
  file.names <- getNames(baseFilename)

# Info  
message("****************************************")
message(paste0("Processing ", file.path(filename.in), "\n"), "\r", appendLF = FALSE)
  
# Read file -> df and check for valid data columns
  file.in <- readInFile(filename.in)  

# pointid is the user-defined ID tag for each point
  
  # > head(file.in)
  # system:index       evi firename     msavi      ndvi pointid         satvi .geo
  # 1 1_1_1_1_LT04_036036_19821121_21 0.2020380    potaz 0.1777873 0.4396589       2  0.0846879557   NA
  # 2 1_1_1_1_LT04_036036_19821121_20 0.1814874    potaz 0.1489038 0.4933655       1  0.0607654005   NA
  # 3 1_1_1_1_LT04_036036_19821121_27 0.2519585     slim 0.2141836 0.5063550       1  0.0561817698   NA
  # 4 1_1_1_1_LT04_036036_19821121_13 0.2217794     dude 0.1877664 0.5543653       3  0.0394049548   NA
  # 5 1_1_1_1_LT04_036036_19821121_16 0.2321570     dude 0.2054031 0.5124726       6  0.0453148410   NA
  # 6 1_1_1_1_LT04_036036_19821207_12 0.2414916     dude 0.1912556 0.5628006       2 -0.0008075946   NA

  # Drop the '.geo' column, extract the date and ID from Landsat tag
  # file has to have "pointid" as a column name
  df <- processLandsatGEEFile_coll1(file.in, baseFilename)

  # > head(df)
  #         evi firename     msavi      ndvi pointid         satvi intid       date
  # 1 0.2020380    potaz 0.1777873 0.4396589       2  0.0846879557  pt21 1982-11-21
  # 2 0.1814874    potaz 0.1489038 0.4933655       1  0.0607654005  pt20 1982-11-21
  # 3 0.2519585     slim 0.2141836 0.5063550       1  0.0561817698  pt27 1982-11-21
  # 4 0.2217794     dude 0.1877664 0.5543653       3  0.0394049548  pt13 1982-11-21
  # 5 0.2321570     dude 0.2054031 0.5124726       6  0.0453148410  pt16 1982-11-21
  # 6 0.2414916     dude 0.1912556 0.5628006       2 -0.0008075946  pt12 1982-12-07

  # Make sure the data are nicely ordered
  df <- df[order(df$date), ]
  df$year <- year(df$date)  # lubridate
  
  # Remove negative numbers and replace with interpolated data later
  df[[vi]][which(df[[vi]] < 0)] <- NA

  
  # Loop through the individual fire names to process
  
  for (firename in levels(df$firename)) {
    
    df.fire <- df[df$firename == firename, ]
  
  # Make sure no one pixel is sampled twice; take out any duplicated rows (i.e., different 
  # rows that contain the same vi values and dates. That would be highly unlikely for different pixels). 
  # One representative point remains for each set.
  # Using only one of the vi's--ndvi, evi, etc--to check for duplicates should be enough.

    df.fire <- ddply(df.fire, .(date), function(x) x[!duplicated(x[[vi]]), ])
  
  # per Huntington et al. 2016, adjust the OLI NDVI to match the ETM+ (and TM) NDVI
  # to correct OLI values (Huntington et al 2016) uncomment 
  #   if (correct_opt & vi == 'ndvi') {
  #      df.fire[which(df.fire$lsid == "LC8"), 'ndvi'] <- (df.fire[which(df.fire$lsid == "LC8"), 'ndvi'] *0.8744 + 0.0074)
  #      add_on <- "_OLI_correction"
  #    }
  
  #  df2 <- df[!(duplicated(df[, c("value", "date")]) | duplicated(df[, c("value", "date")], fromLast = T)),]

  # Remove all other VIs. If the file is for a single fire,
  # make sure the column is renamed to 'pointid' from 'ptid' (holdover from pointid/ptid split, where
  # pointid is the GEE label and ptid is the user-defined point id)
  
  #  if (!("pointid" %in% names(df))) {  
  #    names(df)[names(df) == 'ptid'] <- 'pointid'
  #   } else {
    # Put a "pt" in front of the point values so they can be colnames
    # this depends on the values in "pointid" being numbers.  Format upfront instead.
  #    df.sub$pointid <- paste0('pt', df.sub$pointid)
  #   }
    
# Make sure there is a "pointid" column--this can be absent for single-point files
    if (!'pointid' %in% names(df.fire)) {
      df.fire$pointid <- "pt01"
    }
    
    df.fire$pointid <- rename_points(df.fire$pointid)
  
    df.sub <- df.fire[ , c(vi, 'date', 'year', 'pointid')]
    
 #   colnames(df.sub)[4] <- 'pointid'

 # > head(df.sub)
#         ndvi       date year firename  pointid
#  1 0.4108957 1984-03-22 1984     bell     pt08
#  2 0.4212454 1984-03-22 1984     bell     pt07
#  3 0.4120651 1984-03-22 1984     bell     pt06
#  4 0.4202506 1984-03-22 1984     bell     pt05
#  5 0.4659829 1984-05-12 1984     bell     pt08
#  6 0.5271834 1984-05-12 1984     bell     pt02


# ---- QA/QC ----
  
  message("****************************************")
  message("Performing QA/QC")
  
  # Handle POINTS that are sampled more than once on any given date.  This can 
  # happen during times when Landsats 5,7,8 are operating together OR if there's overlap from 
  # different path/row.  Take the mean of any values that are associated with the
  # same date and point ID.  "V1" is the value column name.  "Year" is retained because 
  # it is needed in subsequent analysis steps, and including it does not affect the command.
  
  # df.melt <- ddply(df.sub, .(date, ptid, year), summarize, 
  #               mean = eval(substitute(mean(col), list(col = as.name(vi)))))
  
  cols <- c('date', 'pointid', 'year')
  df.melt <- ddply(df.sub, (cols), function(d) mean = mean(d[[vi]]))

  # switch column name back to "mean"
  colnames(df.melt)[which(colnames(df.melt) == "V1")] <- "mean"

  # Check to make sure there are sufficient points in the first years, which
  # are generally the least well represented.  Use the first year to have at least 
  # 5 points as the starting year.
  
  # Get the number of sample dates per year. 
  # %>% is similar to the use of pipes in bash. The code pipes df.melt into group_by, 
  # and the result of that operation is piped into summarise. 
  # Divide by the number of levels since otherwise the number of different sampled points will bias the total.
  # Use explicit dplyr::summarize for number of rows
    nPtsPerYear <- df.melt %>% group_by(year) %>% dplyr::summarize(nrows = n()/(nlevels(as.factor(df.melt$pointid))))

  # Get the first year in which there are at least 5 points
  # Last year is simply the last year of data.
  # Note that if the last year's worth of data is incomplete (ends, say, in May)
  # it's better to just hardcode in the appropriate year.
  startyear <- nPtsPerYear$year[min(which(nPtsPerYear$nrows > 4))]
  endyear <- 2017 #df.melt$year[nrow(df.melt)] # last year with a data value
  
  # Remove years before 'startyear'
  df.melt <- df.melt[(df.melt$year >= startyear), ]
  
  # Remove years after 'endyear'
  df.melt <- df.melt[(df.melt$year <= endyear), ]
  
  # Make sure entire time series starts on Jan 1 and ends on Dec 31.
  # If the first day isn't Jan 1, add an entry consisting of all NAs
   if (df.melt$date[1] > paste0(startyear, '-01-01')) { 
       firstrow <- c(rep(NA, ncol(df.melt)))
       df.melt <- rbind(firstrow, df.melt)
       df.melt$date[1]  <- as.Date(paste0(startyear, '-01-01'))
       df.melt$year[1] <- startyear
       df.melt$pointid[1] <- df.melt$pointid[2]  # duplicate point id sacrificial point
       df.melt$firename[1] <- df.melt$firename[2]
  }

  # If the last day isn't Dec 31, add an entry consisting of all NAs
  if (df.melt$date[nrow(df.melt)] < paste0(endyear, '-12-31')) {
      lastrow <- c(rep(NA, ncol(df.melt)))
      df.melt <- rbind(df.melt, lastrow)
      df.melt$date[nrow(df.melt)] <- as.Date(paste0(endyear, '-12-31'))
      df.melt$year[nrow(df.melt)] <- endyear
      df.melt$pointid[nrow(df.melt)] <- df.melt$pointid[nrow(df.melt)-1]
      df.melt$firename[nrow(df.melt)] <- df.melt$firename[nrow(df.melt)-1]
  }
  
  # Extract the max VI value/point/year
  maxVals <- getMaxVals(df.melt)
  
  # Add DOY of max
  maxVals$doy <- yday(maxVals$date)
  
  #Write values to table
  outfileMaxVals <- paste0(baseFilename,"_", firename, "_max_vals.csv")
  write.csv(maxVals, file = file.path(path.out.max, outfileMaxVals), row.names = F)
  message(paste0("Table of max values written to ", file.path(path.out.max, outfileMaxVals)), appendLF = TRUE)
  
  rownames(df.melt) <- 1:nrow(df.melt)
  
  # Extract the min VI value per point per year
  minVals <- getMinVals(df.melt)
  #minVals <- na.omit(minVals)
  
#  mins.list <- split(mins, mins$pointid)
#  out.list <- by(mins$mean, mins$pointid, function(x) tsOutliers(x, T))
  

## --- Replace minimum outliers ----
  
  # - calculate outliers for each group
  # - interpolate values from surrounding minimum values
  # - replace value with interpolated one in original data file (df.melt)
  
  message("****************************************")
  message(paste0("-- removing outliers from ", firename))

  # ----------- function to remove outliers
  remove_outliers <- function(y) {
    id <- y$pointid[1]  # get pointid
    print(paste0("pointid = ", id))
    if (!is.na(id) & (!is.na(y$date[1]))) {  # check for NAs in date or ID--may have been a transient issue 
      y <- y$mean  # get the array of mean values for each data point
      n <- 0  # start counting number of reps
      tsOutliers(y, T)  # calculate outliers in data array and plot them
      outs <- tsOutliers(y) # array of outlier values
      rownums <- as.numeric(names(which(outs > 0)))  # get index values of outliers
      lastnum <- length(y)
      rownums <- rownums[! rownums %in% c(1, lastnum)] # get rid of index values in the 1st or last positions
                                                       # can't interpolate them
      
      while (length(rownums) > 0 & n < 20) {  # if outliers exist AND it hasn't already looped > x times, continue
        replaceIt(id, rownums) # replace with interpolated values
        outs <- tsOutliers(y) # recalculate outliers
        rownums <- as.numeric(names(which(outs > 0)))  # get index values
        rownums <- rownums[!rownums %in% c(1, lastnum)] # remove indices at edges
        tsOutliers(y, T) # calculate outliers w plot
        n <- n + 1
      }
    }
  }
  
  
  
  # ----------- function to remove outliers
  remove_outliers_mean <- function(y) {
    id <- y$pointid[1]  # get pointid
    print(paste0("pointid = ", id))
    threshold <- 0.70
    
    if (!is.na(id) & (!is.na(y$date[1]))) {  # check for NAs in date or ID--may have been a transient issue 
      y <- y$mean  # get the array of mean values for each data point
      y.original <- y
      n <- 0  # start counting number of reps
      
      while (n < 20) {  # do multiple iterations
        for (i in 2:(length(y) - 2)){      # can't look at full range b/c of need to look back, forward 
          # if value is < <threshold>% of both the preceding and subsequent values, replace with mean
          if ((y[i] < threshold * y[i-1]) & (y[i] < threshold * y[i+1])){
            if ((y[i-1] - y[i] > 0.25) & (y[i + 1] - y[i] < 0.1)) {  # if the drop is indicative of fire, don't change
              y[i] <- y[i]
            } else {
              y[i] <- (y[i-1] + y[i+1])/2 # replace with mean
            }
          }
          # took this part out because not sure it was getting the correct values
          # if value is > 150% than the preceeding and subsequent values, replace with mean 
       #   if ((y[i] > 1.5 * y[i-1]) & (y[i] > 1.5 * y[i+1])){  # more tolerant of values being abnormally high
      #      y[i] <- (y[i-1] + y[i+1])/2 
       #   }
          
        }
        n <- n + 1
      }
      
      rownums <- which(y.original != y) # get index addresses of changed values
      replaceIt2(id, rownums, y) # replace original file of minVals with interpolated values
    }
    # print(paste0('n = ', n))
  } # end function
  
  # Run functions sequentially
#  by(minVals, minVals$pointid, FUN = remove_outliers)
  by(minVals, minVals$pointid, FUN = remove_outliers_mean) # I think this one does a better job
  
  # Spread out the data to associate each point with each date
  # Note the use of "spread_" instead of "spread", which allows the evaluation of variable "vi"
  df.wide <- tidyr::spread(df.melt, "pointid", "mean") 

  #> head(df.wide)
  #         date year       pt0       pt1      pt11      pt13      pt14      pt15      pt16      pt17     
  # 1 1984-01-01 1984        NA        NA        NA        NA        NA        NA        NA        NA       
  # 2 1984-05-05 1984        NA 0.1608447        NA 0.1279297 0.1903710 0.1881833 0.4454846        NA   
  # 3 1984-05-21 1984 0.4974560 0.2064470 0.5175009 0.2168142 0.2492776 0.2459297 0.5701208 0.2215569 
  # 4 1984-06-06 1984        NA        NA        NA        NA        NA        NA        NA        NA        
  # 5 1984-06-29 1984 0.4810364 0.2661510        NA 0.2712892        NA 0.3282843 0.6022018 0.2668738 
  # 6 1984-07-08 1984 0.5229930 0.2851064 0.5669947 0.2545983 0.3606695 0.2996047 0.6322106        NA 

  # Get column names (point names) for later use
  point_names <- colnames(df.wide[3:ncol(df.wide)])
  
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

  # Bring in the original point names
  colnames(df.nas) <- c("date", point_names)  

  # Drop levels ## prob not necessary anymore
  #df$ptid <- droplevels(df$ptid)

  # Reference relative column names
  first.pt.name <- point_names[1]
  last.pt.name <- point_names[length(point_names)]

  # Reshape into long format
  df.nas.long <- df.nas %>% gather_('pointid', 'value', point_names)  # tidyr 

  #> head(df.nas.long)
  #         date pointid value
  # 1 1984-01-01  pt0    NA
  # 2 1984-01-02  pt0    NA
  # 3 1984-01-03  pt0    NA
  # 4 1984-01-04  pt0    NA
  # 5 1984-01-05  pt0    NA

  # Cut the data into multi-day chunks
  # 8-day
  list.8day <- dlply(df.nas.long, .(format(date, "%Y"), pointid), # chunk for each year, ptid
             function(x) {split(x$value, ceiling(seq_along(x$value)/8))})

  # 16-day
  list.16day <- dlply(df.nas.long, .(format(date, "%Y"), pointid), # chunk for each year, ptid
              function(x) {split(x$value, ceiling(seq_along(x$value)/16))})

  # Get the associated dates for each chunk, by year and point ID
  list.8day.dates <- dlply(df.nas.long, .(format(date, "%Y"), pointid), function(x) x$date[seq_along(x$date) %% 8 == 1])
  list.16day.dates <- dlply(df.nas.long, .(format(date, "%Y"), pointid), function(x) x$date[seq_along(x$date) %% 16 == 1])

  # > head(list.16day.dates)
  # $`1984.pt0`
  # [1] "1984-01-01" "1984-01-17" "1984-02-02" "1984-02-18" "1984-03-05" "1984-03-21" "1984-04-06" "1984-04-22" "1984-05-08" "1984-05-24" "1984-06-09"
  # [12] "1984-06-25" "1984-07-11" "1984-07-27" "1984-08-12" "1984-08-28" "1984-09-13" "1984-09-29" "1984-10-15" "1984-10-31" "1984-11-16" "1984-12-02"
  # [23] "1984-12-18"

  # $`1984.pt1`
  # [1] "1984-01-01" "1984-01-17" "1984-02-02" "1984-02-18" "1984-03-05" "1984-03-21" "1984-04-06" "1984-04-22" "1984-05-08" "1984-05-24" "1984-06-09"
  # [12] "1984-06-25" "1984-07-11" "1984-07-27" "1984-08-12" "1984-08-28" "1984-09-13" "1984-09-29" "1984-10-15" "1984-10-31" "1984-11-16" "1984-12-02"
  # [23] "1984-12-18"

  # Turn the names into dataframes, because lists frustrate me
  df.8day.dates <- reshape2::melt(list.8day.dates)
  df.16day.dates <- reshape2::melt(list.16day.dates)

  # head(df.16day.dates)
  #        value       L1
  # 1 1984-01-01 1984.pt0
  # 2 1984-01-17 1984.pt0
  # 3 1984-02-02 1984.pt0
  # 4 1984-02-18 1984.pt0
  # 5 1984-03-05 1984.pt0

  # remove the leading date (year) and rename columns
  df.8day.dates$pointid <- lapply(strsplit(df.8day.dates$L1, "\\."), tail, n=1) #this extracts the last element of the values in col L1
  df.8day.dates$pointid <- as.factor(unlist(df.8day.dates$pointid))
  df.8day.dates$L1 <- NULL
  colnames(df.8day.dates)[1] <- "date"

  df.16day.dates$pointid <- lapply(strsplit(df.16day.dates$L1, "\\."), tail, n=1)
  df.16day.dates$pointid <- as.factor(unlist(df.16day.dates$pointid))
  df.16day.dates$L1 <- NULL
  colnames(df.16day.dates)[1] <- "date"
  
  # save the date labels for future use
  # specify first level of pointid so only labels from one point are used
  # this unfortunately writes the pointid to the file as well; needs to be stripped manually.
  
  # info
  
  filename.8day.dates <- file.path(path.out.names, paste0(firename, "_dates_8day.csv"))
  filename.16day.dates <- file.path(path.out.names, paste0(firename, "_dates_16day.csv"))
  message(paste0("Date labels written to ", filename.8day.dates), "\b", appendLF = TRUE)
  message(paste0("Date labels written to ", filename.16day.dates), "\b", appendLF = TRUE)
   
  write.csv(subset(df.8day.dates, pointid == first.pt.name), file = filename.8day.dates, row.names = FALSE)
  write.csv(subset(df.16day.dates, pointid == first.pt.name), file = filename.16day.dates, row.names = FALSE)

  # Take the mean of each 8- or 16-day chunk
  num.8day.mean <- rapply(list.8day, mean, na.rm = T)
  num.16day.mean <- rapply(list.16day, mean, na.rm = T)

  # Attach the correct names, just for the heck of it
  names(num.8day.mean) <- do.call(c, list.8day.dates)
  names(num.16day.mean) <- do.call(c, list.16day.dates)

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
  df.8day.mean <- cbind(df.8day.dates, df.8day.mean)
  df.16day.mean <- cbind(df.16day.dates, df.16day.mean)

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
  df.8day.wide <- tidyr::spread(df.8day.mean, pointid, value)
  df.16day.wide <- tidyr::spread(df.16day.mean, pointid, value)

  # na.approx each column! rule = 2 extends the first/last valid value to leading/trailing NaNs
  mat.8day.approx <- sapply(df.8day.wide[2:ncol(df.8day.wide)], function(x) na.approx(x, rule = 2))
  mat.16day.approx <- sapply(df.16day.wide[2:ncol(df.16day.wide)], function(x) na.approx(x, rule = 2))

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
  # step but I'm keeping it for now.
  df.8day.approx.long <- df.8day.approx %>% tidyr::gather_('pointid', 'value', point_names) 
  df.16day.approx.long <- df.16day.approx %>% tidyr::gather_('pointid', 'value', point_names)

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
  
  # Write to files for later plotting of original points in R
  write.csv(df.8day.approx.long, 
            file = file.path(path.out.rformat, paste0(vi, "Vals_", file.names$longName,  "_", firename, "_8day_r", add_on, ".txt")),
            row.names = FALSE)
  
  write.csv(df.16day.approx.long, 
            file = file.path(path.out.rformat, paste0(vi, "Vals_", file.names$longName, "_", firename, "_16day_r", add_on, ".txt")),
            row.names = FALSE)

  message(paste0("Tables of original points written to ", file.path(path.out.rformat, 
                                                 paste0(vi, "Vals_", file.names$longName, "_", firename, "_16day_r", add_on, ".txt"))),
                                      "\b", appendLF = TRUE)
  
  # Reshape such that each row contains the data for an individual data point
  df.8day.approx.wide <- reshape(df.8day.approx.long, timevar = "date", idvar = c('pointid'), direction = "wide")
  df.16day.approx.wide <- reshape(df.16day.approx.long, timevar = "date", idvar = c('pointid'), direction = "wide")

  # > head(df.16day.approx.wide, 2)
  #       ptid value.1984-01-01 value.1984-01-17 value.1984-02-02 value.1984-02-18 value.1984-03-05 value.1984-03-21 value.1984-04-06 value.1984-04-22 value.1984-05-08
  # 1     pt0        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560        0.4974560
  # 737   pt1        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.1608447        0.2064470
  
  # This is ready for timesat! Almost. Need header elements:
  
  # number of years in the time series
  nyrs <- (endyear - startyear) + 2 #actually is +1 years of data, but pad the timesat files with duplicated last year
  
  # number of data values per year
  nptsperyear_8day <- round(365/8) 
  nptsperyear_16day <- round(365/16) 

  # of time series (i.e., points) in the file
  nts <- length(point_names)

  # 8 day
  # Write results to a table - 8day. Strip off 1st column, which has point_id info.  This can be reattributed
  # later via the point_names files.
  # Write the header - 8-day data
  outfile8day.name <- paste0(vi, "Vals_", file.names$longName, "_", firename, "_8day_timesat", add_on, ".txt")
  write.table(t(c(nyrs, nptsperyear_8day, nts)), 
             file = file.path(path.out.time, outfile8day.name), 
             row.names = FALSE, col.names = FALSE, 
             sep = " ")
  
  # Duplicate last year's worth of data to pad for timesat
  start.col <- ncol(df.8day.approx.wide) - nptsperyear_8day + 1
  end.col <- ncol(df.8day.approx.wide)
  out.table <- cbind(df.8day.approx.wide, df.8day.approx.wide[, start.col:end.col])
  
  # Append data to 8-day table
   write.table(out.table[, 2:ncol(out.table)], 
             file = file.path(path.out.time, outfile8day.name), 
             row.names = FALSE, col.names = FALSE,
             sep = " ", append = TRUE)
  
  # info
  message(paste0("Timesat-format table written to ", file.path(path.out.time, outfile8day.name)), "\b", appendLF = TRUE)


  # 16 day
  # Write results to a table - 16day.  Strip off 1st column, which has point_id info
  outfile16day.name <- paste0(vi, "Vals_", file.names$longName, "_", firename, "_16day_timesat", add_on, ".txt")
   write.table(t(c(nyrs, nptsperyear_16day, nts)), 
             file = file.path(path.out.time, outfile16day.name), 
             row.names = FALSE, col.names = FALSE, 
             sep = " ")
  
  # Duplicate last year's worth of data for Timesat padding
    start.col <- ncol(df.16day.approx.wide) - nptsperyear_16day + 1
    end.col <- ncol(df.16day.approx.wide)
    out.table <- cbind(df.16day.approx.wide, df.16day.approx.wide[, start.col:end.col])
  
  # Write output table
   write.table(out.table[, 2:ncol(out.table)], 
             file = file.path(path.out.time, outfile16day.name), 
             row.names = FALSE, col.names = FALSE,
             sep = " ", append = TRUE)

   message(paste0("Timesat-format table written to ", file.path(path.out.time, outfile16day.name)), "\b", appendLF = TRUE)
   
   
   # Finally, write point names to file 
   # Add column of generic point labels to facilitate merging with Timesat output
  # point_names <- cbind(point_names, c(paste0("pt0", seq(1, 9)), paste0("pt", seq(10, length(point_names)))))
  # colnames(point_names) <- c("pointid", "ptid")
  # write.csv(point_names, file = file.path(path.out.names, "point_names.csv"), row.names = F)
   
   # info
  # message(paste0("Point names written to ", file.path(path.out.names,  "_", firename, "_point_names.csv")), "\b", appendLF = TRUE)

   message(paste0("Done with points from ", firename))

}  # end loop through all firenames
###############################################################################
# Fini

