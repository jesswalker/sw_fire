# process_timesat_seasonality_files.R

#
# 'on hold' until I finalize the noise removal section
#
#  This script reads in all binary seasonal metrics files output from Timesat (*.tpa)
#  contained in a given directory. Each file is opened and the individual elements 
#  are read into a list, which is ultimately converted to a dataframe. 
#  If the file contains data from multiple points, the lists are consolidated into
#  a single dataframe. Each distinct set of data is distinguished by a sequential ID in the 
#  final file.
#
#  ******************************************
#  For proper date attribution, files should have either "8day" or "16day" in the filename. Lack
#  of date info will cause values to be interpreted as occurring at 8-day intervals.
#  ******************************************
#
#  Timesat files have 11 temporal metrics:
#  'start', 'end', 'length', 'baseVal', 'peakt', 'peakval', 'amp', 'lderiv', 'rderiv', 'lint', 'sint'
#  
#  The temporal information is reported in intervals (i.e., 1-23 annual intervals for 16-day data).  Converting 
#  to DOY yields: 'start.doy', 'end.doy', 'length.days', 'peakt.doy', 'year'
#
#  Input: All *.tpa files in the given directory
#  Output: Input filename + "_seasonal_output.csv"
#
#
#  process_timesat_seasonality_files.r
#
# JWalker 5 Nov 2016


# ---------------------------------------------
# Begin main
# ---------------------------------------------

rm(list=ls())

# Get functions
source("D:/projects/sw_fire/R/sw_fire_functions.R")  

# Set directories
path.in <- "D:/projects/sw_fire/output/timesat/binary/grass"
path.tables <-  "D:/projects/sw_fire/data/tables"  
path.out <-  "D:/projects/sw_fire/output/timesat/seasonal" 
year.end <- 2017 # last year of GEE data


# Load libraries
library(plyr)  # to convert list to df; join (merge) command
library(lubridate)  # various date commands

# Set timesat column names
col.names <- c('start', 'end', 'length', 'baseVal', 'peakt', 'peakval', 'amp', 'lderiv', 'rderiv', 'lint', 'sint')

# Identify files to read
filenames.in <- list.files(path.in, pattern = "*.tpa")
                    
setwd(path.in)

# ---- Loop through all Timesat seasonality files (*.tpa)

for (filename.in in filenames.in) {
  
  # Create empty list for data
  list.all <- list()

# Info
  message("-------------------------------------")
  message(paste0("Processing ", filename.in), "\b", appendLF = TRUE)  # appendLF puts a line return after a string
  
# Get input file 
  file.in <- file(filename.in, "rb")

# Read file head: nYears, nPtsPerYear, rowStart, rowStop, colStart, colStop
  file.head <- readBin(file.in, integer(), n = 6, endian = "little")

# 1st element = # years
  nYears <- file.head[1]
  
# 4th element = number of sampling points
  pts <- file.head[4]

# Cycle through all points
  for (pt in 1:pts) {
  
   # Each point has its own header: row, col, and n, where n = # of seasons
     pt.header <- readBin(file.in, integer(), n = 3, endian = "little")
     seasons <- pt.header[3]
  
   # Create a matrix: (11 temporal Timesat values) x (#seasons)
     mat <- matrix(nrow = seasons, ncol = 11)
  
   # Read in each season of data
     for (season in 1:seasons) {
       mat[season, ] <- readBin(file.in, double(), size = 4, n = 11, endian = "little") # size = # bytes, n = #records
     }
  
# Transform to a df
     df <- as.data.frame(mat)
 
# Add the point ID, since it's easier here than in the list > df conversion 
    if (pt < 10) {
      df$ptid <- paste0("pt0", pt)
    } else {
      df$ptid <- paste0("pt", pt)
    }
    df$n <- seq(1, nrow(df))
 
# Set the column names 
    colnames(df) <- c(col.names, "ptid", "n")
 
# Read the data into the list 
    list.all[[pt]] <- df
  
  }

  close(file.in)

# Convert the full list of data into a df
  df.all <- ldply(list.all, data.frame)

#> head(df.all)
#       start       end    length   baseVal     peakt   peakval       amp     lderiv     rderiv     lint     sint ptid
#1   9.381408  18.90015  9.518743 0.2144372  12.73775 0.4587028 0.2442656 0.06168891 0.03028300 3.993829 1.635020  pt01
#2  31.430227  41.06033  9.630105 0.1968178  37.19929 0.7267959 0.5299781 0.07388799 0.12219465 6.268973 3.907160  pt01
#3  55.451824  64.91191  9.460087 0.2368830  60.25266 0.7629936 0.5261107 0.11026879 0.08678151 6.565078 3.959366  pt01
#4  78.245438  89.38048 11.135039 0.2210183  83.19272 0.7841707 0.5631525 0.12341350 0.06584498 7.710951 4.837713  pt01
#5 101.186226 111.06850  9.882265 0.2289208 106.34058 0.8229908 0.5940701 0.10362741 0.07957661 7.295326 4.548277  pt01
#6 123.502441 135.68042 12.177976 0.2168150 129.67311 0.7256430 0.5088280 0.06337284 0.07319126 7.572898 4.537489  pt01


# ------ Convert date references to actual dates ------------

# Check for 8 or 16 day in filename
  if (grepl("8day", basename(filename.in))) {
    time.int <- 8
    print("time interval is 8")
  } else {
    time.int <- 16
    print('time interval is 16')
  }

  year.start <- year.end - nYears + 2 # the extra year takes off the first year
  
# Timesat dates refer to intervals; i.e. the span is 1:number of 8- or 16-day periods in the time series. 
# Convert the intervals to DOYs in each year.  Each file contains duplicated end years.
  
# Set timesat column names.  There are the dates of acquisition that correspond to each Timesat interval.
  date_labels <- read.csv(file.path(path.tables, 'date_labels', paste0("datelabels_", time.int, "day_", year.start, "_", year.end, ".csv")), header = T)
  message(file.path(path.tables, 'date_labels', paste0("datelabels_", time.int, "day_", year.start, "_", year.end, ".csv")))

# Convert the date column to R-recognized dates
  dates <- readDateLabels(date_labels, time.int)  
  df.start <- convertDates(df.all, 'start', time.int, dates)
  df.end <- convertDates(df.all, 'end', time.int, dates)
  df.peakt <- convertDates(df.all, 'peakt', time.int, dates)

# Assimilate into a single dataframe
  df.all$start.doy <- df.start$start.doy
  df.all$end.doy <- df.end$end.doy
  df.all$length.days <- round(df.all$length * time.int)
  df.all$peakt.doy <- df.peakt$peakt.doy
  df.all$year <- df.peakt$year

# Convert all zero entries to NA
  df.all[df.all == 0] <- NA

# Write dataframe to a file. Filename = input filename + "_seasonal_output.csv"
  filename.base <- substr(filename.in, 1, nchar(filename.in) - 4)
  
# Write to file
  message(paste0("Output written to: \n", file.path(path.out, paste0(filename.base, "_seasonal_output.csv"))))
  write.csv(df.all, file = file.path(path.out, paste0(filename.base, "_seasonal_output.csv")), row.names = F)

}

