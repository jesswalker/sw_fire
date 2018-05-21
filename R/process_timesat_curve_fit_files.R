# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #
#  This script reads in the binary time series files output from Timesat (*fit.tts).
#  These files contain the full time series fit by the function selected by the user
#  in Timesat.
#
#  All information is ingested using readBin.
#  Elements are read into a list, then converted to a dataframe.  The Timesat time
#  reference, in which time refers to the interval # (in this case, the intervals are
#  either 8 or 16 days long) is converted to DOY.
#
#  Works, but the files of date labels are currently hardcoded in. 
# 
#  This is an alternative to manually printing out each 'sg' fitted curve from Timesat.
#
#  process_timesat_curve_fit_files.R
#
#  Input:  *has to have either '8day' or '16day' in file name
#           *fit.tts files
#
#  Output:  <input file name>_timeseries_output.csv
#
# JWalker 22 Feb 2017


# ---------------------------------------------
# Begin main
# ---------------------------------------------


rm(list=ls())

# Get functions
source("D:/projects/sw_fire/R/sw_fire_functions.R")  

# Set directories
folder <- "sw_fire" #"coop"
path.in <- file.path("D:/projects", folder, "output/timesat/binary")
setwd(path.in)
path.tables <-  file.path("D:/projects", folder, "data/tables")  
path.out <- file.path("D:/projects", folder, "output/timesat/timeseries/temp") 
year.end <- 2017 # last year of GEE data

# Load libraries
library(plyr)  # to convert list to df; join (merge) command
library(dplyr) # to convert row names to column
library(lubridate)  # for various date commands

# Identify files to read
filenames.in <- list.files(path.in, pattern = "*.tts")
                    

# ---- Loop through all Timesat time series files (*.tts)

for (filename.in in filenames.in) {
  
  #Info
  message(paste0("Processing ", filename.in), "\b", appendLF = TRUE)  # appendLF puts a line return after a string
  
  # Check for 8 or 16 day in filename
  if (grepl("8day", basename(filename.in))) {
    time.int <- 8
    ints.per.year <- 46
  } else {
    time.int <- 16
    ints.per.year <- 23
  }

# Create empty list for future data storage
  list.all <- list()

# Read in binary input file
  file.in <- file(filename.in, "rb")

# Read file head: nYears, nPtsPerYear, rowStart, rowStop, colStart, colStop
  file.head <- readBin(file.in, integer(), n = 6, endian = "little")
  
# 4th element = number of sampling points
  nPts <- file.head[4]

# 1st * 2nd element = total number of values per point
  nValsPt <- file.head[1] * file.head[2]
  
# Convert number of value/pt to start year
  nYrs <- nValsPt/ints.per.year
  year.start <- year.end - nYrs + 1  
  
# Set timesat date labels  There are the dates of acquisition that correspond to each Timesat interval.
  date.labels <- read.csv(file.path(path.tables, paste0("date_labels/datelabels_", time.int, "day_", year.start, "_", year.end, ".csv")), header = T)

# Cycle through all points
  for (pt in 1:nPts) {
  
   # Each point has its own header: row and col
     pt.header <- readBin(file.in, integer(), n = 2, endian = "little")
  
  # Read in each season of data
     mat <- readBin(file.in, double(), size = 4, n = nValsPt, endian = "little")
  
  # Transform to a df
     df <- as.data.frame(mat)
     
  # Add the point ID, since it's easier here than in the list > df conversion 
     if (pt < 10) {
       df$ptid <- paste0("pt0", pt)
     } else {
       df$ptid <- paste0("pt", pt)
     }   
    
  # Add the dates
    df <- cbind(date.labels, df)
    
# Set the column names 
    colnames(df) <- c("date", "value", "pointid")
 
# Read the data into the list 
    list.all[[pt]] <- df
  
  }

# Close the file
  close(file.in)

# Convert the full list of data into a df
  df.all <- ldply(list.all, data.frame)

#  > head(df.all)
#         date       value pointid
#  1  1/1/1984 0.546733618     pt01
#  2 1/17/1984 0.542793870     pt01
#  3  2/2/1984 0.542500019     pt01
#  4 2/18/1984 0.542500019     pt01
#  5  3/5/1984 0.542500019     pt01
#  6 3/21/1984 0.542500019     pt01


# ------ Convert date references to r-recognized dates ------------

  df.all$newdate <- strptime(as.character(df.all$date), "%m/%d/%Y")  #$&&!! excel changed the format to m/d/y
  df.all$date <- as.Date(format(df.all$newdate, "%Y-%m-%d"))
  df.all$newdate <- NULL
  df.all$period <- rep(seq(1, nValsPt), nPts) # associate each 8- or 16-day period with a sequential number

# Write dataframe to a file
  filename.base <- substr(filename.in, 1, nchar(filename.in) - 4)
  
# Write to file
  message(paste0("Output written to: \n", file.path(path.out, paste0(filename.base, "_timeseries_output.csv"))))
  message('------------------------------------------------------------------------')
  write.csv(df.all, file = file.path(path.out, paste0(filename.base, "_timeseries_output.csv")), row.names = F)

  }

