#  This script reads in the binary seasonal metrics files output from Timesat (*.tpa).
#  All information is ingested using readBin.
#  Elements are read into a list, then converted to a dataframe.  The Timesat time
#  reference, in which time refers to the interval # (in this case, the intervals are
#  either 8 or 16 days long) is converted to DOY.
#
#  collect_timesat_data.r
#
# JWalker 5 Nov 2016


# ---------------------------------------------
# Begin main
# ---------------------------------------------


rm(list=ls())

# Get functions
source("D:/projects/Mendenhall/R/gee_functions.R")  

# Set directories
#setwd("D:/projects/Mendenhall/timesat/output/binary/lamesa")
setwd("D:/projects/Coop/timesat/output/binary")
path.in <- getwd()
path.tables <- "D:/projects/Coop/tables" #"D:/projects/Mendenhall/tables"
path.out <- "D:/projects/Coop/timesat/output/seasonal/" #"D:/projects/Mendenhall/timesat/output/seasonal/lamesa"
year.end <- 2016 # last year of GEE data

# Load libraries
library(plyr)  # to convert list to df; join (merge) command
library(lubridate)  # for various date commands

# Set timesat column names
col.names <- c('start', 'end', 'length', 'baseVal', 'peakt', 'peakval', 'amp', 'lderiv', 'rderiv', 'lint', 'sint')

# Identify files to read
filenames.in <- list.files(path.in, pattern = "*.tpa")
                    

# ---- Loop through all Timesat seasonality files (*.tpa)

for (filename.in in filenames.in) {
  
  # Create empty list
  list.all <- list()

# Info
  message(paste0("Processing ", filename.in), "\b", appendLF = TRUE)  # appendLF puts a line return after a string
  
# Get input file 
  file.in <- file(filename.in, "rb")

# Read file head: nYears, nPtsPerYear, rowStart, rowStop, colStart, colStop
  file.head <- readBin(file.in, integer(), n = 6, endian = "little")

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
  df$ptid <- paste0("pt", pt)
  df$n <- seq(1, nrow(df))
 
# Set the column names 
  colnames(df) <- c(col.names, "ptid", "n")
 
# Read the data into the list 
  list.all[[pt]] <- df
  
  }

# Close the file
  close(file.in)

# Convert the full list of data into a df
  df.all <- ldply(list.all, data.frame)


#> head(df.all)
#       start       end    length   baseVal     peakt   peakval       amp     lderiv     rderiv     lint     sint ptid
#1   9.381408  18.90015  9.518743 0.2144372  12.73775 0.4587028 0.2442656 0.06168891 0.03028300 3.993829 1.635020  pt1
#2  31.430227  41.06033  9.630105 0.1968178  37.19929 0.7267959 0.5299781 0.07388799 0.12219465 6.268973 3.907160  pt1
#3  55.451824  64.91191  9.460087 0.2368830  60.25266 0.7629936 0.5261107 0.11026879 0.08678151 6.565078 3.959366  pt1
#4  78.245438  89.38048 11.135039 0.2210183  83.19272 0.7841707 0.5631525 0.12341350 0.06584498 7.710951 4.837713  pt1
#5 101.186226 111.06850  9.882265 0.2289208 106.34058 0.8229908 0.5940701 0.10362741 0.07957661 7.295326 4.548277  pt1
#6 123.502441 135.68042 12.177976 0.2168150 129.67311 0.7256430 0.5088280 0.06337284 0.07319126 7.572898 4.537489  pt1


# ------ Convert date references to actual dates ------------

# Check for 8 or 16 day in filename
  if (grepl("8day", basename(filename.in))) {
    time.int <- 8
  } else {
    time.int <- 16
  }

# Timesat dates refer to intervals; i.e. the span is 1: # of 8- or 16-day periods in the time series. 
# Convert the intervals to DOYs in each year.
  
  dates <- readFile(time.int, filename.in, year.end)  # startyear comes from the .tpa file name...this is crap
  df.start <- convertDates(df.all, 'start', time.int)
  df.end <- convertDates(df.all, 'end', time.int)
  df.peakt <- convertDates(df.all, 'peakt', time.int)

# Assimilate into a single dataframe
  df.all$start.doy <- df.start$start.doy
  df.all$end.doy <- df.end$end.doy
  df.all$length.days <- round(df.all$length * time.int)
  df.all$peakt.doy <- df.peakt$peakt.doy
  df.all$year <- df.peakt$year

# Write dataframe to a file
  filename.base <- substr(filename.in, 1, nchar(filename.in) - 4)
  
# Write to file
  message(paste0("Output written to: \n", file.path(path.out, paste0(filename.base, "_seasonal_output.csv"))))
  write.csv(df.all, file = file.path(path.out, paste0(filename.base, "_seasonal_output.csv")), row.names = F)
  #rm(df, df.all)
}

