##  
##  This script takes GEE files of NDVI values obtained from
##  Landsat 5 time series and plots them out
#  
#   Input files have 3 columns:
#   
#   1.  system:index:  Landsat and point ID in format "LT50330351995012XXX01_1014"
#   2.  mean:  ndvi value for the particular image
#   
#   Libraries called:
#   - reshape2
#   - ggplot2
#   - TTR
#   - plyr



#
#  ===========  START MAIN PROGRAM =================================
#

rm(list=ls())

## Load libraries
library(reshape2)
library(ggplot2)
library(TTR)
library(plyr)
library(lubridate)
library(bfast)
library(zoo)

folder = "fire"
folder = "Mendenhall"
source("D:/projects/Mendenhall/R/gee_functions.R")
path.in <- paste0("D:/projects/", folder, "/tables/*.*")
path.out <- paste0("D:/projects/", folder, "/plots")


#  Choose file

file.in <- choose.files(default = path.in, caption = "Select file",
                        multi = FALSE, filters = c(".csv"))

### metric for the time series df (can be only 1)
metric <- "mean"

# Parse names  
baseFilename <- basename(file.in)
baseFilename <- substr(baseFilename, 1, nchar(baseFilename) - 4)
file.in.names <- getNames(baseFilename)


# clean and summarize
file.in <- readInFile(file.in)
df <- processGEEFile(file.in)

# get rid of some dates
df <- df[which(df$date > "1985-01-01"), ]

# Make sure the data are nicely ordered
df <- df[order(df$date), ]

# if one date has multiple values, take the mean
df.melt <- ddply(df, .(date, id), summarize,
                 mean = mean(ndvi, na.rm = T))

# for these purposes, select an id to examine. 
# Then get rid of the ID tag altogether
df.melt.1 <- df.melt[which(df.melt$id == 1), ]
df.melt.1$id <- NULL

# Convert the df to a zoo object
df.melt.1.zoo <- zoo(df.melt.1[, 2:ncol(df.melt.1)], order.by = df.melt.1$date)

# Create a sequence of daily values
seq_daily <- zoo(, seq(start(df.melt.1.zoo), end(df.melt.1.zoo), by = 1))

# Pad the original data with the daily dates
df.zoo.nas <- merge(df.melt.1.zoo, seq_daily, all = T)

# rename just to keep things confusing
a.zoo <- df.zoo.nas

# Convert the zoo object BACK to a df
a.df <- data.frame(date = time(a.zoo), 
                   data = a.zoo, 
                   check.names = F, 
                   row.names = NULL)

# Cut the data into chunks
# 8-day
a.8 <- dlply(a.df, .(format(date, "%Y")), 
             function(x) {split(x$data, ceiling(seq_along(x$data)/8))})

# 16-day
a.16 <- dlply(a.df, .(format(date, "%Y")), 
             function(x) {split(x$data, ceiling(seq_along(x$data)/16))})

# Take the mean of each chunk
a8.mean <- rapply(a.8, mean, na.rm = T)
a16.mean <- rapply(a.16, mean, na.rm = T)

# Get the associated names (dates) for each chunk
a8.names <- dlply(a.df, .(format(date, "%Y")), function(x) x$date[seq_along(x$date) %% 8 == 1])
a16.names <- dlply(a.df, .(format(date, "%Y")), function(x) x$date[seq_along(x$date) %% 16 == 1])

# attach those names to the mean file
names(a8.mean) <- do.call(c, a8.names)
names(a16.mean) <- do.call(c, a16.names)

# convert the mean df to a mean zoo
a8.mean.zoo <- zoo(a8.mean, order.by = as.Date(names(a8.mean)))
a16.mean.zoo <- zoo(a16.mean, order.by = as.Date(names(a16.mean)))

# spline it
a8.mean.zoo.filled <- na.spline(a8.mean.zoo)
a16.mean.zoo.filled <- na.spline(a16.mean.zoo)

# merge the two files
merged <- merge.zoo(a8.mean.zoo.filled, a16.mean.zoo.filled)

# convert the splined file back to a df
a8.mean.df.filled <- data.frame(date = time(a8.mean.zoo.filled), 
                   data = a8.mean.zoo.filled, 
                   check.names = F, 
                   row.names = NULL)

a16.mean.df.filled <- data.frame(date = time(a16.mean.zoo.filled), 
                                data16 = a16.mean.zoo.filled, 
                                check.names = F, 
                                row.names = NULL)

# merge the 2 zoo files
merged <- merge.zoo(a8.mean.zoo.filled, a16.mean.zoo.filled, a8.mean)

# xform into df
a8_16 <- data.frame(date = time(merged),
                     data8 = merged[, 1],
                     data16 = merged[, 2],
                     data8pts = merged[,3],
                     check.names = F,
                     row.names = NULL)
colnames(a8_16) <- c("date", "8fill", "16fill", "nofill")

# melt to long format
df.melt <- melt(a8_16, .(date))


ggplot(df.melt, aes(date, value, group = variable, color = variable)) + 
                      geom_line(data = df.melt[!is.na(df.melt$value), ]) +
                      geom_point()



