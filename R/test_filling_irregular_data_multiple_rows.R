# This script takes a data series output as the result of
# sampling all available Landsat images (5,7,8) in a burn area at a
# number of points (30, to be precise).  
# The irregular data series is padded to daily intervals (non-data days are NaNs),
# then aggregated to 8- and 16-day intervals.  At that point missing values are
# approximated (linearly) to result in a regular data series consisting of a value
# for each time interval.
#
#   Input files have 3 columns:
#   
#   1.  system:index:  Landsat and point ID in format "LT50330351995012XXX01_1014"
#   2.  mean:  ndvi value for the particular image
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
library(tidyr)
library(lubridate)
library(zoo)

folder = "Mendenhall"
source("D:/projects/Mendenhall/R/gee_functions.R")
path.in <- paste0("D:/projects/", folder, "/tables/*.*")
path.out <- paste0("D:/projects/", folder, "/tables")


#  Choose file to ingest
filename.in <- choose.files(default = path.in, caption = "Select file",
                        multi = FALSE, filters = c(".csv"))

# Choose a VI to extract: ndvi, evi, satvi, or msavi
vi <- "ndvi"

# Parse names  
baseFilename <- basename(filename.in)
baseFilename <- substr(baseFilename, 1, nchar(baseFilename) - 4)
file.names <- getNames(baseFilename)

# Read file > df and check for valid data columns
file.in <- readInFile(filename.in)

# Drop the '.geo' column, extract the date and ID from Landsat tag
df <- processLandsatGEEFile(file.in, baseFilename)

# Make sure the data are nicely ordered
df <- df[order(df$date), ]

# Get rid of some dates. 1985 is the first full year
df$year <- year(df$date)  # need lubridate for this
startyear <- 1984 #year(df$date[1])
endyear <- year(df$date[nrow(df)])

df <- df[which(df$year >= startyear), ]

# Make sure no one pixel is sampled twice; take out any duplicated rows (i.e., different 
# rows that contain the same vi values and dates. That would be highly unlikely for different pixels). 
# Using only one of the vi's--ndvi, evi, etc--should be enough.
# Still doesn't account for spatial autocorrelation! Yikes.
df <- df[!(duplicated(df[, c(vi, "date")]) | duplicated(df[, c(vi, "date")], fromLast = T)),]

# Remove all other VIs until someone learns how to program better.
df.sub <- df[ , c(vi, 'ptid', 'date', 'year')]

#> head(df.sub)
#           ndvi ptid       date year
#  129 0.1991603    4 1985-03-21 1985
#  131 0.1990817   11 1985-03-21 1985
#  133 0.1969757   28 1985-03-21 1985
#  134 0.1627010    0 1985-04-06 1985
#  135 0.1741605    3 1985-04-06 1985
#  136 0.1046584    5 1985-04-06 1985


# Get the max value and corresponding date for each point in each year

max.vals <- do.call(rbind, lapply(split(df.sub, list(df.sub$ptid, df.sub$year)), 
                                  function(x) x[which.max(x[[vi]]), ]))

write.csv(max.vals, file = file.path(path.out, "")


# Make sure the time series starts on Jan 1 and ends on Dec 31
# Adding a Jan 1 and a Dec 31 date ensures that the daily sequence will
# encompass the entire span of dates.  Not sure this is the best way to go.

df.melt <- df.sub  # awkward naming transition holdover from previous versions

# If the first day isn't Jan 1, add an entry consisting of all NAs
if (df.melt$date[1] > paste0(startyear, '-01-01')) { 
  firstrow <- c(rep(NA, ncol(df)))
  df.melt <- rbind(firstrow, df.melt)
  df.melt[1, "date"]  <- as.Date(paste0(startyear, '-01-01'))
  df.melt[1, "year"] <- startyear
  df.melt[1, "ptid"] <- "pt1"  # sacrificial point
}

# If the last day isn't Dec 31, add an entry
if (df.melt$date[nrow(df.melt)] < paste0(endyear, '-12-31')) {
  lastrow <- c(rep(NA, ncol(df.melt)))
  df.melt <- rbind(df.melt, lastrow)
  df.melt[nrow(df.melt), "date"] <- as.Date(paste0(endyear, '-12-31'))
  df.melt[nrow(df.melt), "year"] <- endyear
  df.melt[nrow(df.melt), "ptid"] <- "pt1"
}

# Spread out the data to associate each point with each date
# Note the use of "spread_" instead of "spread", which allows the evaluation of variable "vi"
df.wide <- tidyr::spread_(df.melt, "ptid", vi)

#> head(df.wide)
#         date year       pt0       pt1      pt11      pt13      pt14      pt15      pt16      pt17     
# 1 1984-01-01 1984        NA        NA        NA        NA        NA        NA        NA        NA       
# 2 1984-05-05 1984        NA 0.1608447        NA 0.1279297 0.1903710 0.1881833 0.4454846        NA   
# 3 1984-05-21 1984 0.4974560 0.2064470 0.5175009 0.2168142 0.2492776 0.2459297 0.5701208 0.2215569 
# 4 1984-06-06 1984        NA        NA        NA        NA        NA        NA        NA        NA        
# 5 1984-06-29 1984 0.4810364 0.2661510        NA 0.2712892        NA 0.3282843 0.6022018 0.2668738 
# 6 1984-07-08 1984 0.5229930 0.2851064 0.5669947 0.2545983 0.3606695 0.2996047 0.6322106        NA 

# Get column names for later use
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

# reshape into long format
df.nas.long <- df.nas %>% gather(ptid, value, pt0:pt9)  # tidyr 

#> head(df.nas.long)
#         date ptid value
# 1 1984-01-01  pt0    NA
# 2 1984-01-02  pt0    NA
# 3 1984-01-03  pt0    NA
# 4 1984-01-04  pt0    NA
# 5 1984-01-05  pt0    NA
# 6 1984-01-06  pt0    NA


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
# 6 1984-03-21 1984.pt0

# remove the leading date and rename columns
df.8day.names$ptid <- lapply(strsplit(df.8day.names$L1, "\\."), tail, n=1) #this extracts the last element of the values in col L1
df.8day.names$ptid <- as.factor(unlist(df.8day.names$ptid))
df.8day.names$L1 <- NULL
colnames(df.8day.names)[1] <- "date"

df.16day.names$ptid <- lapply(strsplit(df.16day.names$L1, "\\."), tail, n=1)
df.16day.names$ptid <- as.factor(unlist(df.16day.names$ptid))
df.16day.names$L1 <- NULL
colnames(df.16day.names)[1] <- "date"

# Take the mean of each 8/16-day chunk
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
# in the df without returning a godawful mess.  Kludgy and circuitous carries the day.
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
df.8day.approx.long <- df.8day.approx %>% tidyr::gather(ptid, value, pt0:pt9)  # tidyr
df.16day.approx.long <- df.16day.approx %>% tidyr::gather(ptid, value, pt0:pt9)

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


# Format those crazy long numbers.
df.8day.mean$value <- as.numeric(format(df.8day.mean$value, digits = 3))
df.16day.mean$value <- as.numeric(format(df.16day.mean$value, digits = 3))


# This is ready for timesat! Almost. Need a couple of header elements:
# #years in time series, #data values/year, #time series in the file

nyrs <- (endyear - startyear) + 1
nptsperyear_8day <- round(365/8) 
nptsperyear_16day <- round(365/16) 
nts <- length(col_names)

# Write out the results to a table - 8day
outfile8day <- paste0(vi, "Vals_", file.names$outfile, "_8day_timesat.csv")
write.table(t(c(nyrs, nptsperyear_8day, nts)), file = file.path(path.out, outfile8day), col.name = FALSE, sep= ",")
write.table(df.8day.approx.wide, file = file.path(path.out, outfile8day), row.names = FALSE, col.names = FALSE,
            sep = ",", append = TRUE)

print(paste0("Table written to ", file.path(path.out, outfile8day)))


# Write out the results to a table - 16day
outfile16day <- paste0(vi, "Vals_", file.names$outfile, "_16day_timesat.csv")
write.table(t(c(nyrs, nptsperyear_16day, nts)), file = file.path(path.out, outfile16day), col.name = FALSE, sep= ",")
write.table(df.16day.approx.wide, file = file.path(path.out, outfile16day), row.names = FALSE, col.names = FALSE,
            sep = ",", append = TRUE)

print(paste0("Table written to ", file.path(path.out, outfile16day)))













ggplot(df.melt, aes(date, value, color = variable)) + geom_point()

save(df.melt, file = "D:/projects/Mendenhall/R/irregularData.rda")

# writing out the data to a timesat-friendly format
header <- t(c(1,1,1)) # test out the leading bits
write.table(header, 
            file = "D:/projects/Mendenhall/tables/testTimesatx.txt", 
            col.names = FALSE, row.names = FALSE)

write.table(t(df.melt.interp$value), 
            file = "D:/projects/Mendenhall/tables/testTimesatx.txt", 
            col.names = FALSE, row.names = FALSE, append = TRUE)

# this is automatically a single row of data NOPE!
gg <- as.numeric(formatC(df.melt.interp$value, digits = 4, format = "f"))

write.table(t(gg), 
            file = "D:/projects/Mendenhall/tables/testTimesat3.txt", 
            col.names = FALSE, row.names = FALSE)

ggg <- as.numeric(formatC(a8.mean, digits = 4, format = "f"))

