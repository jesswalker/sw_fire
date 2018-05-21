# This script examines the result of taking an irregular data series,
# padding it to daily intervals (i.e., days between valid data = 0),
# then aggregating to 8- and 16-day intervals and applying a spline
# filling technique.
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

filename.in <- choose.files(default = path.in, caption = "Select file",
                        multi = FALSE, filters = c(".csv"))

### metric for the time series df (can be only 1)
metric <- "mean"
vi <- "ndvi"

# Parse names  
baseFilename <- basename(filename.in)
baseFilename <- substr(baseFilename, 1, nchar(baseFilename) - 4)
file.in.names <- getNames(baseFilename)

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
# rows contain the same vi value and date. That would be highly unlikely). Using only one of the 
#  vi's--ndvi, evi, etc--should be enough.
# Still doesn't account for spatial autocorrelation!
df <- df[!(duplicated(df[, c(vi, "date")]) | duplicated(df[, c(vi, "date")], fromLast = T)),]


# FOR NOW, REMOVE ALL OTHER VI's

df.sub <- df[ , c(vi, 'ptid', 'date', 'year')]

#> head(df.sub)
#           ndvi ptid       date year
#  129 0.1991603    4 1985-03-21 1985
#  131 0.1990817   11 1985-03-21 1985
#  133 0.1969757   28 1985-03-21 1985
#  134 0.1627010    0 1985-04-06 1985
#  135 0.1741605    3 1985-04-06 1985
#  136 0.1046584    5 1985-04-06 1985


# DO NOT DO THIS STEP if you want to retain info for individual points

# If one date has multiple values, take the mean
# structuring the eval/substitute command this way lets ddply take 
# a variable for the column (i.e., 'ndvi', 'evi', etc.)
#  df.melt <- ddply(df, .(date), summarize,   #.(date, ptid)
#                 mean = eval(substitute(mean(col), list(col = as.name(vi))))) #, na.rm = T)

 df.melt <- df.sub
#> head(df.melt)
#       date      mean
#1 1984-05-05 0.2350623
#2 1984-05-21 0.3123615
#3 1984-06-06 0.2246302
#4 1984-06-29 0.3247095
#5 1984-07-08 0.4087580
#6 1984-07-24 0.3440730

# Make sure the time series starts on Jan 1 and ends on Dec 31
# Adding a Jan 1 and a Dec 31 date makes sure that the daily sequence will
# encompass the entire span of dates.

if (df.melt$date[1] > paste0(startyear, '-01-01')) { 
  firstrow <- c(rep(NA, ncol(df)))
  df.melt <- rbind(firstrow, df.melt)
  df.melt[1, "date"]  <- as.Date(paste0(startyear, '-01-01'))
  df.melt[1, "year"] <- startyear
  df.melt[1, "ptid"] <- 0
}

if (df.melt$date[nrow(df.melt)] < paste0(endyear, '-12-31')) {
  lastrow <- c(rep(NA, ncol(df.melt)))
  df.melt <- rbind(df.melt, lastrow)
  df.melt[nrow(df.melt), "date"] <- as.Date(paste0(endyear, '-12-31'))
  df.melt[nrow(df.melt), "year"] <- endyear
  df.melt[nrow(df.melt), "ptid"] <- 0
}

# Make sure the values aren't strings
# df.melt$mean <- as.numeric(df.melt$mean)

# for these purposes, select an id to examine. 
# Then get rid of the ID tag altogether
df.melt.1 <- df.melt[which(df.melt$ptid == 1), ]
df.melt.1$id <- NULL

# added this in bc I keep needing something other than 1
df.melt.1 <- df.melt

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

# na.approx it
a8.mean.zoo.approx <- na.approx(a8.mean.zoo)
a16.mean.zoo.approx <- na.approx(a16.mean.zoo)

#test <- na.fill(na.approx(a8.mean), "extend")

# spline it
a8.mean.zoo.filled <- na.spline(a8.mean.zoo)
a16.mean.zoo.filled <- na.spline(a16.mean.zoo)

# merge the two files
#merged <- merge.zoo(a8.mean.zoo.filled, a16.mean.zoo.filled)


# convert the splined file back to a df
#a8.mean.df.filled <- data.frame(date = time(a8.mean.zoo.filled), 
#                   data = a8.mean.zoo.filled, 
#                   check.names = F, 
#                   row.names = NULL)

# this is b/c padding f's up the splines...
#a8.mean.df.filled <- a8.mean.df.filled[which(a8.mean.df.filled$data > -1), ]

# convert back to a zoo
#a8.mean.zoo.filled <- zoo(a8.mean.df.filled, order.by = as.Date(names(a8.mean)))


# convert splined to a df
#a16.mean.df.filled <- data.frame(date = time(a16.mean.zoo.filled), 
#                                data16 = a16.mean.zoo.filled, 
#                                check.names = F, 
#                                row.names = NULL)

# this is b/c padding f's up the splines...
#a16.mean.df.filled <- a16.mean.df.filled[which(a16.mean.df.filled$data > -1), ]

# and back to a zoo
#a16.mean.zoo.filled <- zoo(a16.mean.df.filled, order.by = as.Date(names(a16.mean)))

# convert back to a df, oh my!
a8.df <- data.frame(date = time(a8.mean.zoo), 
                   data = a8.mean.zoo, 
                   check.names = F, 
                   row.names = NULL)

# convert the approx file back to a df
a16.mean.df.approx <- data.frame(date = time(a16.mean.zoo.approx), 
                   data = a16.mean.zoo.approx, 
                   check.names = F, 
                   row.names = NULL)


# merge the zoo files
merged <- merge.zoo(a8.mean.zoo.filled, a16.mean.zoo.filled, a.zoo)

# xform into df
a8_16 <- data.frame(date = time(merged),
                     data8 = merged[, 1],
                     data16 = merged[, 2],
                     origPts = merged[,3],
                     check.names = F,
                     row.names = NULL)

# melt to long format
df.melt <- melt(a8_16, .(date))


ggplot(df.melt, aes(date, value, group = variable, color = variable)) + 
                      geom_line(data = df.melt[!is.na(df.melt$value), ]) +
                      geom_point() +
  theme_bw()
+
  ggsave("D:/")


#**************************

merged <- merge.zoo(a8.mean.zoo, test)

test2 <- data.frame(date = time(merged),
                    origPts = merged[, 1],
                    approx = merged[, 2],
                    check.names = F,
                    row.names = NULL)

# melt to long format
df.melt <- melt(test2, .(date))


ggplot(subset(df.melt, date < as.Date("1987-12-31")), 
       aes(x=date, y=value, shape=variable)) + 
  geom_point(aes(size = variable, color = variable)) +
  geom_line(aes(color = variable)) +
  scale_size_manual(values = c(6, 3)) +
  ylim(0,0.7) + 
  theme_bw()
  #geom_line(data = df.melt[!is.na(df.melt$value), ]) +

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

