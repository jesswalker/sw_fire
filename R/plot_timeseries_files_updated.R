# 
#  Here's the deal: this routine is broken for the data format that now exists.
#  It would be nice to have, but is not a deal killer right now. 
#  Much more important to get the other stats done.
#  JW 5/18/18
#  Import results of Savitzgy-Golay fitting

library(ggplot2)
library(lubridate)
library(bfast)
library(zoo) # for as.yearmon

folder <- "sw_fire"

# inputs
path.in <- file.path("D:/projects", folder, "output/timesat/timeseries")
path.plot <- file.path("D:/projects", folder, "output/plots/timeseries/test")
#path.orig <- file.path("D:/projects", folder, "tables/r_format")
path.tables <- file.path("D:/projects", folder, "data/tables/r_format")
setwd(path.in)
lastyear <- 2017

r_format_prefix <- "ndviVals_pts_1982_2017_4578_swfires_COLL1_"

# Identify FITTED series files
filenames.in <- list.files(path.in, pattern = "*.csv")

# ---- Loop through all files
filename.in <- filenames.in[1]
#for (filename.in in filenames.in) {
  

  # Info  
  message("****************************************")
  message(paste0("Processing ", file.path(filename.in), "\n"), "\r", appendLF = FALSE)
  
# get components
  baseFilename <- substr(filename.in, 1, nchar(filename.in) - 4)
  fire <- unlist(strsplit(baseFilename, "_"))[1]
  ptnum <- unlist(strsplit(baseFilename, "_"))[3] # this is obsolete bc points are now all in one file
  
# split the filename at "_", take the 2nd part ("8day" or "16day"), then substitute "day" with "" to 
# get the interval portion
  
  int <- as.numeric(gsub("day", "", unlist(strsplit(baseFilename, "_"))[2]))
  
  if (int == 8) {
    nper <- 46
  } else {
    nper <- 23
  }
  
  
# import FITTED files
  x <- read.csv(filename.in, header = T)
  x$date <- as.Date(x$date, format = "%Y-%m-%d")

# Step through each point in the file
  for (pt in levels(x$pointid)) {
       x.pt <- x[x$pointid == pt, ]
    
# slice off last nper rows of data, which are duplicated 
# Timesat does not extract seasonality for the 1st or last years, but it does give the fitted curve
# for them.

# Make sure rows are ordered according to period
      x.pt <- x.pt[order(x.pt$period), ]
  
# Remove duplicates--this get rid of doubled last-year dates
      x.pt <- x.pt[!duplicated(x.pt[c('date', 'pointid')]), ]
  
# Get number of points
      #npts <- nrow(x)/nrow(x[x$pointid == 'pt01', ])
      npts <- nrow(x.pt)

# Get number of years
      nyrs <- npts/nper
  
# set starting year
      startyr <- (lastyear - nyrs) + 1

# get original data
      filename.orig <- paste0(r_format_prefix, fire, "_", int, "day_r.txt")

      #  filename.orig <- paste0("ndviVals_", fire, "_", int, "day_r_OLI_correction.txt" )
      y <- read.csv(file.path(path.tables, filename.orig), header = T)

# reformat
      y$pointid <- "orig"

# take the period column before rbinding
  y <- cbind(y, 'period' = x$period)

# combine the two datasets
  xy <- rbind(x, y)

  
  
# ---- Do this for each point ---- VVVVV
  
  
get_plot <- function(df) {  
  
# started to set up a function to go through all points in each fire
  
# get breakpoints
  start.time <- Sys.time()
  message("Extracting breakpoints...\n", "\r", appendLF = FALSE)
  
  ts.x <- ts(x$value, start = c(year(x$date[1]), month(x$date[1])), 
                      end = c(year(x$date[nrow(x)]), month(x$date[nrow(x)])), 
                      frequency = nper)

  fit <- bfast(ts.x, h=0.13, season = "harmonic", max.iter = 5)
  
  end.time <- Sys.time()
  time.elapsed <- end.time - start.time
  print(time.elapsed)

# get the breakpoints

# > fit$output[[1]][5]$bp.Vt[1][[1]]
# [1] 124 287 409

# individual breakpoints

#> fit$output[[1]][5]$bp.Vt[1][[1]][3]
# [1] 409

# pull out trend component
# this is output$Tt 

  trend.ts <- (fit$output[[1]][1][[1]])
  trend.df <- data.frame(date = as.Date(as.yearmon(time(trend.ts))), 
                        data = as.matrix(trend.ts), 
                        check.names = F)
  trend.df$pointid <- "x"
#   plot(fit, 
#    #  ylim = c(0, ymax),
#      #     text(1985, 0.5, paste0(metric, " NDVI")),
#      type = "trend", #all",  # "trend" show trend component + breakpoints
#      ANOVA = T,
#      largest = T) # show largest break

# plot data

  ymin <- 0
  ymax <- 0.9

# blue:  #00BFC4

# Original points
  plotname <- paste0(fire, "_", int, "day_", ptnum, "_1orig_pts.png")
  
  p <- ggplot(xy, aes(date, value, color = pointid)) + 
    geom_point(data = subset(xy, pointid == "orig"), aes(group = pointid), size = 1.9) + # pts
    theme_bw() +
    ylim(ymin, ymax) + 
    ylab("NDVI") +
    xlab("Date") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.title.y = element_text(margin=margin(0,10,0,0)),
          axis.title.x = element_text(margin=margin(10,0,0,0))) +
    scale_color_manual(values = c("#F8766D", "#00BFC4"), labels = c("Original data", "SG fit")) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14))
  print(p)
#  ggsave(file.path(path.plot, plotname), width = 14, height = 7, dpi = 600)

# Original points w SG fit
  plotname <- paste0(fire, "_", int, "day_", ptnum, "_2orig_pts_w_sg_line.png")
  p <- ggplot(xy, aes(date, value, color = pointid)) + 
    geom_point(data = subset(xy, pointid == "orig"), aes(group = pointid), size = 1.9) +  #pts
    geom_line(data = subset(xy, pointid == paste0("pt", ptnum)), aes(group = pointid), size = 0.9) +  #line
    theme_bw() +
    ylim(ymin, ymax) + 
    ylab("NDVI") +
    xlab("Date") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.title.y = element_text(margin=margin(0,10,0,0)),
          axis.title.x = element_text(margin=margin(10,0,0,0))) +
    scale_color_manual(values = c("#F8766D", "#00BFC4"), labels = c("Original data", "SG fit")) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14))
  
  print(p)
#  ggsave(file.path(path.plot, plotname), width = 14, height = 7, dpi = 600)


# SG fit with trend line
  plotname <- paste0(fire, "_", int, "day_", ptnum, "_3sg_line_w_trend.png")
  p <- ggplot(xy, aes(date, value)) + 
       geom_point(data = subset(xy, pointid == "orig"), size = 1.9, aes(color = pointid)) +  #pts
       geom_line(data = subset(xy, pointid == paste0("pt", ptnum)), aes(color = pointid, group = pointid), size = 0.9) +  #sg line
       geom_line(data = trend.df, aes(date, data, color = "x"), size = 0.4) +  #trend
       theme_bw() +
       ylim(ymin, ymax) + 
       ylab("NDVI") +
       xlab("Date") + 
       theme(axis.text = element_text(size = 14),
             axis.title = element_text(size = 16),
             axis.title.y = element_text(margin=margin(0,10,0,0)),
             axis.title.x = element_text(margin=margin(10,0,0,0))) +   
       scale_color_manual(values = c( "#F8766D", "#00BFC4", "#000000"), labels = c("Original data", "SG fit", "Trend line")) +
       theme(legend.title = element_blank(),
             legend.text = element_text(size = 14))
  print(p)
#  ggsave(file.path(path.plot, plotname), width = 20, height = 7, dpi = 600)
}
#}