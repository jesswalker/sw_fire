# Import results of Savitzgy-Golay fitting

library(ggplot2)
library(lubridate)
library(bfast)
library(zoo) # for as.yearmon

folder <- "mendenhall"

# inputs
path.in <- file.path("D:/projects", folder, "timesat/output/sg/")
path.plot <- file.path("D:/projects", folder, "plots/")
path.orig <- file.path("D:/projects", folder, "tables/r_format")
path.tables <- file.path("D:/projects", folder, "tables")
setwd(path.in)


# Identify files to read
filenames.in <- list.files(path.in, pattern = "*.txt")

# ---- Loop through all files

for (filename.in in filenames.in) {

  # Info  
  message("****************************************")
  message(paste0("Processing ", file.path(filename.in), "\n"), "\r", appendLF = FALSE)
  
# get components
  baseFilename <- substr(filename.in, 1, nchar(filename.in) - 4)
  fire <- unlist(strsplit(baseFilename, "_"))[1]
  ptnum <- unlist(strsplit(baseFilename, "_"))[3]
  
# split the filename at "_", take the 2nd part ("8day" or "16day"), then substitute "day" with "" to 
# get the interval portion
  
  int <- as.numeric(gsub("day", "", unlist(strsplit(baseFilename, "_"))[2]))
  
  if (int == 8) {
    nper <- 46
  } else {
    nper <- 23
  }
  
  
# import files
  x <- read.table(filename.in)
  x <- t(x)
  x <- data.frame(value = x)

# slice off last nper rows of data, which are duplicated (2015 twice)
# Timesat does not extract seasonality for the 1st or last years, but it does give the fitted curve
# for them.
  x <- x$val[1:(nrow(x) - nper)]
  x <- data.frame(value = x)

# Get total number of years
  nyrs <- (nrow(x)/nper) 

# set starting year
  startyr <- (2015 - nyrs) + 1

# get date labels
  filename.labels <- paste0("datelabels_", int, "day_", startyr, "_2015.csv")
  datelabels <- read.csv(file.path(path.tables, filename.labels), header = T)

# add to data file
  x <- cbind(datelabels, x)
  x$date <- as.Date(x$date, format="%m/%d/%Y")

# make sure x has a pointid column
  x$pointid <- paste0("pt", ptnum)

# get original data
  filename.orig <- paste0("ndviVals_", fire, "_", int, "day_r_OLI_correction_grass.txt" )
  y <- read.csv(file.path(path.tables, "r_format", filename.orig), header = T)

# get correct point
  y.sub <- subset(y, pointid == paste0("pt", ptnum))

# reformat
  y.sub$pointid <- "orig"

# combine the two datasets
  xy <- rbind(x, y.sub)

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
  ggsave(file.path(path.plot, plotname), width = 14, height = 7, dpi = 600)

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
  ggsave(file.path(path.plot, plotname), width = 14, height = 7, dpi = 600)


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
  ggsave(file.path(path.plot, plotname), width = 20, height = 7, dpi = 600)

}
  