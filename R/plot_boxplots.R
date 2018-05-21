rm(list=ls())

# Import  
library(ggplot2)
library(lubridate)
library(bfast)
library(zoo) # for as.yearmon
library(dplyr) # much better than plyr
library(lazyeval) # need for dplyr 

# ------- function getStats 
# summarized stats for pre, post fire
getStats <- function(df, col) {
 # col <- deparse(substitute(col))
  funs <- c("n", "mean", "stats::median", "sd", "se")
  dots <- c(sprintf("sum(!is.na(%s))", col),
            sprintf("%s(%s, na.rm = TRUE)", funs[2:4], col),
            "sd/sqrt(n)")
  names(dots) <- gsub("^.*::", "", funs)
  summarize_(df, .dots = dots)
}

# ----------- function plotBox
plotBox <- function(df, fire_name, var_x, var_y, y_min, y_max, title, y_lab) {  
  ggplot(df, aes_string(var_x, var_y)) + 
    geom_boxplot(notch = F, aes_string(color = var_x, fill = var_x)) + 
    theme_minimal() + 
    ylim(y_min, y_max) +
    ylab(y_lab) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
          axis.line.x = element_blank(), axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14)) +
    ggtitle(paste0(fire_name, title)) + 
    theme(plot.title = element_text(hjust = 0.5)) +  #center title
    theme(legend.position = "top", 
          legend.title=element_blank(), 
          panel.grid.major.x=element_blank(),
          legend.text = element_text(size = 12)) +
    scale_color_manual(values = c('pre' = "#F8766D", "post" = "#00BFC4"),
                       breaks = c("pre", "post"),
                       labels = c("Pre-fire", "Post-fire")) +
    scale_fill_manual(values = c('pre' = "#F8766D", "post" = "#00BFC4"),
                      breaks = c("pre", "post"),
                      labels = c("Pre-fire", "Post-fire")) +
    stat_summary(geom = "crossbar", 
                 width = 0.65, 
                 fatten = 0, 
                 color = "white", 
                 fun.data = function(x) {return(c(y=median(x), ymin = median(x), ymax = median(x)))}) 
}


# -----------
# inputs
# ----------


path.in <- "D:/projects/Mendenhall/timesat/output/seasonal"
path.plot <- "D:/projects/Mendenhall/plots/newer"
setwd(path.in)


fireNames <- c("bell", "blackhawk", "bonner", "control", "dude", "lamesa", "lasconchas", "pot", "potaz", "rattlesnake",
               "rincon", "rodeo", "slim", "south")
fireYears <- c("1986", "1985", "1985", "1984", "1986", "1984", "1984", "1986", "1984", "1984", "1984", "1986", "1986", "1985")

# oh dear lord, a loop
for (i in 1:length(fireNames)){
  fireName = fireNames[i]
  year <- fireYears[i]
  print(paste0(" ----------------  ", fireName, "_", year))
  
  x <- read.csv(paste0(tolower(fireName), "_16day_TS_", year,"_seasonal_output_edit.csv"), header = T)
  
  x <- x[ , c("baseVal", "peakval", "amp", "ptid", "n", "start.doy", "end.doy", 
            "length.days", "peakt.doy", "year", "phase", "tpostfire", "name")]
  x.sub <- x
#  if (fireName == "bell"){
#      x.sub <- subset(x, ptid == "pt1" | ptid == "pt5" | ptid == "pt6" | ptid == "pt7" | ptid == "pt8")
#  } 
  x.sub$ptid <- droplevels(x.sub$ptid)

# =========================
# Set desired area and metric

  colNames <- c("amp", "peakt.doy", "baseVal", "peakval", "start.doy", "length.days")

  for (colName in colNames) {
    print(paste0("processing ", colName))
  
  
  if (colName == "amp") {
    title <- "\n Amplitude of annual growth cycle"
    y.lab <- ""
    y.min <- 0.00
    y.max <- 0.4
  } else {
    if (colName == "peakt.doy"){
      title <- "\n Timing of peak greenness" 
      y.lab <- "Day of year (DOY)"
      y.min <- 100
      y.max <- 330
    } else {
      if (colName == "peakval") {
        title <- "\n Peak greenness value"
        y.lab <- "NDVI"
        y.min <- 0.35
        y.max <- 0.9
      } else {
        if (colName == "length.days") {
          title <- "\n Length of growing season"
          y.lab <- "Days"
          y.min <- 150
          y.max <- 330
        } else {
          if (colName == "start.doy") {
            title <- "\n Start of growing season"
            y.lab <- "Day of year (DOY)"
            y.min <- 75
            y.max <- 200
          } else {
            if (colName == "baseVal") {
              title <- "\n Base value of annual growing cycle"
              y.lab <- "NDVI"
              y.min <- 0.1
              y.max <- 0.9
            }
          }
        }
      }
    }
  }


# get stats
  x.stats <- x.sub %>% group_by(year, phase, tpostfire) %>% getStats(colName)
  x.stats <- as.data.frame(x.stats)

# clean up the results
  x.stats$na.rm <- NULL
  x.stats <- subset(x.stats, phase != "fire")
  x.stats$phase <- droplevels(x.stats$phase)
  x.stats$phase <- factor(x.stats$phase, levels = c("pre", "post"))

# ----- function for max DOY or NDVI boxplot ----------

  plotname <- paste0(fireName, "_", colName, "_boxplot.png")
  plotname5 <- paste0(fireName, "_", colName, "_boxplot_lt11.png")
  plotname15 <- paste0(fireName, "_", colName, "_boxplot_gt10lte15.png")
  plotname25 <- paste0(fireName, "_", colName, "_boxplot_gt15lte26.png")
  plotname35 <- paste0(fireName, "_", colName, "_boxplot_gt25.png")
  
  plotBox(x.stats, toupper(fireName), "phase", "mean", y.min, y.max, title, y.lab)
  
  # save the plot
  ggsave(file.path(path.plot, plotname), width = 7, height = 7, dpi = 600)
  
  # it was worth a try
#   title_new <- paste0(title, "\n < 10 years post fire")
#   plotBox(subset(x.stats, phase == "pre" | tpostfire < 10), toupper(fireName), "phase", "mean", y.min, y.max, title_new, y.lab) 
#   ggsave(file.path(path.plot, plotname5), width = 10, height = 7, dpi = 600)
#   
#   title_new <- paste0(title, "\n 10 - 15 years post fire")
#   plotBox(subset(x.stats, phase == 'pre' | (tpostfire > 10 & tpostfire < 16)), toupper(fireName), "phase", "mean", y.min, y.max, title, y.lab)
#   ggsave(file.path(path.plot, plotname15), width = 10, height = 7, dpi = 600)
#   
#   title_new <- paste0(title, "\n 15 - 25 years post fire")
#   plotBox(subset(x.stats, phase == 'pre' | (tpostfire > 15 & tpostfire < 26)), toupper(fireName), "phase", "mean", y.min, y.max, title, y.lab)
#   ggsave(file.path(path.plot, plotname25), width = 10, height = 7, dpi = 600)
#   
#   title_new <- paste0(title, "\n > 25 years post fire")
#   plotBox(subset(x.stats, phase == 'pre' | (tpostfire > 26), toupper(fireName), "phase", "mean", y.min, y.max, title, y.lab)
#   ggsave(file.path(path.plot, plotname35), width = 10, height = 7, dpi = 600)
 }
}






