
########################################################################### #
#
# plot_descriptive_stats.R
#
# Objective:
# This script uses output from get_descriptive_stats.R to generate
# plots for each metric: amp, baseval, peakdoy, peakval

# Input:
# descriptive_data.RData - file of R data
#
# Output:
# Plots 
#  
# August 2017 JWalker 
#
# thanks: https://rcompanion.org/rcompanion/d_08.html
########################################################################### #


# Clear!
rm(list=ls())

## Load libraries
library(ggplot2)

# Load data saved from 'get_descriptive_stats.R'
folder <- "sw_fire"
path.in <- file.path("D:/projects/", folder, "data")
rdata <- "descriptive_stats.RData"
load(file = file.path(path.in, rdata))

# Set directories

path.plot <- file.path("D:/projects", folder, "output/plots/apr2018b") 

# Set consistent plot parameters
pd = position_dodge(0.16)
theme_set(theme_bw())
plot_opts <- theme(plot.margin=unit(c(6, 8, 6, 6), "points"),  #top, right, bottom, left
                    axis.text = element_text(size = 12), 
                    axis.title = element_text(size = 14),
                    axis.title.y = element_blank(),
                    legend.position = "top", 
                    legend.title = element_blank(), 
                    legend.text = element_text(size = 12), 
                    panel.grid.major.x = element_blank())

  

# ---------- plot split: pre, postfire --------------------- 

# Note: phase, mean are hardcoded in 
plot_by_fire <- function(df, metric, y_label) {
              p <- ggplot(df[df$metric == metric, ], aes(name, mean, color = phase)) + 
                      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.2, size = 0.7, position = pd) +
                      geom_point(size = 3, position = pd, aes(shape = phase)) +
                      theme_bw() + 
                      ylab(y_label) +
                coord_flip() +
                      xlab("Fire") +
                      scale_x_discrete(labels = rev(fire.desc$labels)) + # reverse labels for reversed factors
                      scale_shape_manual(values = c(15, 16), labels = c("Prefire  ", "Postfire")) +
                      scale_color_manual(values = c("gray", "black"), labels = c("Prefire  ", "Postfire")) +
                      plot_opts 
              print(p)
}


# ---------- plot split: time since fire ---------------------

# Note: phase, mean are hardcoded in 
plot_by_tpostfire <- function(df, metric, y_label) {
                   p <- ggplot(df[df$metric == metric, ], aes(name, mean, color = tpostfire)) + 
                        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                        width = 0.2, size = 0.7, position = pd) +
                        geom_point(size = 3, position = pd, aes(shape = phase)) +
                        theme_bw() + 
                        ylab(y_label) +
                        xlab("Fire") + 
                        coord_flip() +  # x to y, y to x
                        scale_x_discrete(labels = rev(fire.desc$labels)) + # reverse labels for reversed factors
                        scale_shape_manual(values = c(15, 16), labels = c("Prefire  ", "Postfire")) +
                        scale_color_manual(values = c("gray", "black"), labels = c("Prefire  ", "Postfire")) +
                        plot_opts 
                   print(p)
}


# ----------- plot: regrowth -----------------------

plot_regrowth <- function(df) {
  
 # df <- x.stats.tpost.16.reg
  
               p <- ggplot(df[df$metric == 'peakt.doy', ], aes(regrowth, mean, color = tpostfire)) + 
                    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = 0.2, size = 0.7, position = pd) +
                    geom_point(size=3, position = pd) +
                    theme_bw() + 
                    ylab("Peak DOY") +
                    xlab("Regrowth type") + 
                    coord_flip() +  # x to y, y to x
                    scale_x_discrete(labels = rev(fire.desc$labels)) + # reverse labels for reversed factors
                    scale_shape_manual(values = c(15, 16), labels = c("Prefire  ", "Postfire")) +
                    scale_color_manual(values = c("gray", "black"), labels = c("Prefire  ", "Postfire")) +
                    plot_opts 
                print(p)
}




# ---------------------------------------------------------------------------
# ---- plot all metrics by fire, split by pre/post phases (bar plots) ----
# ---------------------------------------------------------------------------

plotPhase <- function(df, time, category, allorpt) {
  
  # reverse name factor levels so labels are in alphabetical order vertically
  df$name <- factor(df$fire_name, levels = c("south", "slim", "rodeo", "rincon", "rattlesnake", "potaz", "pot",
                                         "lasconchas", "lamesa", "dude", "control", "blackhawk", "bell"))
for (metric in c("baseval", "peakval", "amp", "peakt.doy")) {
    y_label <- switch(metric,
                    "baseval" = "Base NDVI",
                    "peakt.doy" = "Day of Year",
                    "amp" = "NDVI",
                    "peakval" = "NDVI")
    plot_by_fire(na.omit(df), metric, y_label)
    plotname <- paste0("all_fires_", metric, "_", time, "_", category, "_", allorpt, ".png")
    ggsave(file.path(path.plot, plotname), width = 10, height = 6, dpi = 600)
  }
} 
# 

# plot all metrics by fire, split by pre/post phase
plotPhase(x.stats.phase.8.all, 8, "prepost", "all")
plotPhase(x.stats.phase.16.all, 16, "prepost", 'all')

plotPhase(x.stats.phase.8.pt, 8, "prepost", "pt")
plotPhase(x.stats.phase.16.pt, 16, "prepost", 'pt')


# --------------------------------------------------------------------------
# ---- plot consolidated metrics by time since fire (facet plots) ----
# --------------------------------------------------------------------------

# -----------------------  plot regrowth as facet plot
# t.doy, t.amp, t.peak, t.base
plot_tpostfire <- function(df, time, type, ytitle) {
  plotname <- paste0("facet_", time, "_", type, ".png") 
  p <- ggplot(df, aes(years_post, mean)) + 
    geom_line() + 
    facet_grid(regrowth ~ .) +
    theme_bw() +
    ylab(ytitle) +
    xlab("Time since fire (years)")
  p
  ggsave(file.path(path.plot, plotname), width = 10, height = 6, dpi = 2000)
}

# produce plots of MOVING AVERAGES
plot_tpostfire(t_amp_8, 8, "amp", "NDVI")
plot_tpostfire(t_base_8, 8, "base", "NDVI")
plot_tpostfire(t_peak_8, 8, "peak", "NDVI")
plot_tpostfire(t_doy_8, 8, "doy", "Day of year")


plot_tpostfire(t_amp_16, 16, "amp", "NDVI")
plot_tpostfire(t_base_16, 16, "base", "NDVI")
plot_tpostfire(t_peak_16, 16, "peak", "NDVI")
plot_tpostfire(t_doy_16, 16, "doy", "Day of year")





# ------------------------------------------- #
#   Plot breakpoints ----
# ------------------------------------------- #


# *** Set up distinctions by fire, point, and metric

# ------------------- function for breakpoints
get_breakpts <- function(df) {
  df.out <- data.frame()  # set up holding df
  
  # get the starting year for the plot. If it's an area for which there 
  # was no fire, take the year for which data collection started.
  if (df$fire_name == "control" | df$fire_name == "lamesa") {
    offset <- fire.desc$start_year[which(tolower(fire.desc$fire_name) == df$fire_name[1])]
  } else {
    offset <- fire.desc$fire_year[which(tolower(fire.desc$fire_name) == df$fire_name[1])]
  }
  # create date column from year 
  df$date <- df$years_post + offset
  df$date <- as.Date(paste0(df$date, '-01-01'))
  
  # create time series
  ts.sub <- ts(df[['mean']], start = c(year(df$date[1])), 
               end = c(year(df$date[nrow(df)])), frequency = 1)
  
  # 3-year moving average worked nicely for getting rid of noise
  ts.sub.mva <- zoo::rollmean(ts.sub, 3, align = "center")
  
  # convert ts to df for plotting
  df.mva <- data.frame(date = as.Date(as.yearmon(time(ts.sub.mva))), 
                       data = as.matrix(ts.sub.mva), 
                       check.names = F)
  df.mva$years_post <- as.numeric(year(df.mva$date)) - offset                    
  
  # get the breakpoints
  ts.trend <- Trend(ts.sub.mva, mosum.pval = 1)
  
  # individual breakpoints
  brkpts <- ts.trend$bp$breakpoints
  
  # convert to df b/c plotting date in ggplot is a pain
  df.trend <- data.frame(date = as.Date(as.yearmon(time(ts.trend$trend))), 
                         data = as.matrix(ts.trend$trend), 
                         check.names = F)
  
  # get the # of postfire years
  df.trend$years_post <- as.numeric(year(df.trend$date)) - offset
  
  # plot
  plot.name <- paste0("plot_", df$fire_name[1], "_", df$ptid[1], "_", df$metric[1], "_", df$t[1], "day.png")
  plot.title <- paste0(df$fire_name[1], "_", df$ptid[1], "_", df$metric[1], "_", df$t[1], "day")
  
  p <- ggplot(df, aes(years_post, mean)) + 
    geom_point() +
    geom_line(color = "black") + 
    geom_line(data = df.trend, aes(years_post, data), size = 0.8, color = "red") +
    geom_line(data = df.mva, aes(years_post, data), color = "blue") +
    geom_vline(xintercept = brkpts[1] + 1, linetype = "dashed", size = 0.7) +
    geom_vline(xintercept = brkpts[2] + 1, linetype = "dashed", size = 0.7) +
    theme_bw() +
    ggtitle(plot.title) 
  p
  
  print(paste0('Breakpoints plot saved to ', file.path(path.plot, plot.name)))
  ggsave(file.path(path.plot, plot.name), width = 10, height = 6, dpi = 600)
}    

# get breakpoints - all
#by(x.stats.years_post.8.all, x.stats.years_post.8.all[, c('fire_name', 'metric')], function(x) get_breakpts(x))
#by(x.stats.years_post.16.all, x.stats.years_post.16.all[, c('fire_name', 'metric')], function(x) get_breakpts(x))

# get breakpoints - by individual points
by(x.stats.years_post.8.pt, x.stats.years_post.8.pt[, c('fire_name', 'metric', 'ptid')], function(x) get_breakpts(x))
by(x.stats.years_post.16.pt, x.stats.years_post.16.pt[, c('fire_name', 'metric', 'ptid')], function(x) get_breakpts(x))



# ----------------------------------------
#  plots below don't work as intended!
# ----------------------------------------
# 
# 
# # --------- plot breakpoints
# 
# p <- ggplot(df, aes(years_post, mean)) + 
#       geom_point() +
#       geom_line(color = "black") + 
#       geom_line(data = df.trend, aes(years_post, data), size = 0.8, color = "red") +
#       geom_line(data = df.mva, aes(years_post, data), color = "blue") +
#       geom_vline(xintercept = brkpts[1] + 1, linetype = "dashed", size = 0.7) +
#       geom_vline(xintercept = brkpts[2] + 1, linetype = "dashed", size = 0.7) +
#       theme_bw() +
#       ggtitle(paste0(df$name[1], df$ptid[1], df$metric[1], sep = "_")) 
# p
# print(paste0('Saved to ', file.path(path.plot, plot.name)))
# ggsave(file.path(path.plot, plot.name), width = 10, height = 6, dpi = 600)
# 
# 
# 
# 
# # ------------- plot original data with BFAST line overlay ------------- #
# plotname <- "bfast_trend.png"
# p <- ggplot(x.test, aes(years_post, mean)) + geom_line(aes(color = "black")) +
#   geom_line(data = trend.df, aes(years_post, data, color = "red"), size = 0.8) +  # trend line 
#   geom_vline(xintercept = brkpts[1], linetype = "dashed", size = 0.7) +
#   geom_vline(xintercept = brkpts[2], linetype = "dashed", size = 0.7) +
#   geom_vline(xintercept = brkpts[3], linetype = "dashed", size = 0.7) +
#   theme_bw() +
#   ylab("Area (ha)") +
#   xlab("Year") + 
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         axis.title.y = element_text(margin=margin(0,10,0,0)),
#         axis.title.x = element_text(margin=margin(10,0,0,0))) +   
#   scale_color_manual(values = c( "#000000", "red"), labels = c("Original data", "Trend line")) +
#   theme(legend.title = element_blank(),
#         legend.text = element_text(size = 14))
#   p
# ggsave(file.path(path.plot, plotname), width = 20, height = 7, dpi = 1200)
# 
# 



