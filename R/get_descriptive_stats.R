########################################################################### #
#
# get_descriptive_stats.R
#
# Objective:
# This script generates a set of descriptive stats
# (n, mean, median, sd, se) for different metrics 
# baseval, peakval, amp, peakt.doy) per fire.
#
# Input:  Edited output of Timesat seasonal phenology metrics 
#         (i.e., "control_16day_TS_seasonal_output_edit.csv")
#
#         Working directory is typically 
#         D:/projects/sw_fire/output/timesat/seasonal
# 
#
# Output: "descriptive_stats.R"
#
#         in 
#         D:/projects/sw_fire/data
#  
# August 2017 JWalker 
#
#
########################################################################### #


# 17 Aug 2017 JWalker


# Remove all
rm(list=ls())

# Load libraries
library(plyr)  # to convert list to df; join (merge) command
library(dplyr)
library(greenbrown)  # trend/breakpoint analysis
library(zoo) # moving window 
library(reshape2) # melt
library(lubridate) # year

# Get functions
source("D:/projects/sw_fire/R/sw_fire_functions.R")  

# Set directories
setwd("D:/projects/sw_fire/output/timesat/seasonal")
path.in <- "D:/projects/sw_fire/output/timesat/seasonal"
path.tables <-  "D:/projects/sw_fire/output/tables" 
path.plot <- "D:/projects/sw_fire/output/plots/apr2018"
path.r <- "D:/projects/sw_fire/data"  
rdata <- "descriptive_stats.RData"


# ---------- function: get descriptive stats for each fire, for each metric ----------

stats_by_group <- function(df) {
  df.out <- data.frame()  # setup holding df
  for (metric in c("baseval", "peakval", "amp", 'peakt.doy')) {
    stats.temp <- getStats(df, metric)
    stats.temp$metric <- metric
    df.out <- bind_rows(df.out, stats.temp)
  }
  df.out$metric <- as.factor(df.out$metric)
  return(df.out)
}


# -------------- function create time series
create_ts <- function(df, value) {
  ts.x <- ts(df[[value]], start = c(year(x$date[1])), 
             end = c(year(x$date[nrow(x)])), 
             frequency = 1)
}

# -------------- function get breakpoints ----
stats_by_phase <- function(df){ #}, group.var) {
  df.out <- data.frame()  # set up holding df
  for (metric in c("baseval", "peakval", "amp", 'peakt.doy')) {
    stats.temp <- df %>% group_by_("name", 'ptid') %>% get_breaks('mean')
    stats.temp$metric <- metric
    df.out <- bind_rows(df.out, stats.temp)
  }
  return(df.out)
}


# Can't figure out an efficient way to run the moving average on distinct subgroups 
# Do it manually for now


# ---------- function get moving average ----------
get_mvavg <- function(df) {
  
  # get moving average
  l.mvg <- by(df$mean, df$regrowth, (function(x) rollmean(x, 3, na.pad = T, align = 'center')))
  
  df.mvg <- ldply(l.mvg, rbind)
  colnames(df.mvg)[1] <- "regrowth"
  
  # melt back to long
  df.long <- melt(df.mvg, .(regrowth), variable.name = 'years_post', value.name = 'mean')
  
  # convert time to a number rather than a factor
  df.long$years_post <- as.numeric(levels(df.long$years_post))[df.long$years_post]
  return(df.long)
}


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

# removed Rodeo (start: 1986, fire.year: 2002)

# combine all fire data into one large happy file
files <- list.files(path.in, pattern = "*_edit.csv$", full.names = FALSE, ignore.case = FALSE)
x <- do.call(rbind, lapply(files, read.csv, header=T))

# standardize column names
colnames(x) <- tolower(colnames(x))

# get rid of extraneous columns
x <- x[c("baseval", "peakval", "amp", "ptid", "n", "peakt.doy", "year", "phase", "years_post", "fire_name", "t")]

# > head(x)
#     baseval   peakval        amp ptid n peakt.doy year phase years_post name  t
# 1 0.6477563 0.7146601 0.06690384 pt01 1       276 1985   pre         0 bell 16
# 2 0.5372279 0.7125424 0.17531456 pt01 2       218 1986   pre         0 bell 16
# 3 0.5054810 0.7429433 0.23746228 pt01 3       198 1987   pre         0 bell 16
# 4 0.5803843 0.7164091 0.13602488 pt01 4       226 1988   pre         0 bell 16
# 5 0.5605682 0.6806482 0.12008002 pt01 5       281 1989   pre         0 bell 16
# 6 0.5206859 0.7154045 0.19471860 pt01 6       247 1990   pre         0 bell 16

# QA/QC take out weird data
# Backtracked on this b/c hard to define "weird"
#x$peakt.doy[x$peakt.doy < 45 | x$peakt.doy > 320] <- NA
x <- na.omit(x)

# make sure t (time period: 8 or 16) is a factor
x$t <- as.factor(x$t)

# drop the fire phase
x <- subset(x, phase != "fire")
x$phase <- droplevels(x$phase)
x$phase <- factor(x$phase, levels = c("pre", "post"))

# set up the regrowth type
x$regrowth <- "Forest"

# Bell
x$regrowth[x$name == 'bell' & !(x$ptid %in% c('pt02', 'pt03'))] <- "Herbaceous" #Grass/shrub"

# Blackhawk
x$regrowth[x$name == 'blackhawk'] <- "Shrub"

# Dude
x$regrowth[x$name == 'dude'] <- "Herbaceous"
x$regrowth[x$name == 'dude' & (x$ptid %in% c('pt05', 'pt06'))] <- "Shrub" #"Grass/shrub"

# La Mesa
x$regrowth[x$name == 'lamesa' & (x$ptid %in% c('pt01', 'pt06'))] <- "Shrub"
x$regrowth[x$name == 'lamesa' & (x$ptid == 'pt07')] <- "Herbaceous"

# Las Conchas
x$regrowth[x$name == 'lasconchas' & (x$ptid == 'pt01')] <- "Herbaceous" #"Grass/shrub"
x$regrowth[x$name == 'lasconchas' & (x$ptid == 'pt02')] <- "Herbaceous"

# Pot (NM)
x$regrowth[x$name == 'pot'] <- "Deciduous"
x$regrowth[x$name == 'pot' & (x$ptid == 'pt01')] <- "Herbaceous"

# Pot (AZ)
x$regrowth[x$name == 'potaz'] <- "Herbaceous"

# Rattlesnake
x$regrowth[x$name == 'rattlesnake' & (x$ptid %in% c('pt02', 'pt03'))] <- "Herbaceous"  #Shrub
x$regrowth[x$name == 'rattlesnake' & (x$ptid == 'pt01')] <- "Herbaceous"

# Rincon
x$regrowth[x$name == 'rincon'] <- "Herbaceous"

# Slim
x$regrowth[x$name == 'slim'] <- "Shrub"

# South
x$regrowth[x$name == 'south'] <- "Shrub"

# Stone
x$regrowth[x$name == 'stone'] <- "Shrub"

# fire characteristics
# took out shelly, bonner

#fireNames <- c("bell", 'blackhawk', 'control', 'dude', 'lamesa', 'lasconchas', 'pot', 'potaz', 'rattlesnake', 'rincon',
#                'slim', 'south')
fireLabels <- c("Bell", "Blackhawk", "Control", "Dude", "La Mesa", "Las Conchas", "Pot (NM)", "Pot (AZ)", "Rattlesnake",
                "Rincon", "Slim", "South")

get_fire_year <- function(y) {
  year_temp <- min(y[which(y$years_post == 1), ]$year)
  if (!is.infinite(year_temp)) {
    return(year_temp)
  } else {
    return(NA)
  }
}
#dataStart <- c(1985, 1985, 1984, 1987, 1984, 1984, 1986, 1984, 1984, 1984, 1986, 1985)
#fireYears <- c(1993, 1993, 2017, 1990, 1977, 2011, 1994, 1996, 1994, 1994, 1987, 1995)
#fire.desc <- data.frame(year = fireYears, name = fireNames, start = dataStart, labels = fireLabels)  

# Get data start year
temp.start <- ddply(x, .(fire_name), summarize, 
                    start_year = min(year))

# Get fire year
# Look for the first "post-fire" year, subtract 1
temp.fire <- ddply(x, .(fire_name), function(y)  {
  suppressWarnings(
    temp <- min(y[which(y$years_post == 1), ]$year))
  if (!is.infinite(temp)) {
    data.frame(fire_year = temp - 1)
  } else {
    data.frame(fire_year = NA)
  }
})
fire.desc <- merge(temp.start, temp.fire, by = "fire_name")
fire.desc$labels <- fireLabels



# ---------------------------------------------------------------- #
# stats by phase (pre/post) and name (points are aggregated) ----
# ---------------------------------------------------------------- #

x.stats.phase.8.all <- x[x$t==8, ] %>% group_by(fire_name, phase) %>% stats_by_group()
x.stats.phase.16.all <- x[x$t==16, ] %>% group_by(fire_name, phase) %>% stats_by_group()

#> head(x.stats.phase.16.all)
#       name phase   n      mean    median         sd          se  metric
#1      bell   pre  85 0.5122012 0.5183115 0.06108287 0.006625368 baseval
#2      bell  post 140 0.2137434 0.2053897 0.04674217 0.003950435 baseval
#3 blackhawk   pre   6 0.5989001 0.5815791 0.04199062 0.017142599 baseval
#4 blackhawk  post  22 0.2091737 0.2093612 0.03936628 0.008392919 baseval
#5   control   pre  52 0.4563193 0.4588355 0.05484602 0.007605775 baseval
#6      dude   pre  17 0.5748819 0.5712164 0.06791413 0.016471596 baseval

write.csv(x.stats.phase.8.all, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_fire_all_8.csv"), row.names = F)
write.csv(x.stats.phase.16.all, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_fire_all_16.csv"), row.names = F)

# -------------------------------------------------------------- #
# --- stats by phase (pre/post), name, and point ----
# -------------------------------------------------------------- #

x.stats.phase.8.pt <- x[x$t==8, ] %>% group_by(fire_name, phase, ptid) %>% stats_by_group()
x.stats.phase.16.pt <- x[x$t==16, ] %>% group_by(fire_name, phase, ptid) %>% stats_by_group()

#> head(x.stats.phase.16.pt)
#  name phase ptid  n      mean    median         sd          se  metric
#1 bell   pre pt01  7 0.5556748 0.5376199 0.04747527 0.017943965 baseval
#2 bell   pre pt02 17 0.5179602 0.5249631 0.07389809 0.017922919 baseval
#3 bell   pre pt03 19 0.4997694 0.5235786 0.07836931 0.017979153 baseval
#4 bell   pre pt04 16 0.4778225 0.4895456 0.04517894 0.011294736 baseval
#5 bell   pre pt05  7 0.5409580 0.5385751 0.03469521 0.013113557 baseval
#6 bell   pre pt06  6 0.5343155 0.5401456 0.02425080 0.009900348 baseval

write.csv(x.stats.phase.8.pt, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_fire_pt_8.csv"), row.names = F)
write.csv(x.stats.phase.16.pt, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_fire_pt_16.csv"), row.names = F)

# ---------------------------------------------------------------- #
# stats by time since fire and name (points are aggregated) ----
# ---------------------------------------------------------------- #

x.stats.years_post.8.all <- x[x$t==8, ] %>% group_by(fire_name, years_post) %>% stats_by_group()
x.stats.years_post.16.all <- x[x$t==16, ] %>% group_by(fire_name, years_post) %>% stats_by_group()

#> head(x.stats.tpost.16.all)
#  name years_post  n      mean    median         sd          se  metric
#1 bell         0 85 0.5122012 0.5183115 0.06108287 0.006625368 baseval
#2 bell         1  8 0.1792775 0.1792386 0.02266586 0.008013590 baseval
#3 bell         2  8 0.1879514 0.1864579 0.02557598 0.009042474 baseval
#4 bell         3  8 0.1912053 0.1935639 0.01607038 0.005681739 baseval
#5 bell         4  8 0.2041848 0.2045284 0.02478045 0.008761214 baseval
#6 bell         5  8 0.2004619 0.2027343 0.02581317 0.009126333 baseval

write.csv(x.stats.years_post.8.all, file = file.path(path.tables, "descriptive_stats", "stats_by_fire_years_post_all_8.csv"), row.names = F)
write.csv(x.stats.years_post.16.all, file = file.path(path.tables, "descriptive_stats", "stats_by_fire_years_post_all_16.csv"), row.names = F)

# -------------------------------------------------------------- #
# stats by time since fire, name, and point ----
# -------------------------------------------------------------- #

x.stats.years_post.8.pt <- x[x$t==8 & x$years_post != 0, ] %>% group_by(fire_name, years_post, ptid) %>% stats_by_group()
x.stats.years_post.8.pt$t <- 8

x.stats.years_post.16.pt <- x[x$t==16 & x$years_post != 0, ] %>% group_by(fire_name, years_post, ptid) %>% stats_by_group()
x.stats.years_post.16.pt$t <- 16

#> head(x.stats.tpost.16.pt)
#  name years_post ptid n      mean    median  sd  se  metric
#1 bell         1 pt01 1 0.2044205 0.2044205 NaN NaN baseval
#2 bell         1 pt02 1 0.1825926 0.1825926 NaN NaN baseval
#3 bell         1 pt03 1 0.1758847 0.1758847 NaN NaN baseval
#4 bell         1 pt04 1 0.1854673 0.1854673 NaN NaN baseval
#5 bell         1 pt05 1 0.1371447 0.1371447 NaN NaN baseval
#6 bell         1 pt06 1 0.2098297 0.2098297 NaN NaN baseval

write.csv(x.stats.years_post.8.pt, file = file.path(path.tables, "descriptive_stats", "stats_by_fire_years_post_pt_8.csv"), row.names = F)
write.csv(x.stats.years_post.16.pt, file = file.path(path.tables, "descriptive_stats", "stats_by_fire_years_post_pt_16.csv"), row.names = F)

# -------------------------------------------------------------- #
# stats by pre/post and regrowth type (points aggregated) ----
# -------------------------------------------------------------- #

x.stats.phase.reg.8.all <- x[x$t==8, ] %>% group_by(regrowth, phase) %>% stats_by_group()
x.stats.phase.reg.16.all <- x[x$t==16, ] %>% group_by(regrowth, phase) %>% stats_by_group()

#> head(x.stats.phase.16.reg)
#   regrowth phase   n      mean    median         sd          se  metric
#1 Deciduous   pre  31 0.4838723 0.4929430 0.06293468 0.011303403 baseval
#2 Deciduous  post  84 0.2092328 0.2140476 0.03568828 0.003893910 baseval
#3    Forest   pre 121 0.4678394 0.4720970 0.06384715 0.005804287 baseval
#4    Forest  post 165 0.2970766 0.2694065 0.10848467 0.008445520 baseval
#5     Grass   pre  48 0.5089609 0.5243272 0.06213518 0.008968441 baseval
#6     Grass  post  90 0.1766146 0.1747460 0.02510187 0.002645969 baseval

write.csv(x.stats.phase.reg.8.all, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_regrowth_all_8.csv"), row.names = F)
write.csv(x.stats.phase.reg.16.all, file = file.path(path.tables, "descriptive_stats", "stats_by_phase_regrowth_all_16.csv"), row.names = F)

# ------------------------------------------------------------------- #
# stats by time since fire and regrowth type (points aggregated) ----
# ------------------------------------------------------------------- #

x.stats.years_post.reg.8.all <- x[x$t==8 & x$years_post != 0, ] %>% group_by(regrowth, years_post) %>% stats_by_group()
x.stats.years_post.reg.16.all <- x[x$t==16 & x$years_post != 0, ] %>% group_by(regrowth, years_post) %>% stats_by_group()

#> head(x.stats.tpost.reg.16.all)
#   regrowth years_post n      mean    median         sd         se  metric
#1 Deciduous         1 4 0.3079408 0.2614214 0.14748212 0.07374106 baseval
#2 Deciduous         2 4 0.3005765 0.2375595 0.15544918 0.07772459 baseval
#3 Deciduous         3 4 0.2804252 0.2190004 0.14016511 0.07008255 baseval
#4 Deciduous         4 4 0.2291705 0.2064023 0.06646931 0.03323466 baseval
#5 Deciduous         5 3 0.2021335 0.1868690 0.04410893 0.02546630 baseval
#6 Deciduous         6 4 0.1678633 0.1356292 0.07244381 0.03622190 baseval

write.csv(x.stats.years_post.reg.8.all, file = file.path(path.tables, "descriptive_stats", "stats_by_years_post_regrowth_all_8.csv"), row.names = F)
write.csv(x.stats.years_post.reg.16.all, file = file.path(path.tables, "descriptive_stats", "stats_by_years_post_regrowth_all_16.csv"), row.names = F)


# get moving averages ----
t_doy_8 <- get_mvavg(x.stats.years_post.reg.8.all[x.stats.years_post.reg.8.all$metric == 'peakt.doy', ])
t_peak_8 <- get_mvavg(x.stats.years_post.reg.8.all[x.stats.years_post.reg.8.all$metric == 'peakval', ])
t_amp_8 <- get_mvavg(x.stats.years_post.reg.8.all[x.stats.years_post.reg.8.all$metric == 'amp', ])
t_base_8 <- get_mvavg(x.stats.years_post.reg.8.all[x.stats.years_post.reg.8.all$metric == 'baseval', ])

t_doy_16 <- get_mvavg(x.stats.years_post.reg.16.all[x.stats.years_post.reg.16.all$metric == 'peakt.doy', ])
t_peak_16 <- get_mvavg(x.stats.years_post.reg.16.all[x.stats.years_post.reg.16.all$metric == 'peakval', ])
t_amp_16 <- get_mvavg(x.stats.years_post.reg.16.all[x.stats.years_post.reg.16.all$metric == 'amp', ])
t_base_16 <- get_mvavg(x.stats.years_post.reg.16.all[x.stats.years_post.reg.16.all$metric == 'baseval', ])







# Save data and environment settings  
print(paste0("R data file saved to ", file.path(path.r, rdata)))
save.image(file = file.path(path.r, rdata))
