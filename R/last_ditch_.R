# plot sequence against moving average


rm(list = ls())

library(dplyr)
library(zoo)

# ----------------- function getSTats
getStats <- function(df, col) {
  
  # col <- deparse(substitute(col))
  funs <- c("n", "mean", "stats::median", "sd", "se")
  dots <- c(sprintf("sum(!is.na(%s))", col),
            sprintf("%s(%s, na.rm = TRUE)", funs[2:4], col),
            "sd/sqrt(n)")
  names(dots) <- gsub("^.*::", "", funs)
  dplyr::summarize_(df, .dots = dots)
}



# -------------------------------
# bring something in?
# ------------------------------

zoo.stats <- readRDS("D:/projects/Mendenhall/R/zoo_stats_bell.rds")
zoo.all <- readRDS("D:/projects/Mendenhall/R/zoo_all_lamesa_amp.rds")


# -----------------------------------------------------------
# get all files in a given path/with a given pattern


path.in <- "D:/projects/Mendenhall/timesat/output/seasonal"
path.plot <- "D:/projects/Mendenhall/plots"
setwd(path.in)

filenames <- list.files(path.in, pattern = "*_edit.csv$", full.names=F, ignore.case=F)

# read in filenames to a single csv file

df <- do.call(rbind, lapply(filenames, read.csv, header=T))

df <- df[ , c("baseVal", "peakval", "amp", "ptid", "n", "start.doy", "end.doy", 
            "length.days", "peakt.doy", "year", "phase", "tpostfire", "name", "t")]

zoo.all <- data.frame()


#---------------------------------
# pick your poison
colName <- "peakt.doy"
phase <- "post"
firename = "lamesa"
ptid <- "pt01"
evtClass <- "pine"
chunk <- 8
# ---------------------------------

# subset 
df.sub <- subset(df, name == firename & phase == phase & ptid == ptid & t == chunk)
df.sub <- df.sub[, c("tpostfire", colName, "ptid", "name")]

# get moving average
vals <- c(df.sub[[colName]], NA, NA)
df.ts <- zoo(vals) 
byNum <- 1
df.zoo <- rollapply(df.ts, width = 3, by = byNum, FUN = mean, align = "center", na.rm = T)
zoo.df <- data.frame(tpostfire = time(df.zoo), df.zoo, check.names = F, row.names = NULL)

#check for consistency in time
if (df.sub$tpostfire[1] > zoo.df$tpostfire[1]) {
    zoo.df$tpostfire <- zoo.df$tpostfire + (df.sub$tpostfire[1] - zoo.df$tpostfire[1] + byNum)
}
zoo.df$ptid = "moveAvg"
colnames(zoo.df)[2] <- colName
zoo.df$name <- df.sub$name

# combine moving average and non-moving average
x <- rbind(df.sub, zoo.df)


# plot out
ggplot(x, aes_string("tpostfire", colName, group = "ptid")) +
  geom_point(aes(color = ptid)) + 
  geom_line(aes(color = ptid, size = ptid)) +
  scale_size_manual(values = c(0.1, 1.1)) +
  ggtitle(paste0(toupper(firename), "\n ", colName)) +
  theme_bw()

# assemble into 1 dataframe
zoo.df$ptid <- paste0('pt', pt)
zoo.df$evt <- evtClass


# ---------------------------------
zoo.all <- rbind(zoo.all, zoo.df)
# -----------------------------------



# ---------------------------------------------
# once you've assembled the df, run this:
# -----------------------------------------------
zoo.stats <- zoo.all %>% 
  group_by(evt, tpostfire) %>% 
  getStats(colName)


# for LaMesa:  ran for pts 1, 6 (shrub), 2-5 (pine)

if (colName == "amp") {
  title <- "\n Amplitude of annual growth cycle"
  y.lab <- "Value"
  y.min <- 0.0
  y.max <- 0.4
} else {
  if (colName == "peakt.doy"){
    title <- "\n Timing of peak greenness" 
    y.lab <- "Day of year (DOY)"
    y.min <- 175
    y.max <- 300
  } else {
    if (colName == "peakval") {
      title <- "\n Peak greenness value"
      y.lab <- "NDVI"
      y.min <- 0.0
      y.max <- 0.8
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

limits <- aes(ymin = mean - se, ymax = mean + se)
dodge = position_dodge(width = 0.2)
colorList <- c(Grass = "#66c2a5", Shrub = "#fc8d62", Pine = "#8da0cb")
#colorList <- c("#66c2a5", "#fc8d62", "#8da0cb")
zoo.stats <- as.data.frame(zoo.stats)
zoo.stats$evt <- factor(zoo.stats$evt, levels=c("grass", "shrub", "pine"), labels=c("Grass", "Shrub", "Pine"))

y.min <- 190
y.max <- 340
ggplot(zoo.stats, aes(tpostfire, mean, group = evt, color = evt)) + 
  geom_line(aes(color = evt)) +
 geom_errorbar(limits, position = dodge, width = 0.25, size = 0.3) +
  theme_bw() +
  ylim(y.min, y.max) +
  ylab(y.lab) + 
  xlab("Years after fire") +
  ggtitle(paste0(toupper(firename), title)) +
  #theme(plot.title = element_text(size = 16)) +
  scale_color_manual(values = colorList) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))	+
  theme(legend.title = element_blank())


###
ggsave(paste0("D:/projects/Mendenhall/plots/", firename, "_peakt_pine_shrub.png"), width = 10, height = 7, dpi = 600)


# for posterity!
saveRDS(zoo.stats, file = paste0("D:/projects/Mendenhall/R/zoo_stats_", firename, "_amp.rds"))
saveRDS(zoo.all, file = paste0("D:/projects/Mendenhall/R/zoo_all_", firename, "_amp.rds"))



# parked

# dplyr::summarize(n = sum(!is.na(peakt.doy)),
#             mean = mean(peakt.doy, na.rm = T),
#             sd = sd(peakt.doy, na.rm = T),
#             se = sd/sqrt(n))



