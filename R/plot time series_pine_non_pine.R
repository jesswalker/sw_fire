##  
##  This script takes GEE files of NDVI values and plots them out
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

## Load libraries
library(reshape2)
library(ggplot2)
library(TTR)
library(plyr)

source("D:/projects/Mendenhall/R/gee_functions.R")
# Set path
path_in <- "D:/projects/Mendenhall/tables"

grass <- processGEEFile(path_in, "ndvi_grass_LC.csv")
pine <- processGEEFile(path_in, "ndvi_pine_lc_espAll_30pts_1984_2015.csv")
pineControl <- processGEEFile(path_in, "ndviVals_pine_controlSW_espAll_30pts_1984_2015.csv")

# compute simple moving average, just for display clarity
grass.sma <- SMA(grass$mean, n=8)
grass$sma <- grass.sma 
pine.sma <- SMA(pine$mean, n=8)
pine$sma <- pine.sma

## merge the pine and grass data
pinegrass <- merge(pine, grass, by = "date")

## fiddle

colnames(pinegrass) <- c("date", "pine_ndvi", "pine_sma", "grass_ndvi", "grass_sma")
pinegrass.sma <- pinegrass[ , c("date", "pine_sma", "grass_sma")]
pinegrass <- pinegrass[ , c("date", "pine_ndvi", "grass_ndvi")]
pinegrass.sma.long <- melt(pinegrass.sma, id.vars = c("date"))
pinegrass.long <- melt(pinegrass, id.vars = c("date"))
colnames(pinegrass.sma.long) <- c("date", "type", "ndvi")

##   plot both

ggplot(data = pinegrass.sma.long, aes(x = date, y = ndvi, by=type, col=type)) + 
geom_line(size=0.8, aes(group=type)) +
  geom_smooth(method="lm", aes(group=type), formula=y~x, se=FALSE)+
  theme_bw() +
  xlab("Date") +
  ylab("NDVI") +
  ylim(0, 0.7) +
scale_color_manual(name = "", labels = c(" Pine ", " Grass"), 
                   values = c("orange", "lightslategray")) +
  theme(legend.text = element_text(size=16)) +
  theme(legend.position = c(0.1, 0.9),
        legend.direction="horizontal") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) 


plot1 <- ggplot(outFileAll, aes(x=DOY, y=NDVI, group=Year, col=Year, shape=Year)) + 
  geom_line(size=0.3) + 
  geom_point(aes(shape=Year), size=2.2)+ 
  ylim(0.4, 0.6) + 
  theme_bw() +
  xlab("Day of Year") +
  ylab("NDVI") +
  
  theme(
    panel.border = element_rect(fill=NA, colour="black", size=0.3),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  
  theme(
    legend.key = element_blank(),
    legend.key.size=unit(.7,"cm"),
    legend.text=element_text(size=10),
    legend.background = element_rect(fill = "white", colour = "gray34", size = 0.3),
    legend.title=element_text(size=10)) +
  
  theme(
    plot.background = element_blank(),
    plot.margin = unit(c(.1, .1, .1, 0.1),"cm")) +   #top right bottom left
  
  theme(
    axis.title.x = element_text(vjust = 0.1, size = 12),
    axis.title.y = element_text(vjust = 0.35, size = 12),
    axis.line = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(size = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)) +
  
  scale_color_manual(name="Year", 
                     labels=c("2005", "2006", "2007", "2008", "2009"),
                     #values = c("gray65","gray57","gray38","gray57","black")) +
                     values = c("blue", "darkgoldenrod1", "forestgreen", "turquoise", "Chocolate4"))+
  
  
  scale_shape_manual(name="Year",
                     labels=c("2005", "2006", "2007", "2008", "2009"),
                     values = c(15, 21, 17, 5, 19))

#      guides(color=guide_legend(override.aes=list(size=1)))

fileNameOut <- "D:/Diss_3/Graphics/plot_036_mean2_color_range.pdf"

pdf(file = fileNameOut, 5, 3, pointsize = 8)

print(plot1)
dev.off()
#graphics.off()
