#corm(list=ls())

setwd("D:/projects/Mendenhall/tables/climate")
temp.mean <- read.csv("weather_station_los_alamos_monthly_mean_temp.csv", header = T)
precip.mean <- read.csv("weather_station_los_alamos_monthly_precip.csv", header = T)
nlcd.means <- read.csv("weather_station_los_alamos_ncdc_monthly_means_1981_2010.csv", header = T)

# housekeeping
colnames(precip.mean) <- tolower(colnames(precip.mean))
colnames(temp.mean) <- tolower(colnames(temp.mean))


precip.mean$jfm <- precip.mean$jan + precip.mean$feb + precip.mean$mar
precip.mean$fma <- precip.mean$feb + precip.mean$mar + precip.mean$apr
precip.mean$mam <- precip.mean$mar + precip.mean$apr + precip.mean$may
precip.mean$amj <- precip.mean$apr + precip.mean$may + precip.mean$jun
precip.mean$mjj <- precip.mean$may + precip.mean$jun + precip.mean$jul
precip.mean$jja <- precip.mean$jun + precip.mean$jul + precip.mean$aug
precip.mean$jas <- precip.mean$jul + precip.mean$aug + precip.mean$sep
precip.mean$aso <- precip.mean$aug + precip.mean$sep + precip.mean$oct
precip.mean$ond <- precip.mean$oct + precip.mean$nov + precip.mean$dec

precip.sub <- subset(precip.mean, year > 1983 & year < 2016)

# using dataframes of values produced in last_ditch.R

cor.test(precip.sub$jfm, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$fma, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$mam, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$amj, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$mjj, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$jja, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$jas, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$aso, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$son, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$ond, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$feb, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$mar, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$apr, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$may, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$jun, df.sub$peakt.doy, method = "spearman")
cor.test(precip.sub$jul, df.sub$peakt.doy, method = "spearman")


# plot it

avg <- colMeans(subset(precip.sub, !is.na(mam)))['mam']


#ggplot(data=subset(precip.sub, !is.na(mam))
ggplot(precip.sub, aes(year, mam)) +
  geom_line() +
  geom_point() + 
  theme_bw() +
  ylab("cm") + 
  xlab("Year") +
  ggtitle("Los Alamos Station Total Precipitation \n March, April, May") +
  geom_hline(yintercept = avg, color = "red") + 
  geom_text(x = 2005, y = 5, label = "37-year mean", color = "red", size = 5) +
  theme(plot.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))	

ggsave("D:/projects/Mendenhall/plots/los_alamos_precip_mam.png", width = 10, height = 7, dpi = 600)
