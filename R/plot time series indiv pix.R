library(reshape)
library(ggplot2)


path.in <- "D:/Projects/Mendenhall/output/"
path.out <- "D:/Projects/Mendenhall/output/"

types = matrix(c("lamesa_grassland_", "Grassland", "lamesa_shrubland_", "Shrubland", "lamesa_forest_", "Forest"), 2, 3)

menuChoice = menu(types[2,], graphics= T, title = "Choose desired ecosystem ")
fname.root = types[1, menuChoice]

fname.in <- paste0(fname.root, "raw.csv")
fname.out <- paste0(fname.root, "processed.csv")
print(fname.in)
  
# Check if the cleaned file already exists
if (!file.exists(paste0(path.out, fname.out))){
    x <- read.csv(paste0(path.in, fname.in), header= T)
#x <- x[1:8,]
#x.mean <- colMeans(x, na.rm = TRUE)
#x$Name <- NULL

# if first column <> "OID_" then x.melt <- melt(x, id.vars = c("OBJECTID") 
    x[,1] <- paste0("pt", seq(1:nrow(x)))
    colnames(x)[1] <- "point"
    x.melt <- melt(x, id.vars=c("point"))
    x.melt.sort <- x.melt[order(x.melt$variable), ]
    x.melt.sort$date <- substr(x.melt.sort$variable, 1, 8)
    x.melt.sort$year <- as.numeric(substr(x.melt.sort$variable, 2, 5))
    x.melt.sort$doy <- as.numeric(substr(x.melt.sort$variable, 6, 8))
    x.melt.sort$doyfraction <- x.melt.sort$year + x.melt.sort$doy/365

    x.melt.sort.nozeros <- x.melt.sort[!(x.melt.sort$value == 0), ]
    write.csv(x.melt.sort.nozeros, 
          file = paste0(path.out, fname.out), 
          row.names = F)

} else {
  x.melt.sort.nozeros <- read.csv(paste0(path.out, fname.out), header = T)
}

ggplot(x.melt.sort.nozeros, aes(x=doyfraction, y= value,
                                group=as.factor(point),
                                color = as.factor(point))) + 
      theme_bw()+
  
 #      geom_point() +
      geom_line() +   
      xlab("Date") +
      ylab("NDVI") +
      ylim(0, 1) +
      ggtitle(types[2, menuChoice]) +
#theme(title=element.text(size=16))+
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=12),
      axis.title=element_text(size=14))






ggplot(x.melt.sort.nozeros, aes(x=doyfraction, y= value)) + 
  
  geom_point() +
  #     theme_bw() +
  xlab("Date") +
  ylab("NDVI") +
  ylim(0, 1) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))








# -----------------------
y <- read.csv("D:/Projects/Mendenhall/output/la_mesa_1970_fire_points_raw.csv", header= T)
y[1:15,1:15]
y$Name <- c(1:5)
y$OID_ <- NULL

y.melt <- melt(y, id.vars = c("Name"))
y.melt.sort <- y.melt[order(y.melt$variable),]
y.melt.sort$date <- substr(y.melt.sort$variable, 1, 8)
y.melt.sort$year <- as.numeric(substr(y.melt.sort$variable,2,5))
y.melt.sort$doy <- as.numeric(substr(y.melt.sort$variable,6,8))
y.melt.sort$doyfraction <- y.melt.sort$year + y.melt.sort$doy/365
y.melt.sort.nozeros <- y.melt.sort[!(y.melt.sort$value == 0),]

write.csv(y.melt.sort.nozeros, 
          file = "D:/Projects/Mendenhall/output/LaMesa_1970fire_processed.csv", 
          row.names = F)

y <- read.csv("D:/Projects/Mendenhall/output/LaMesa_1970fire_processed.csv", header=T)

ggplot(y, aes(x=doyfraction, y= value, 
                                group=as.factor(Name),
                                color = as.factor(Name))) + 
  geom_point() +
#  theme_bw() +
  xlab("Date") +
  ylab("NDVI") +
  ylim(0, 0.7) +
  theme(legend.position = "none")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


# -----------------------
z <- read.csv("D:/Projects/Mendenhall/output/la_mesa_1990_fire_points_raw.csv", header= T)
z[1:15,1:15]
z$Name <- c(1:5)
z$OID_ <- NULL

z.melt <- melt(z, id.vars = c("Name"))
z.melt.sort <- z.melt[order(z.melt$variable),]
z.melt.sort$date <- substr(z.melt.sort$variable, 1, 8)
z.melt.sort$year <- as.numeric(substr(z.melt.sort$variable,2,5))
z.melt.sort$doy <- as.numeric(substr(z.melt.sort$variable,6,8))
z.melt.sort$doyfraction <- z.melt.sort$year + z.melt.sort$doy/365
z.melt.sort.nozeros <- z.melt.sort[!(z.melt.sort$value == 0),]

write.csv(z.melt.sort.nozeros, 
          file = "D:/Projects/Mendenhall/output/LaMesa_1990fire_processed.csv", 
          row.names = F)

ggplot(z.melt.sort.nozeros, aes(x=doyfraction, y= value, 
                                group=as.factor(Name),
                                color = as.factor(Name))) + 
  geom_point() +
#  theme_bw() +
  xlab("Date") +
  ylab("NDVI") +
  ylim(0, 0.7) +
  theme(legend.position = "none")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

