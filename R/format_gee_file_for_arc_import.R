filename <- "mxd14a1_ak_GEE_2002.csv"
folder = "D:/projects/ak_fire/data"

x <- read.csv(file.path(folder, filename), header = T)

# rename first column for convenience
 colnames(x)[1] <- "index"
 
# R slaps an "X" in front of numerical columns; switch it to "class"
names.sub <- names(x)[which((substring(names(x), 1,1) == "X"))]
names(x)[which((substring(names(x), 1,1) == "X"))] <- paste0("class", substring(names.sub, 2))

# drop .geo column
if (".geo" %in% names(x)) {
  x$.geo <- NULL
}

# drop 1st row
x <- x[-1, ]

# save file
write.csv(x, file = file.path(folder, filename), row.names = FALSE)

