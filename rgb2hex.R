table <- read.table('CLC2018_CLC2018_V2018_20_QGIS.txt', sep = ',')
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
test <- rgb2hex(table$V2, table$V3, table$V4)

library(stringr)
test2 <- str_remove(test, '#')

cat(paste(shQuote(test2, type="cmd"), collapse=", "))
